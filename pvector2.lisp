;;a rudimentary port of clojure's persisent vectors to common lisp.
;;hopefully, this is the start of something grand....

;;Notes-> Clojure's pvector is actually pretty complicated, and doesn't
;;have a lot of documentation in the source.
;;The naive trie-based pvector implementation is straightforward...
;;you basically build a trie using nodes that contain arrays, dimensioned 
;;by the branching factor of the trie, and figure out how to traverse the
;;trie, given an index (usually in base10).  The naive implementation takes 
;;the base10 integer and converts it into a list of digits in baseX, where 
;;x is the base of the trie, which then outlines a "path" to get from 
;;the root of the trie to the base.  Traversal is accomplished by following 
;;the "path", selecting the nth element of each node's array, where n is 
;;the head of the path (a digit in baseX). 

;;Clojure's implementation makes a couple of really cool tweaks.  
;;Specifically, instead of using lists, the path is abstracted as 
;;a 32 bit integer.  The integer is divided into N regions of 5 bits, 
;;where each 5-bit element corresponds  to a hash for a level in the trie.
;;Given any base10 integer N, we derive the "path" to the appropriate node 
;;in the trie using bit-shifting operations and masking. 
;;Since we have 32 nodes in each trie, represented as a 32-element array of 
;;objects, we can use the 5-bit integer to select an element.  
;;As the index grows > 32, we have to start adding more nodes to the trie.
;;As we add more nodes, we encode the depth of the tree in some state 
;;associated with the node, namely, a "shift" field, or a depth integer.
;;New nodes then inherit this shift value, until the point where the trie has 
;;to increase its depth again.  Then the trie returned by adding a node that 
;;overflows the root, is actually a new root node, with with old root as its first 
;;child, and a new shift value or depth value that encodes the height of the trie.

;;One unique feature of clojure's implementation is the maintenance of a special 
;;tail node at all times.  Essentially, there is a "disconnected" tail node in 
;;every trie.  For operations that require adding elements to the end of the logical 
;;vector represented by the trie, the tail node is referenced immediately (in O(1))
;;time.  When the tail node overflows, it's simply inserted as a child node in the main trie.
;;A fresh, detached tail node is created, and a new trie is returned with the newtail.

;;There are couple of fundamental operations, which mainly involve handling insertion...
;;Note -> default insertion is to act like a queue;  In other words, elements are added to 
;the tail, FIFO, rather than the stack-like nature of list consing. 

;;In the base insertion case, we add an element to the trie, returning a new trie that shares 
;;structure with the old...
;;Since we have access to the tail, we copy the tail's node, and add the element to it, returning 
;; a new trie whose rootnode is the same as the old, but whose tail is the copied (updated) tail.

;;What if adding the element to the tail causes it to overflow? 
;;We create a new trie, which subsumes the "old" trie.
;;  The new trie has the root node as its 0th child, and the tail as its first child. 
;;  In this case, we actually have to push a path of nodes, where the tail is the last, 
;;  which is equal in height to the path of the root.
;;  Clojure has a pushTail method for this, where you basically just create a chain of 
;;  empty root nodes, n levels deep, with the tail as the last root node. 


(defpackage :clojure.pvector
  (:use :common-lisp)
  (:export :persistent-vector
	   :empty-vec?
	   :get-node
	   :cons-vec
	   :cons-many
	   :assoc-vec
	   :vec-chunks
	   :vec-to-list
	   :vec-to-array))
(in-package :clojure.pvector) 

;Original from Stack Overflow, with some slight modifications.
(defun |bracket-reader| (stream char)
  "A reader macro that allows us to define persistent vectors
   inline, just like Clojure."
  (declare (ignore char))
  `(persistent-vector ,@(read-delimited-list #\] stream t)))
(set-macro-character #\[ #'|bracket-reader|)
(set-syntax-from-char #\] #\))


;utility functions

;Persistent vectors require a lot of array copying, and 
;according to the clojure implementation, bit-twiddling.

;porting from Spiewak's excellent blog post, 
;which is a port from Clojure's implementation. 
(defconstant +branches+ 32) ;use a 32-way trie....
;a bytespec is like a window..
;it's a user-defined set of continugous bits in an integer
;use (byte width position) to define the window...
(defconstant +bit-width+ 5)
(defconstant +mask+ (byte +bit-width+ 0)) ;denotes [00000] with "weights" [2^4 2^3 2^2 2^1 2^0]

(defun >>> (i n)
  "Shift integer i by n bits to the right."  
  (ash i (* -1 n)))

(defun <<< (i n)
  "Shift integer i by n bits to the left."  
  (ash i n))

(defun last-five-bits (n)
  "Helper to mask everything but the 5 least-significant bits."
  (mask-field +mask+ n))

(define-condition index-out-of-bounds (error) 
  ((text :initarg :text :reader text)))

(defun copy-vector (array n &key
		    (element-type (array-element-type array))
		    (fill-pointer (and (array-has-fill-pointer-p array)
				       (fill-pointer array)))
		    (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
   adjustability (if any) as the original, unless overridden by the keyword
   arguments.  "
  (let* ((dimensions (incf (first (array-dimensions array)) n))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

;Persistent vector definition: 

;;note -> clojure's implementation uses an object array...
;;so that the distinction between nodes and data is blurred.
;;a node is just a thread-safe wrapper around an object array.
;;in CL, this is just an array without an initial type arg. 
(defun make-data (&key (branches +branches+) (element-type t) (initial-element nil)) 
  "Standard constructor for a node in our hash trie."
  (if (and (null initial-element) 
	   (not (eq element-type t)))
      (make-array branches  :element-type element-type)
      (make-array branches  :element-type element-type :initial-element initial-element)))

(defun make-node () (make-data))

;;Note -> the analysis immediately following is only 
;;partially correct, in fact, the persistent vector does not
;;allow conjoining of arbitrary indices, and will throw an 
;;out of bounds exception if it can't address the index, or if it 
;;can't cons the item onto the tail.

;stores information for the data contained in a node, 
;a 32-element array, and the branches (another 32 element
;array of child tries), and the count of stuff in the trie. 
;Note that counter serves a couple of purposes: 
;It tells us how many items are in the trie, but it also tells 
;us the upper bound on addressable items.  In Clojure, it's 
;entirely possible to conjoin an item into a vector at an 
;arbitrary index....That means that the result of the conj will
;be a vector that has grown to include the element at the appropriate
;index.  Even though there are only "three elements" in the vector, 
;if the third element is conjoined at an index non-contiguous with 
;the first 2, say at index 1000, where the other are @ 1,2, then 
;the vector will necessarily contain ALL the intermediate substructure
;to contain up to 1000 items, to accomodate the indexing and provide 
;algorithmic guarantees.  Basically, the data structure is chunky, rather
;than sparse, in that it WILL allocate 32 additional arrays of 32 elements 
;to provide enough space to index up to 1000 (32^2 = 1024), in which our 
;3rd item @ index 1000 would be located in the 31st child of the root.

;Note that the count of all items in the trie includes the items in the 
;"floating" tail node....
;As we conjoin items, we may need to grow the trie. 
;Growth happens when the tail is full...

(defstruct pvec (root nil)
	        (tail nil)		
		(shift 5)
		(counter 0))
  
(defun ->pvec (root tail shift counter)
  "Simple persistent vector builder.  Used to derive from other pvectors 
   to share structure where possible."
  (make-pvec :root root :tail tail :shift shift :counter counter))

(defconstant +empty-pvec+ (make-pvec))
(defun empty-vec () +empty-pvec+)
(defun empty-vec? (v) (eq v +empty-pvec+))

(defun tail-end (n &optional (b +branches+))
  "Given a count of items, n, where is the tail located in an integer 
   hash?  Note, this assumes a 5 bit encoding for levels in an 32-way 
   trie.  I might generalize this later..." 
  (if (< n b)
      0
      (<<< (>>> (1- n) +bit-width+) +bit-width+)))

(defun tail-off (v)
  "Defines the integer index at which the tail starts."
  (tail-end (pvec-counter v) +branches+))

(defun count-tail (v) (length (pvec-tail v)))

(defun find-node (rootnode shift idx)
  "Given a rootnode with child nodes, a bit-shift amount, and an index, 
   traverses the rootnode's children for the node defined by idx."
  (if (<= shift 0)
      rootnode ;found our guy
      (find-node (aref rootnode (last-five-bits (>>> idx shift))) 
		 (- shift +bit-width+) idx)))

(defun copy-path (root shift0 idx &optional (leaf-function #'identity))
  "Copies the nodes from root to idx, returning a new root.  If a leaf function 
   is provided, it will be applied to the final node.  If the path does not exist, 
   intermediate structures WILL be created."
  (labels ((walk (rootnode shift)    
	     (if (zerop shift)
		 (funcall leaf-function rootnode)
		 (let ((childidx (last-five-bits (>>> idx shift)))
		       (newnode (if (null rootnode) 
				    (make-node)
				    (copy-vector rootnode 0))))
		   (progn (setf (aref newnode childidx) 
				(walk (if (null rootnode) 
					  (make-node)  
					  (aref rootnode childidx)) 
				      (- shift +bit-width+)))	 
			  newnode)))))
    (walk root shift0)))

(defun insert-path (rootnode shift idx x)
  "Copies the path to the node at idx, replacing the value of the final node
   on the path, the address at idx,  with value x."
  (copy-path rootnode shift idx 
	     #'(lambda (node) 
		 (progn (setf (aref node (last-five-bits idx)) x) 
			node))))

(defun get-node (v idx)
  "Fetches the node (an object array) at index idx, from
   persistent vector v, where idx is 0-based.  Currently assumes 
   5-bit encoding of integer keys for each level, thus 32 elements 
   per level."
  (if (and (<= idx (pvec-counter v)) (>= idx 0))
      (if (>= idx (tail-end (pvec-counter v) +branches+))
	  (pvec-tail v)
	  (find-node (pvec-root v) (pvec-shift v) idx))
      (error 'index-out-of-bounds)))

(defun nth-vec (v idx)
  "Returns the nth element in a persistent vector."
    (aref (get-node v idx) (last-five-bits idx)))

;copy-vector should probably use displaced arrays. 
(defun conj-tail  (v x)
  "Conjoins item x onto pvector v's tail node, returning a new pvector that 
   uses the new tail, along with an incremented count."
  (let ((newtail (if (null (pvec-tail v)) 
		     (vector x)
		     (let ((growntail (copy-vector (pvec-tail v) 1)))
		       (progn (setf (aref growntail (1- (length growntail))) x)
			      growntail)))))
	   (make-pvec :root (pvec-root v) 
		      :tail  newtail
		      :shift (pvec-shift v)
		      :counter (1+ (pvec-counter v)))))

(defun new-path (shift node)
  "Given a node and an amount of initial 'shift', recursively builds 
   a nested tree of nodes, currently 32-wide arrays, linked by the first element, 
   with node at the logical 'bottom' of the tree, where shift = 0.  This allows us 
   to inject a node, with the required path structure, into the trie, if the path did 
   not exist before.  Typically used for inserting the tail into the pvector."
  (if (zerop shift)
      node
      (let ((newnode (make-node)))
	(progn (setf (aref newnode 0) 
		     (new-path (- shift +bit-width+) node))
	       newnode))))
	   
;;note-> the clojure implementation gets really esoteric, or non-obvious to 
;;me due to bit twiddling, but it's a good exercise for a newb like me...
;;The key check to see whether the current root can handle a tail node, 
;;which is 2^5 elements wide, which brings the count up to counter + 
;;2^5, is this line in Java:  if (count >>> 5) > (1 << shift) then ...
;;where shift is the initial shift amount, or height of the trie 
;;the pvector is rooted on.  Bear in mind, the height of the trie may 
;;or may not be sufficient to account for the tail node, which is 
;;initially just sort of "floating" outside the trie.  That's 
;;where the >>> 5 comes in...
;;The primary discriminator here is (count >>> 5) > (1 <<< shift).

;How do we know if a trie needs to grow to accomodate a new entry? 
;We need to know how many children the trie can address, C. 
;If the next entry is C+1, or more generally, C+n, we need to grow the trie.
;can a vector with count elements be represented by a vector with 
;  (/ shift 5 ) levels? 
;A vector with (/ shift (log branch-factor 2)) levels can address 
;  (expt branch-factor (/ shift (log branch-factor 2))) children.
;A vector with count' elements would be a child of the (/ count' branch-factor) node.
;Thus, if (< (/ count' branch-factor) (expt branch-factor (/ shift (log branch-factor 2))) 
;then the shift amount is sufficient, else, we need to increase the 
;height (shift), and insert the new node as a child in the "larger" trie.

(declaim (inline max-capacity))
(defun max-capacity (h &optional (b +branches+))
  "Given a trie of base b, with h levels, returns the maximum 
   capacity of the trie."
  (expt b h))

(defun root-full? (v)
  "The root node of vector v is full if and only if the addition of another 
   node would exceed the height of v.  We know that for every shift level, 
   where shift is a multiple of the trie's branch-factor (default is 32), 
   we can contain branch-factor elements.  So the  
   capacity of the trie  = (expt branch-factor (/ shift branch-factor)).
   If t"  
  (> (>>> (pvec-counter v) 5)
     (<<< 1 (pvec-shift v))))

(defun push-tail (v tl)
  "Given presistent vector v, returns a new persistent vector, with 
   tail t subordinated as a right-most child node.  Returned persistent 
   vector has a fresh tail."  
  (labels ((aux (count shift parentnode tailnode)
	     (let* ((idx (last-five-bits (>>> (1- count) shift))) ;where is the trail currently? 
		    (newparent (copy-vector parentnode 0)) ;duplicate the path so far...
		    (newchild (if (= shift +bit-width+) ;if the trail has ended -base case- 
			       tailnode  ;we have a place to push the tail...
			       (let ((currentchild (aref parentnode idx))) ;otherwise, see if we can descend more..
				 (if (not (null currentchild)) ;if we can...
				     (aux count (- shift +bit-width+) currentchild tailnode) ;descend into the next level.
				     (new-path (- shift +bit-width+) tailnode)))))) ;otherwise, create the necessary structure.
	       (progn (setf (aref newparent idx) newchild) ;embed the newly found/created node(s) as a child of the parent.
		      newparent)))) ;back out, building a (copied and modified) path of nodes as we go...
    (let ((newroot (aux (pvec-counter v) (pvec-shift v) (if (null (pvec-root v)) (make-node) (pvec-root v)) tl)))
      (->pvec newroot nil (pvec-shift v) (pvec-counter v)))))


;; (defun push-node (v node)
;;   "Given presistent vector v, returns a new persistent vector, with 
;;    tail t subordinated as a right-most child node.  Returned persistent 
;;    vector has a fresh tail."  
;;   (labels ((aux (idx shift parentnode newnode)
;; 	     (let* ((idxchild (last-five-bits (>>> (1- count) shift))) ;where is the trail currently? 
;; 		    (newparent (copy-vector parentnode 0)) ;duplicate the path so far...
;; 		    (newchild (if (= shift +bit-width+) ;if the trail has ended -base case- 
;; 			       newnode  ;we have a place to push the tail...
;; 			       (let ((currentchild (aref parentnode idxchild))) ;otherwise, see if we can descend more..
;; 				 (if (not (null currentchild)) ;if we can...
;; 				     (aux idx (- shift +bit-width+) currentchild newnode) ;descend into the next level.
;; 				     (new-path (- shift +bit-width+) newnode)))))) ;otherwise, create the necessary structure.
;; 	       (progn (setf (aref newparent idx) newchild) ;embed the newly found/created node(s) as a child of the parent.
;; 		      newparent)))) ;back out, building a (copied and modified) path of nodes as we go...
;;     (let ((newroot (aux (pvec-counter v) (pvec-shift v) (if (null (pvec-root v)) (make-node) (pvec-root v)) tl)))
;;       (->pvec newroot nil (pvec-shift v) (pvec-counter v)))))

(defun grow-root (v &optional (newchild nil))
  "When the trie must be grown to accomodate a new child node, we create a new pvector, 
   that has an increased shift height, an empty tail node, the original node as its first 
   child, and the new child node placed as the second child."
  (let ((rt (make-array +branches+)))
    (progn (setf (aref rt 0) (pvec-root v))
    	   (setf (aref rt 1) newchild)
    	   (->pvec rt nil (+ (pvec-shift v) +bit-width+) (pvec-counter v)))))  

(defun cons-vec (v x)
  "Constructs a new persistent vector that is the logical cons of 
   value x to the tail of v.  Persistent vectors act like queues...except 
   they offer near-constant (log32(n)) access to elements, and O(1) access 
   to the tail."
  (with-slots (counter tail shift root) v
    (if (< (- counter (tail-off v)) +branches+)
	(conj-tail v x)
	(let ((newroot 	(if (root-full? v) 
			    (grow-root v (new-path (pvec-shift v) tail))
		            (push-tail v tail))))
	  (conj-tail newroot x)))))

(defun cons-many (v xs)
  "Conjoin multiple xs onto v.  Will probably deprecate this in favor of 
   the conj generic function."
  (reduce (lambda (acc x) (cons-vec acc x)) xs :initial-value v))

(defun persistent-vector (&rest xs)
  "Funcallable constructor for building vectors from arglists.  Used for
   read-macro dispatch as well."
  (cons-many (empty-vec) xs))

(defun assoc-vec (v idx x)
  "Returns a persistent vector that associates value x at 
   position idx.  If idx = the count of the vector, the 
   new vector will be automatically grown."
  (let ((count (pvec-counter v)))
    (if (< idx (pvec-counter v))
	(with-slots (shift root tail) v
	  (if (<= (- idx (tail-off v)) +branches+)
	      (let ((newtail (copy-vector tail 0)))
		(->pvec root
			(progn (setf (aref newtail (last-five-bits idx)) x) newtail)   
			shift
			count))
	      (->pvec (insert-path root shift idx x) tail shift count)))
	(if (= idx count)
	    (cons-vec v x)
	    (error 'index-out-of-bounds)))))	    		

(defun vec-chunks (v &key (direction :reverse))
  "Returns a list of the chunks (arrays) that compose the vector.
   Since we already have a function that fetches the vector chunks for us, we 
   call get-node over a list of appropriate indices to get all the chunks.
   If a direction is supplied, other than :reverse, will return the vector 
   in-order, with the tail last.  Otherwise, the nodes are returned tail-first
   by default."
  (do ((i 0 (incf i +branches+))
       (acc (list (get-node v 0)) (push (get-node v i) acc)))
      ((> (+ i +branches+) (tail-off v)) 
       (if (not (eq direction :reverse))
	   (nreverse acc)
	   acc))))

(defun vec-to-array (v)
  "Extracts the values from persistent vector v into a simple vector.  Processes 
   the chunks from tail to head for efficiency."
  (let ((newarr (make-array (pvec-counter v)))
	(chunklist (vec-chunks v)))
    (labels ((load-chunk (idx chunk chunks)
	       (if (null chunk)
		   newarr
		   (let* ((rawlength (length chunk))
			  (sublength (1- rawlength)))
		     (progn (dotimes (i rawlength)
			      (setf (aref newarr (- idx i)) (aref chunk (- sublength i))))
			    (load-chunk (- idx rawlength) 
					(first chunks) 
					(rest chunks)))))))
      (load-chunk (1- (pvec-counter v))  (first chunklist) (rest chunklist)))))

(defun vec-to-list (v)
  "Extracts the values from persistent vector v into a simple list."
  (let ((newlist nil))
    (dolist (chunk (vec-chunks v) newlist)
      (let* ((rawlength (length chunk))
	     (sublength (1- rawlength)))
	(dotimes (i rawlength)
	  (push (aref chunk (- sublength i)) newlist))))))

(defun vec-print (v &optional (stream t))
  (format stream "[~{~a~^ ~}]" (vec-to-list v)))

(defmethod print-object ((obj pvec) stream)
  (vec-print obj stream))

;; (defun vec-iterator (v)
;;   (let ((node nil)
;; 	(idx 0))

;Testing helpers...
(defun range (n)
  (loop for i below n collect i))

(defun rangedata (n)
  (mapcar (lambda (x) (list :entry x)) (range n)))

(defun sample-vec (n) 
  (cons-many (empty-vec) (rangedata n)))

(defun inline-vec ()
  "Example of inline-vector creation, using clojure
   syntax."
  [1 2 3 4 5 6 7 8 9 10])

;; ;not yet built...
;; (defmethod print-object ((obj pvec) stream)
;;   (do ((nodes (pvec-root obj) (next acc))
;;        (


;; (defun resize-vec (v idx)
;;   "Continually grows persistent vector v until it can 
;;    address idx."
;;   (grow-root (push-tail v (pvec-tail v)) (make-node)) idx x)




;;Ancient history....original derivation from Spiewak's post.


;;(def myvec [])
;;so myvec -> 
;;   #PVec{:root nil :tail NodeTree [] :shift 0 :counter 0 :branchfactor 2} 
;let's say we use 10-way tries instead of 32, for simplicity 
;while counter <= 10, we just copy the tail node, add the value to it, 
;and return a new pvector that points to it...
;(insert-list  myvec '(1 2))

;; (defun insert-values (v xs)
;;   (let* ((conjcount (count xs))
;; 	 (newlength (+ (count-tail v) conjcount))))
;;   (if (<= newlength (pvec-branch-factor v))
;;       (

;; (defun get-height (tr)
;;   (if (zerop (trie-length tr))
;;       0
;;       (floor (log (trie-length tr) (trie-branch-factor tr)))))




;; (defparameter *nums* #())
;; (defun conj-num (n)
;;   (let ((arr #()))
;;   (dotimes (i n)
;;     (

;; ;;testing
;; ;; CL-USER> (get-height (make-trie :length 32))
;; ;; 1
;; ;; 0.0
;; ;; CL-USER> (get-height (make-trie :length 2))
;; ;; 0
;; ;; 0.2
;; ;; CL-USER> (get-height (make-trie :length 15))
;; ;; 0
;; ;; 0.78137815
;; ;; CL-USER> (get-height (make-trie :length 1024))
;; ;; 2
;; ;; 0.0
;; ;; CL-USER> (get-height (make-trie :length 5000))
;; ;; 2
;; ;; 0.45754242
;; ;; CL-USER> 

;; (defun get-node (n tr)
;;   "Given an integer index n, fetch the node from trie 
;;    tr, iff n is a valid index in tr."
;;   (assert (and (<= n 0) (< n (trie-length v))) (n) "Index n is out of bounds...")
;;   (do ((level 0 (incf level)
;;        (index n (>>> index 5)

;; ;taking a cue from speiwak, but implementing in lispier style...
;; ;provide a few helper functions to derive a "path" through 
;; ;the trie.

;; ;1: -> convert a number, base pair into a list of (numeral .  digit) pairs, 
;; ; where each pair represents a component of the list-represented number.
;; ;2:->  compose one or more pairs of (numeral .  digit) into a list, whose 
;; ; entries correspond to a number. 

;; ;start with 2...
;; (defun ->numeral (n digit) 
;;   "Represents a component of a number.  Composed with 
;;    other numerals."
;;   (list n digit))

;; (defun pad-amount (numeral1 numeral2)
;;   (1- (- (second numeral1) (second numeral2))))

;; (defun write-numeral (n pad acc)
;;   (progn 
;;     (push n  acc)
;;     (dotimes (i pad)
;;       (push  0 acc))
;;     acc))

;; (defun nums->numlist (&optional acc ns)
;;   "Given a list of '(digit place) numerals, converts the 
;;    numerals into a padded-list representation. "
;;   (if (null ns)
;;       (nreverse acc)
;;       (let ((numeral (first ns))
;; 	    (next-numeral (second ns)))
;; 	(cond ((null next-numeral)
;; 	        (nums->numlist (write-numeral (first numeral) (1- (second numeral)) acc)))
;; 	      ('t 
;; 		(nums->numlist (write-numeral (first numeral) (pad-amount numeral next-numeral) acc) (rest ns))))))) 

;; (defun shift-max (x base)
;;   "Given an input integer x, and a base, returns a list of 
;;    '(digit place), where place is the maximum amount of 
;;    shifts or divisions possible in the given base, before x 
;;    is < the numeric base.  Digit is truncated, so that the 
;;    result is representative of the 'left-most' digit in 
;;    the numeric base, where the numeric components increase in 
;;    a leftward fashion."
;;   (do ((digit x (/ digit base))
;;        (position 0 (incf position)))
;;       ((< digit base) (list (truncate digit) position))))   
	
;; (defun compute-path (n base)
;;   "Given base-10 integer N, and a target base, computes a list representation 
;;    of the resulting number in the new base.  Used for mapping paths through 
;;    tries, where the branches of the trie correspond to the base. "
;;   (assert (>= n 0) (n) "Index n must be a positive integer")
;;   (labels ((get-path (n acc)
;; 	     (if (< n base)
;; 		 (nums->numlist (list) (nreverse (cons (shift-max n base) acc)))
;; 		 (let* ((numeral   (shift-max n base))
;; 			(places    (second numeral))		
;; 			(remainder (mod n (expt base places))))
;; 		   (get-path remainder (cons numeral acc))))))
;;     (get-path n (list))))
	
;; ;;testing
;; ;CL-USER>(compute-path 20017 10)
;; ;(2 0 0 1 7)

;; ;this is "not" the actual way tries are implemented in clojure.
;; ;the hashing function, to compute a path from b10N -> b32N 
  


