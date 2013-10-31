;This is an implementation of Clojure's 
;persistent map for Common Lisp.
(defpackage :clclojure.pmap
  (:use :common-lisp)
  (:export :persistent-map
	   :empty-map?
	   :pmap-count 
	   :pmap-map
	   :pmap-reduce)
  (:shadow :assoc
	   :find))
;	   :pmap-chunks
;	   :pmap-element-type
;	   :pmap-assoc
;	   :pmap-nth))
(in-package clojure.pmap)

;Original from Stack Overflow, with some slight modifications.
(defun |brace-reader| (stream char)
  "A reader macro that allows us to define persistent maps
   inline, just like Clojure."
  (declare (ignore char))
  `(persistent-map ,@(read-delimited-list #\] stream t)))
(set-macro-character #\{ #'|brace-reader|)
(set-syntax-from-char #\} #\))


(define-condition not-implemented (error) 
  ((text :initarg :text :reader text)))

;utility functions

;Persistent maps require a lot of array copying, and 
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

(defun mask (hash shift)
  "Helper, used by maps.  Maps a hash into a local index at 
   given level in the trie."
  (last-five-bits (>>> hash shift)))

(defun bit-pos (hash shift)
  "Helper to compute the bit-position of n from a mask.  This provides 
   a mapping to the nth bit"
   (<<< 1 (mask hash shift)))

(defun index (n)
  "Given an index into a hash, which represents a sparse mapping of values 
   from [0 31] to n children, we can find out which child the index represents
   by using a logical count of the 1 bits in n."
  (logcount (1- n)))

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

;Persistent Map definition: 

;A persistent hashmap is a small structure that points to a root node.
;It also contains information about the underlying trie, such as null 
;keys, the count of elements, etc.
(defstruct pmap (count 0) (root nil) (has-null nil) (null-value nil))
(defun ->pmap (count root has-null null-value)
  (make-instance pmap :count count 
		      :root root 
		      :has-null has-null 
		      :null-value null-value))
(defconstant +empty-map+ (->pmap))
(defun empty-map () +empty-map+)

;The INode interface is crucial.  We dispatch based on the node types...
;We'll implement the interface as a set of generic functions. 

(defgeneric assoc (nd shift hash key val &optional addedLeaf))
(defgeneric without (nd shift hash key))
(defgeneric find (shift  hash  key))
;(defgeneric find(shift hash key notFound))
(defgeneric nodeSeq (nd))
; assoc(AtomicReference<Thread> edit, int shift, int hash, Object key, Object val, Box addedLeaf);
;INode without(AtomicReference<Thread> edit, int shift, int hash, Object key, Box removedLeaf);
(defgeneric kvreduce (f init))
;(defgeneric fold (combinef reducef  fjtask  fjfork fjjoin))

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
(defun make-indexed-node () 
  (make-data :branches +indexed-branches+))

;define implementations for different node types: 

;-Type identifier for empty nodes, i.e. empty maps. 
;empty-node
;(defstruct empty-node

;-Type identifier for nodes with a single value.
;-This is optimized away in the clojure java implementation...
;leaf-node

;-Type identifier for nodes with 16 key/vals (full or array nodes).
(defstruct array-node count (nodes (make-data)))
(defun ->array-node (count nodes)
  (make-array-node :count count :nodes nodes))

;-Type identifier for nodes that project a 32-bit index, the 
;-bitmap, onto an array with less than 32 elements.
;bitmapindexed-node
(defconstant +indexed-branches+ 16) ;indexes contained 8 keyval pairs.
(defstruct indexed-node (bitmap 0) (nodes (make-data :branches +indexed-branches+)) )
(defun ->indexed-node (bitmap nodes)
  (make-indexed-node :bitmap bitmap :nodes nodes))
(defconstant +empty-indexed-node+  (make-indexed-node))
(defun empty-indexed-node () +empty-indexed-node+)

(declaim (inline key-idx val-idx key-at-idx val-at-idx))
(defun key-idx (idx)
  "Return the offset key of an index"
  (* 2 idx))
(defun val-idx (idx)
  "Return the offset value of an index"
 (1+ (* 2 idx)))


(defun equiv (x y)
  "Generic equality predicate."
  (error 'not-implemented))

(defun key-at-idx (idx nodes)
  "Fetches  the offset key from an array 
   packed like a propertylist, key/val/key/val/..."
  (aref nodes (* 2 idx)))

(defun val-at-idx (idx nodes)
  "Fetches  the offset value from an array 
   packed like a propertylist, key/val/key/val/..."
  (aref nodes (1+ (* 2 idx))))

(defun pairs (xs)
  "Aux function that converts a list of xs into 
   a list of pairs."
  (do ((acc (list))
       (remaining xs (rest (rest remaining))))
      ((null remaining) (nreverse acc))
    (let ((x (first remaining))
	  (y (second remaining)))
      (when (and x y)
	(push (list x y) acc)))))      

(defun assoc-array (arr idx k v)
  (progn (setf (aref arr (key-idx idx)) k)
	 (setf (aref arr (val-idx idx)) v)))

(defun hash (o)
  "Generic hash function."
  (error 'not-implemented))

(defun remove-pair (array idx)
  "Auxillary function to drop pairs from an array 
   where the pairs are packed akin to a plist, ex. 
   key/val/key/val....returns a new, smaller array 
   with the pair removed."
  (error 'not-implemented)
  (cond ((> idx (- (1- (array-total-size array)) 2))
	 (error 'index-out-of-bounds))
	((and (= (array-total-size array) 2) (= idx 0))
	 nil)
	(t
	 (let* ((dimensions (decf (first (array-dimensions array)) 2))
		(new-array (make-array dimensions
				       :element-type (array-element-type array)
				       :fill-pointer (and (array-has-fill-pointer-p array)
							  (fill-pointer array))
				       :adjustable (adjustable-array-p array))))
	   (loop for i from 0 to (1- idx)
	      do (assoc-array new-array i (key-at-idx i array) (val-at-idx i array)))
	   (loop for i from (1+ idx) to (- (array-total-size new-array) 2)
	      do (assoc-array new-array (1- i) (key-at-idx i array) (val-at-idx i array)))
	   new-array))))

(defun insert-pair (array idx k v)
  "Auxillary function to drop pairs from an array 
   where the pairs are packed akin to a plist, ex. 
   key/val/key/val....returns a new, smaller array 
   with the pair removed."
  (error 'not-implemented)
  (cond ((> idx (1- (- (array-total-size array) 2)))
	 (error 'index-out-of-bounds))
	(t
	 (let* ((dimensions (incf (first (array-dimensions array)) 2))
		(new-array (make-array dimensions
				       :element-type (array-element-type array)
				       :fill-pointer (and (array-has-fill-pointer-p array)
							  (fill-pointer array))
				       :adjustable (adjustable-array-p array))))
	   (loop for i from 0 to (1- idx) 
	      do (assoc-array new-array i (key-at-idx i array) (val-at-idx i array)))
	   (assoc-array new-array idx k v)
	   (when (< idx (1- (/ (array-total-size array) 2)))
	     (loop for i from idx to (1- (/ (array-total-size new-array) 2))
		do (assoc-array new-array (1+ i) (key-at-idx i array) (val-at-idx i array))))
	   new-array))))
;; INode createNode(int shift, Object key1, Object val1, int key2hash, Object key2, Object val2) {
;; int key1hash = hash(key1);
;; if(key1hash == key2hash)
;; return new HashCollisionNode(null, key1hash, 2, new Object[] {key1, val1, key2, val2});
;; Box _ = new Box(null);
;; AtomicReference<Thread> edit = new AtomicReference<Thread>();
;; return BitmapIndexedNode.EMPTY
;; .assoc(edit, shift, key1hash, key1, val1, _)
;; .assoc(edit, shift, key2hash, key2, val2, _)
					;
(defun create-node (shift key1 val1 key2hash key2 val2)
  "Generic function to create nodes....I need a better 
   explanation.  Also, the box var may not be necessary.
   I have to see how it's used..."
  (let ((key1hash (hash key1)))
    (if (= key1hash key2hash)
	;record the hash collision with a new collision node, these will chain.
	(->collision-node key1hash 2 (vector key1 val1 key2 val2)) 
	(let ((box (list))) ;else assoc both values into an empty indexed node.
	  (assoc (assoc (empty-indexed-node) shift key1hash key1  val1 box)
		 shift key2hash key2 val2 box)))))

(defun clone-and-set (arr &rest kvps) 
  "Aux function that clones an array and 
   sets the element at idx = to v, where 
   idx and v are drawn from the list kvps."
  (let ((acc (copy-vector arr 0)))
    (loop for (idx v) in (pairs kvps)
       do (setf (aref acc idx) v)
       finally (return acc))))

(declaim (inline bit-set?))
(defun bit-set? (bitmap i)
  "Determines if the ith bit is set in bitmap."
  (not (zerop (logand (>>> bitmap i) 1))))

(defun indexed-array->full-array (nodes bitmap shift hash key val addedleaf)
  "Projects an indexed array, indexed by a 32-bit map, 16 (unknown) 
   bits of which indicate the presence of a key in an an underlying 
   32-element assoc array, onto an full array.  The full array is 
   a direct mapping of a 32-bit key, projected onto a 32 element 
   array of nodes, by masking all but the last 5 bits.  This is 
   basically an optimization, so that we use indexed nodes while 
   the node is sparse, when the keys are <= 16, then shift to a 
   node with no intermediate bit mapping."
  (let ((newnodes (make-data)) ;create the 32 element array for the arraynode.
	(jdx (mask hash shift)) ;set the index for the element we're adding.
	(j 0))
    (progn (setf (aref newnodes jdx)  ;initialze the newly added node.
		 (assoc (empty-indexed-node) 
			(+ 5 shift) hash key val addedleaf)) 	       
	   (loop for i from  0 to 32 ;traverse the bitmap, cloning...
	      do (when (bit-set? bitmap i) ;bit i is stored at j
		   (if (null (aref nodes j))
		       (setf (aref newnodes i) (aref nodes (1+ j)))
		       (assoc (empty-indexed-node)
			      (+ 5 shift) 
			      (hash (aref nodes j))
			      (aref nodes j)
			      (aref nodes (1+ j))
			      addedleaf))
		   (incf j 2)))
	   newnodes)))


(defun indexed-node->array-node (nd shift hash key val addedleaf)
  "Projects an indexed node, indexed by a 32-bit map, 16 (unknown) 
   bits of which indicate the presence of a key in an an underlying 
   32-element assoc array, onto an array node.  The array node is 
   a direct mapping of a 32-bit key, projected onto a 32 element 
   array of nodes, by masking all but the last 5 bits.  This is 
   basically an optimization, so that we use indexed nodes while 
   the node is sparse, when the keys are <= 16, then shift to a 
   node with no intermediate bit mapping."
  (with-slots (bitmap nodes) nd
    (->array-node (1+ (logcount bitmap)) 
      (indexed-array->full-array nodes  bitmap  shift  
				 hash  key val addedleaf))))

;Partially implemented.  
;; (defmethod assoc ((nd indexed-node) shift hash key val addedLeaf)
;;   (with-slots (bitmap nodes) nd
;;     (let ((b (bit-pos hash shift))
;; 	  (idx (index b))
;; 	  (exists? (not (zerop (logand bitmap b)))))
;;       (if exists? 
;; 	  (let ((k (key-at-idx idx nodes))
;; 		(v (val-at-idx idx nodes)))
;; 	    (cond ((null k)
;; 		   (let ((newnode (assoc v (+ 5 shift) hash key val addedleaf)))
;; 		     (if (eq val-at-idx newnode)
;; 			 nd ;no node to add to null key.
;; 			 ;actually have val associated with null, causes changed. 
;; 			 (->indexed-node bitmap (clone-and-set nodes (val-idx idx) newnode)))))
;; 		  ((equiv key k) ;key exists.
;; 		   (if (eq v val)
;; 		       nd ;no change
;; 		       ;value changed
;; 		       (->indexed-node bitmap (clone-and-set nodes (val-idx idx) val))))
;; 		  (t 
;; 		   (->indexed-node bitmap (clone-and-set nodes (key-idx idx) nil 
;; 							       (val-idx idx) (create-node (+ 5 shift) k v hash key val)))))
;; 		  )
;; 	  (if (>= (logcount bitmap) +indexed-branches+) 
;; 	      ;create an array node, or full node, if the number of on-bits is excessive.
;; 	      (indexed-node->array-node nd shift hash key val addedleaf)
;; 	      (let ((newarray (copy-vector nodes 2)))
;; 		(progn (
	      
;; )))))) 
	

;-Type identifier for nodes that have a direct correspondence
;-between a 5 bit integer hash and an entry in the 32 element
;-node array. 
;full-node 


;-Type identifier for nodes that collide.  Essentially, a 32
;-element array of nodes that have the same hash.  I have an 
;-idea of how this works using the 5 bit hashing scheme.
;collision-node
(defstruct collision-node hash count nodes)
(defun ->collision-node (hash count nodes)
  (make-instance collision-node 
		 :hash hash
		 :count count
		 :nodes nodes))

(defconstant +empty-pvec+ (make-pvec))
(defun empty-vec () +empty-pvec+)
(defun empty-vec? (v) (eq v +empty-pvec+))

(defgeneric vector-count (v)
  (:documentation 
   "Fetches the count of items in the persistent vector."))

(defmethod vector-count ((v pvec))
  (pvec-counter v))

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

(defgeneric get-node (v idx)
  (:documentation   
   "Fetches the node (an object array) at index idx, from
   persistent vector v, where idx is 0-based.  Currently assumes 
   5-bit encoding of integer keys for each level, thus 32 elements 
   per level."))

(defmethod get-node ((v pvec) idx)
  (if (and (<= idx (pvec-counter v)) (>= idx 0))
      (if (>= idx (tail-end (pvec-counter v) +branches+))
	  (pvec-tail v)
	  (find-node (pvec-root v) (pvec-shift v) idx))
      (error 'index-out-of-bounds)))

(defgeneric nth-vec (v idx)
  (:documentation   "Returns the nth element in a persistent vector."))
(defmethod nth-vec ((v pvec) idx)
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
