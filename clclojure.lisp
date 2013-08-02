;;this is a simple set of utils I'd like to have around
;;to further my knowledge, I'll stick it in a package
;;using common lisp parlance.
(load "pvector.lisp")
(load "protocols.lisp")

(defpackage :clojurecl
  (:use :common-lisp
	:clojure.protocol
	:clojure.pvector)
  (:export :take
	   :drop
	   :ndrop
	   :take-while
	   :drop-while
	   :ndrop-while
	   :filter
	   :fold 
	   :partition
	   :partition-offset
	   :interleave
	   :->>
	   :->
	   :lazy
	   :force
	   :lazy-null
	   :lazy-nil
	   :lazy-car
	   :lazy-cdr
	   :lazy-cons
	   :make-lazy
	   :iterate)
  (:shadow :first
	   :rest
	   :cons))
;:export -> names of stuff to export.

(in-package :clojurecl)

(defgeneric seq (x) 
  (:documentation 
   "Basic constructor for lazy sequences.")


;using protocols to implement library functionality
;now. 

;; clojure.lang.ISeq
;; (first [self] (first a))
;; (next [self] (next a))
;; (more [self] (rest a))

;; ;Ported from clojurescript compiler.  Fundamental protocols.
(defprotocol ICounted
   (-count [coll] "constant time count"))

(defprotocol IEmptyableCollection
   (-empty [coll]))

(defprotocol ICollection
   (-conj [coll o]))

(defprotocol IOrdinal
    (-index [coll]))

(defprotocol IIndexed
   (-nth [coll n] [coll n not-found]))

(defprotocol ASeq)

(defprotocol ISeq
  (-first [coll])
  (-rest [coll]))

(defprotocol INext
  (-next [coll]))

(defprotocol ILookup
  (-lookup [o k] [o k not-found]))

(defprotocol IAssociative
  (-contains-key? [coll k])
  #_(-entry-at [coll k])
  (-assoc [coll k v]))

(defprotocol IMap
  #_(-assoc-ex [coll k v])
  (-dissoc [coll k]))

(defprotocol IMapEntry
  (-key [coll])
  (-val [coll]))

(defprotocol ISet
  (-disjoin [coll v]))

(defprotocol IStack
  (-peek [coll])
  (-pop [coll]))

(defprotocol IVector
  (-assoc-n [coll n val]))

(defprotocol IDeref
 (-deref [o]))

(defprotocol IDerefWithTimeout
  (-deref-with-timeout [o msec timeout-val]))

(defprotocol IMeta
  (-meta [o]))

(defprotocol IWithMeta
  (-with-meta [o meta]))

(defprotocol IReduce
  (-reduce [coll f] [coll f start]))

(defprotocol IKVReduce
  (-kv-reduce [coll f init]))

(defprotocol IEquiv
  (-equiv [o other]))

(defprotocol IHash
  (-hash [o]))

(defprotocol ISeqable
  (-seq [o]))

(defprotocol ISequential
  "Marker interface indicating a persistent collection of sequential items")

(defprotocol IList
  "Marker interface indicating a persistent list")

(defprotocol IRecord
  "Marker interface indicating a record object")

(defprotocol IReversible
  (-rseq [coll]))

(defprotocol ISorted
  (-sorted-seq [coll ascending?])
  (-sorted-seq-from [coll k ascending?])
  (-entry-key [coll entry])
  (-comparator [coll]))

;; (defprotocol ^:deprecated IPrintable
;;   "Do not use this. It is kept for backwards compatibility with existing
;; user code that depends on it, but it has been superceded by IPrintWithWriter
;; User code that depends on this should be changed to use -pr-writer instead."
;;   (-pr-seq [o opts]))

;; (defprotocol IWriter
;;   (-write [writer s])
;;   (-flush [writer]))

;; (defprotocol IPrintWithWriter
;;   "The old IPrintable protocol's implementation consisted of building a giant
;; list of strings to concatenate. This involved lots of concat calls,
;; intermediate vectors, and lazy-seqs, and was very slow in some older JS
;; engines. IPrintWithWriter implements printing via the IWriter protocol, so it
;; be implemented efficiently in terms of e.g. a StringBuffer append."
;;   (-pr-writer [o writer opts]))

(defprotocol IPending
  (-realized? [d]))

(defprotocol IWatchable
  (-notify-watches [this oldval newval])
  (-add-watch [this key f])
  (-remove-watch [this key]))

(defprotocol IEditableCollection
  (-as-transient [coll]))

(defprotocol ITransientCollection
  (-conj! [tcoll val])
  (-persistent! [tcoll]))

(defprotocol ITransientAssociative
  (-assoc! [tcoll key val]))

(defprotocol ITransientMap
  (-dissoc! [tcoll key]))

(defprotocol ITransientVector
  (-assoc-n! [tcoll n val])
  (-pop! [tcoll]))

(defprotocol ITransientSet
  (-disjoin! [tcoll v]))

(defprotocol IComparable
  (-compare [x y]))

(defprotocol IChunk
  (-drop-first [coll]))

(defprotocol IChunkedSeq
  (-chunked-first [coll])
  (-chunked-rest [coll]))

(defprotocol IChunkedNext
  (-chunked-next [coll]))

(defprotocol ISeq 
    (first (s) "Returns the first element of a sequence.")
    (next  (s) "Returns the next element of the sequence or nil")
    (more  (s) "Returns the rest of sequence s as a lazy seq."))

;(defprotocol IReduce...)

;Include lists and arrays....
(extend-protocol ISeq 
    null (first (s) nil)
         (next (s)  nil)
	 (more (s)  nil)
    cons (first (s) (common-lisp:first s))
         (next  (s) (common-lisp:rest s))
         (more  (s) (seq (common-lisp:rest s)))
    clojure.pvector::pvec 
         (first (s) (nth-vec s 0))
         (next  (s) (subvec s 1))
	 (more  (s) (seq (subvec s 1))))

;; clojure.lang.IPersistentCollection
;; (seq [self] (if (seq a) self nil))
;; (cons [self o] (Foo. a (conj b o)))
;; (empty [self] (Foo. [] []))
;; (equiv
;;  [self o]
;;  (if (instance? Foo o)
;;    (and (= a (.a o))
;;         (= b (.b o)))
;;    false))

(defprotocol IPersistentCollection 
    (seq (x) "Returns a lazy sequence of the input.")
    (cons (x y) "Returns a lazy sequence constructed from x and s.")
    (empty (x) "Determines if x is empty.")
    (equiv (x y) "Equality comparison between x and y."))

;;borrowed shamelessly from Conrad Barksi's excellent 
;;Land of Lisp....the definitive work on building lisp 
;;games and being a better person! 

(defmacro lazy (&body body)  
  "Creates a lazy value from v, returning a thunk'd 
   function that, upon evaluation, caches the result."
  (let ((forced? (gensym))
	(val    (gensym)))
    `(let ((,forced? nil)
	   (,val nil))
       (lambda ()
	 (unless ,forced?
	   (setf ,val (progn ,@body))
	   (setf ,forced? 't))
	 ,val))))

(defun force (lazy-value)
  "Ensures that any thunks are evaluated, thus providing 
   the rich, tender values underneath.  I added a quick 
   function check to allow non-thunked values to be 
   forced, for consistency...."
  (if (functionp lazy-value)
      (funcall lazy-value)
      lazy-value))
  
(defmacro lazy-cons (x y)
  "Creates a lazy cons-cell from x and y."
  `(lazy (cons ,x ,y)))

(defun lazy-car (x)
  "Lazified version of car...note that since I 
   generalized force, we can use it on either lazy
   or non-lazy lists."
  (car (force x)))

(defun lazy-cdr (x)
  "Lazified version of cdr...again, since force 
   can handle eager values, this works for any 
   list."
  (cdr (force x)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))


(defprotocol ILazySeq 
    (make-lazy (coll) "Converts collection into a lazy sequence."))
;IChunkedSeq is a lot like subvec, in that it works in chunks.
;Basically, as we pull elements out of our chunked sequence, 
;we return light wrapper objects that refer to the chunk.
(defprotocol IChunkedSeq 
    (chunked-first (coll) "Get the first lazy chunk")
    (chunked-next (coll)  "Get the next lazy chunk")
    (chunked-more (coll)  "Get the next lazy chunk"))

(extend-protocol  ILazySeq 
     null (make-lazy (coll) nil)
     cons (make-lazy (coll) 
	    (lazy (when coll
		    (cons (first coll) 
			  (make-lazy (rest coll)))))))
 ;; clojure.pvector:pvec 
 ;;       (make-lazy (coll)
 ;; 		  (fn 


;; (defgeneric make-lazy (lst))
;; (defmethod  make-lazy ((lst cons))
;;   "Converts a normal list into a lazy list."


(defmacro ->> (x form &rest more)
  "Threading operator, identical to Clojure.
   Threads x as the last argument through form.
   If more forms are passed in, nests the threading, 
   so that each preceding form is evaluated as the 
   last form in next form."
  (if (null more) 
      (if (atom form)
	  (list form x)
	  `(,(first form) ,@(rest form) ,x))
      `(->> (->> ,x ,form) ,@more))) 

(defmacro -> (x form &rest more)
  "Threading operator, identical to Clojure.
   Threads x as the second argument through form.
   If more forms are passed in, nests the threading, 
   so that each preceding form is evaluated as the 
   second form in next form."
  (if (null more) 
      (if (atom form)
	  (list form x)
	  `(,(first form) ,x ,@(rest form)))
      `(->> (->> ,x ,form) ,@more))) 

(defgeneric conj (x coll))
;;(defmethod  conj (x 
;(defgeneric take (n l))
;; (defmethod take (n (l cons))
;;   "Takes n elements from a list"
;;   (do ((remaining l (rest remaining))
;;        (acc (list))
;;        (i   n (decf i))) 
;;       ((or (= 0 i) (null remaining)) (nreverse acc))
;;       (push (first remaining) acc)))

(defun take (n coll)
  "Takes n elements from a sequence."
  (do ((remaining l (rest remaining))
       (acc [] ()
       (i   n (decf i))) 
      ((or (= 0 i) (null remaining)) acc)
      (push (first remaining) acc)))


(defun drop (n coll)
  "Drops the first n elements from a sequence."
  (do ((remaining l (rest remaining))
       (acc nil)
       (i n (decf i)))
      ((null remaining) acc)
      (when (zerop i) 
	(progn (setf acc (copy-list remaining))
	       (setf remaining nil)))))  

;(defgeneric drop (n l))
;; (defmethod drop (n (l cons))
;;   "Drops the first n elements from a list"
;;   (do ((remaining l (rest remaining))
;;        (acc nil)
;;        (i n (decf i)))
;;       ((null remaining) acc)
;;       (when (zerop i) 
;; 	(progn (setf acc (copy-list remaining))
;; 	       (setf remaining nil)))))  

(defun ndrop (n l)
  "Drops the first n elements from a list.  Returns the sublist 
   of the inputlist, rather than accumulate a copy."
  (do ((remaining l)
       (i n (decf i))) 
      ((or (= 0 i) (null remaining)) remaining)
      (when (not (zerop i))
	(setf remaining (rest remaining)))))

(defgeneric filter (f l))
(defmethod filter (f (l cons))
  "Returns a new list l, for all elements where 
   applications of f yield true."
  (do ((remaining l (rest remaining))
       (acc (list)))
      ((null remaining) (nreverse acc))
    (when (funcall f (first remaining))
      (push (first remaining) acc))))
                
(defgeneric take-while (f l))
(defmethod take-while (f (l cons))
  "Draws elements from a list while f yields true.
   Returns the resulting list."
  (do ((remaining l (rest remaining))
       (acc (list)))
      ((null remaining) (nreverse acc))
    (if (funcall f (first remaining))
	(push (first remaining) acc)
	(setf remaining nil))))

(defgeneric drop-while (f l))
(defmethod drop-while (f (l cons))
  "Draws elements from a list while f yields true.
   Returns the resulting list."
  (do ((remaining l (rest remaining))
       (acc (list)))
      ((null remaining) acc)
    (when (not (funcall f (first remaining)))
	(progn (setf acc (copy-list remaining))
	       (setf remaining nil))))) 

(defun ndrop-while (f l)
  "Draws elements from a list while f yields true.
   Returns the resulting list.  Impure."
  (do ((remaining l (rest remaining))
       (acc nil))
      ((null remaining) acc)
    (when (not (funcall f (first remaining)))
	(progn (setf acc remaining)
	       (setf remaining nil))))) 

(defun fold (f init l)
  "A simple wrapper for reduce."
  (reduce f l :initial-value init))

(defgeneric partition (n l &key offset))
(defmethod  partition (n (l cons) &key (offset n))
  "Akin to partition from clojure.  Builds 
   a list of lists, where each list is size n 
   elements."
   (do ((remaining l (ndrop offset remaining))
	(acc (list)))
       ((null remaining) (nreverse acc))
     (let ((nxt (take n remaining)))
       (if (= (length nxt) n)
	   (push nxt acc)
	   (setf remaining nil)))))

(defun partition-offset (n offset l)
  "A form of partition, with adjustable offsetting
   that is friendly to the ->> threading macro."
  (partition n l :offset offset))

(defgeneric interleave  (xs ys))
(defmethod  interleave ((xs cons) (ys cons))
  "Returns a list composed of interwoven values drawn from
   input lists xs and ys.  Stops the interleaving process 
   when either list is exhausted."
  (do ((left xs (rest left))
       (right ys (rest right))
       (acc nil))
      ((or (null left) (null right)) (nreverse acc))
    (progn 
      (push (first left) acc)
      (push (first right) acc))))



(defun iterate (f init)
  "Produces a lazy sequence of results, where 
   f is applied repeatedly, first to init, then 
   to the result (f (f (f init)))"
  (let ((res (funcall f init)))
    (if res 
      (lazy-cons res (iterate f res))
      nil)))

;(lazy-cons 2 nil) 
;(lazy-list 2) 
;(lazy-list 2 3 4) -> (lazy-cons 2 (lazy-cons 3 (lazy-cons 4)))
	   
;;(->> (list) 
;;     (mapcar #'1+))

;;(mapcar #'1+ (list))

