(defpackage :sequences
  (:use :common-lisp :common-utils)
  (:shadow :first :rest :second :map :filter :reduce :cons :flatten :apply)
  (:export
   :apply
   :first
   :second
   :rest
   :map
   :filter
   :concat
   :reduce
   :flatten
   :take
   :drop
   :take-while
   :drop-while
   :filter
   :fold 
   :partition
   :partition-offset
   :interleave
   :iterate
   :init-reduce?
   :internal-reduce?
   :seq-count))

(in-package :sequences)
;;an abstract lazy sequence.  
;;This is meant to serve as an interface for anything that 
;;wants to act like a seq.  We stick it in s and pass all the 
;;sequence functions onto s.

;;maybe extend to sbcl's sequence type later.
;;I had this deriving from sbcl's extensible sequence deal,
;;but ran into problems with method dispatch causing things to
;;break (like literally crashing sbcl).  Removed for now.
(defclass  LazySeq ()                   ;(sequence standard-object)
  ((value       :initarg :value)
   (pending     :initarg :pending)))

(defclass FuncSeq ()
  ((sequence     :initarg :sequence)
   (sval         :initarg :sval)
   (seed         :initarg :seed)))

(defun seq? (s)
  (or  (eq (type-of s) 'LazySeq)
       (eq (type-of s) 'FuncSeq)))


(defmacro lazy-seq (&rest body)
  `(make-instance 'FuncSeq
                  :sequence nil
                  :sval nil
                  :seed (delay ,@body)))

(defmethod sval ((obj FuncSeq))
  (with-slots (sval seed sequence) obj
    (when seed
      (setf sval (force seed))
      (setf seed nil))
    (if (not (null sval))
        sval
        sequence)))

(defun func-seq? (s) (eq (type-of s) 'FuncSeq))

(defmethod seq ((obj FuncSeq))
  (sval obj)
  (with-slots (sval sequence) obj
    (if (not (null sval))
        (let ((ls sval))
          (setf sval nil)
          (setf sequence 
                (loop until   (not (func-seq? ls))
                      do      (setf ls (sval ls))
                      finally (return ls)))
          sequence))))

;;sb-sequence claims these are its fundamental protocol:

;;Ugh....It's easier to just define our own stuff and use that internally.
;;length
;;elt
;;(setf elt [sb-sequence])
;;adjust-sequence
;;make-sequence-like

;;Naive protocol for singly-linked sequences.  We don't
;;take advantage of chunking here for now.
(defgeneric seq-first (obj))

(defmethod seq-first ((obj common-lisp:cons))
  (common-lisp:first obj))
(defmethod seq-first ((obj null))
  nil)

(defmethod seq-first ((obj LazySeq))
  (slot-value obj 'value))
(defmethod seq-first ((obj FuncSeq))
  (seq-first (seq obj)))

(defgeneric seq-rest  (obj))

(defmethod seq-rest ((obj common-lisp:cons))
  (common-lisp:rest obj))
(defmethod seq-rest ((obj FuncSeq))
  (seq-rest (seq obj)))

(defun method-args (method)
  (length (sb-mop::generic-function-lambda-list method)))

(defun ts (n)
  (loop for i from 1 to n
        collect (quote  t)))

;;this only works for single-arity stuff....
(defun implements? (method x)
  (find-method method '() (list*  (class-of x) (ts (1- (method-args method)))) nil))

;;general lazy sequence constructors.
;;Coerce a thing into a LazySeq
(defgeneric seq (xs))
;;maybe inefficient unless we
;;memoize, but fine for bootstrapping.
(defun seqable? (x)
  (or (seq? x)
      (when (implements? #'seq x))
        t))

(defmethod seq ((xs LazySeq)) xs)

(defmacro lazy-cons (x y)
  "Creates a LazySeq from x and y."
  `(make-instance 'LazySeq :value ,x :pending (delay ,y)))

(defmethod seq ((xs common-lisp:cons))
  (if (not (null xs))
      (lazy-cons (seq-first xs) (seq-rest xs))
      nil))

(defmethod seq ((xs null))
  nil)

;; (defmethod more ((obj LazySeq))
;;   (promise? (slot-value obj ')))

(defmethod lazy-rest ((s LazySeq))
  (let ((old (slot-value s 'pending)))
    (if (not (promise? old))
        old
        (let ((new (force old)))
          (setf (slot-value s 'pending) new)
          new))))

(defmethod seq-rest ((obj LazySeq))
  (let ((xs (lazy-rest obj)))
    (when (not (null xs))
      (lazy-cons (seq-first xs) (seq-rest xs)))))

(defmethod seq-rest ((obj null))
  nil)

;;temporary printing helper
;;we'd really like to lazily print.
(defun seq->list (lz)   
  (loop with head = (seq lz) 
        until   (null head)
        collect (seq-first head)
        do      (setf head (seq-rest head))))

;;I like this one way better.
;; (defun seq->list (lz)   
;;   (labels ((aux (acc xs)
;;              (if (seq xs)
;;                  (aux (cons (seq-first xs) acc) (seq-rest xs))
;;                  acc)))
;;     (nreverse (aux '() lz))))

;;naive eager version.
(defun print-seq (s &optional (stream t))
  "Generic vector printer."
  (format stream "(~{~s~^ ~})" (seq->list s)))

                                        ;extend printing to both pvecs and subvectors
(defmethod print-object ((obj LazySeq) stream)
  (print-seq obj stream))

(defmethod print-object ((obj FuncSeq) stream)
  (print-seq obj stream))

;;Basic Seq API
;;=============
(defun  first (xs) (seq-first (seq xs)))
(defun  rest  (xs) (seq-rest  (seq xs)))
(defun  next
  (coll)
  "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil"
  (when-not (nil? coll)           
      (seq (rest coll))))

(defun cons (x obj)
  (cond ((null obj) (list x))
        ((listp obj)
         (common-lisp:cons x obj))
        (t (lazy-cons x (seq obj)))))

(defgeneric empty? (obj))
(defmethod  empty? ((obj common-lisp:cons))
  (null obj))
(defmethod  empty? ((obj LazySeq))
  (null (seq obj)))
(defmethod  empty? ((obj null))
  t)


;;defines a simple protocol for objects that
;;natively implement efficient reductions.
(defgeneric internal-reduce (obj f))
(defgeneric init-reduce     (obj f init))

(defun init-reduce?     (obj)
  (or (sb-kernel::sequencep  obj)
      (implements? #'init-reduce obj)))
(defun internal-reduce? (obj)
  (or (sb-kernel::sequencep  obj)
      (implements? #'internal-reduce obj)))

;;This will likely be superceded or buttress clojure protocol.
(defgeneric -deref (obj))
(defmethod  -deref  (obj) obj)

(defun deref? (obj) (implements? #'-deref obj))
(defun deref (obj) (if (not (deref? obj))
                       obj (-deref obj)))

(defstruct (reduced-value
            (:constructor reduced (v)))
  v)

(defmethod -deref ((obj reduced-value))
  (reduced-value-v obj))

(defun reduced? (obj)
  (eq (type-of obj) 'reduced-value))

(defun seq-int-reduce (obj f)
  (block early
    (common-lisp:reduce (lambda (acc x)
                          (let ((res (funcall f acc x)))
                            (if (not (reduced? res))
                                res
                                (return-from early (deref res)))))
                        obj)))

(defun seq-init-reduce (obj f init)
  (block early
    (common-lisp:reduce (lambda (acc x)
                          (let ((res (funcall f acc x)))
                            (if (not (reduced? res))
                                res
                                (return-from early (deref res)))))
                        obj :initial-value init)))

(defmethod internal-reduce ((obj sequence) f)
  (seq-int-reduce obj f))

(defmethod init-reduce ((obj sequence) f init)
  (seq-init-reduce obj f init))

(defmethod internal-reduce ((obj common-lisp:cons) f)
  (seq-int-reduce obj f))

(defmethod init-reduce ((obj common-lisp:cons) f init)
  (seq-init-reduce obj f init))

(defmethod internal-reduce ((obj common-lisp:simple-vector) f)
  (seq-int-reduce obj f))

(defmethod init-reduce ((obj common-lisp:simple-vector) f init)
  (seq-init-reduce obj f init))

(defmethod internal-reduce ((obj common-lisp:array) f)
  (seq-int-reduce obj f))

(defmethod init-reduce ((obj common-lisp:array) f init)
  (seq-init-reduce obj f init))

(defmethod internal-reduce ((obj string) f)
  (seq-int-reduce obj f))

(defmethod init-reduce ((obj string) f init)
  (seq-init-reduce obj f init))

(defmethod internal-reduce ((obj LazySeq) f)
  (let ((init (first obj)))
    (loop with head = (rest obj)
          with acc  = init
          until (empty? head)
          do  (setf acc  (funcall f acc (first head)))
              (setf head (rest head))
          when  (reduced? acc)
              return (deref acc) 
          finally (return  acc))))

(defmethod init-reduce ((obj LazySeq) f init)
  (loop with head = obj
        with acc  = init
        until (empty? head)
        do  (setf acc (funcall f acc (first head)))
            (setf head (rest head))
        when (reduced? acc)
          return (deref acc)
        finally (return acc)))

;;default behavior is to coerce to seq.
(defmethod internal-reduce (obj f)
  (internal-reduce (seq obj) f))

(defmethod init-reduce  (obj f init)
  (init-reduce (seq obj) f init))


;;Some useful core functions.
;;this will get replaced by the clojure.core stuff,
;;but for now it'll be useful for bootstrapping.
;;Note: using reduce to implement a lot of stuff is great,
;;except that leveraging the extant common lisp reduce over
;;cl sequences also means we can't use early termination
;;criteria, is indicated by (reduced ..).  One (inefficient)
;;solution is to force everything to be a seq.  It'd be nice
;;if we could interop better, but I have no idea how
;;to stop a reduction in cl, unless we signal an error
;;intentionally.
(defun* reduce
    ((f coll)     
        (internal-reduce coll f))
    ((f init coll) 
        (init-reduce coll f init)))

(defun iterate
    (f init)
  "Produces a lazy sequence of results, where 
   f is applied repeatedly, first to init, then 
   to the result (f (f (f init)))"
  (let ((res (funcall f init)))   
    (when res
      (lazy-seq
       (cons init (iterate f res))))))

(defun every? (pred coll)
  (reduce (lambda (acc x)
            (let ((v (funcall pred x)))
              (if (and v acc)                  
                  t
                  (reduced nil)))) (funcall pred (first coll)) (rest  coll)))

(defun filter (pred coll)
  (when-let ((x  (first (seq coll))))
    (lazy-seq (cons x (filter pred (rest coll))))))

(defun* concat
  (() (lazy-seq nil))
  ((x) (lazy-seq x))
  ((x y)
   (lazy-seq
    (let ((s (seq x)))
      (if s
          (cons (first s) (concat (rest s) y))
          y))))
  ((x y &rest zs)
   (labels ((cat  (xys zs)
              (lazy-seq
               (let ((xys (seq xys)))
                 (if xys
                     (cons (first xys) (cat (rest xys) zs))
                     (when zs
                       (cat (first zs) (next zs))))))))
     (cat (concat x y) zs))))

;;for now...we listify this.
;;apply is eager.
(defun apply (f arg &rest args)
  (let ((arg (if (seq? arg)
                 (seq->list arg)
                 arg)))
    (common-lisp:apply f arg)))

(defun* map
    ((f coll)
     (lazy-seq
      (when-let ((s (seq coll)))
        (cons (funcall f (first s)) (map f (rest s))))))
  ((f c1 c2)
      (lazy-seq
       (let ((s1 (seq c1))
             (s2 (seq c2)))
         (when (and s1 s2)
           (cons (funcall f (first s1) (first s2))
                 (map f (rest s1) (rest s2)))))))
  ((f c1 c2 c3)
      (lazy-seq
       (let ((s1 (seq c1))
             (s2 (seq c2))
             (s3 (seq c3)))
         (when (and  s1 s2 s3)
           (cons (funcall f (first s1) (first s2) (first s3))
                 (map f (rest s1) (rest s2) (rest s3)))))))
  ((f c1 c2 c3 &rest colls)
   (let ((xs  (cons c1 (cons c2 (cons c3 colls)))))
     (labels ((stp (cs)                
                (let ((ss (seq  (map #'sequences::seq cs))))
                  (when (every? #'identity ss)                       
                    (lazy-seq
                     (cons (map #'sequences:first ss) (stp (map #'sequences:rest ss))))))))
       (map (lambda (xs) (apply f xs)) (stp xs))       
       ))))

(defun mapcat (f xs)
  (->> xs
       (apply #'concat)
       (map f)))

(defun filter (f coll)
  (labels ((aux (s)
	     (if-let ((x (first s)))
		     (if (funcall f x)
			 (lazy-cons x (aux (rest s)))
			 (aux (rest s))))))
    (aux (seq coll))))

(defun take (n coll)
  (labels ((aux (k s)
	     (when (> k 0)
	       (if-let ((res (first s)))
		       (lazy-cons res (aux (1- k) (rest s)))))))
    (aux n (seq coll))))

(defun take-while (pred coll)
  (labels ((aux (s)
	       (when-let ((res (first s)))
		       (when (funcall pred res)
			 (lazy-cons res (aux (rest s)))))))
    (aux (seq coll))))

(defun drop (n coll)
  (labels ((aux (k s)   
	     (if (> k 0)
		 (if-let ((res (first s)))
			 (aux (1- k) (rest s)))
		 s)))
    (aux n (seq coll))))

(defun drop-while (pred coll)
  (labels ((aux (s)   
	     (when-let ((res (first s)))
		     (if (funcall pred res) (aux (rest s)) s))))
    (aux (seq coll))))

(defun get-entry! (iter) 
  (multiple-value-bind (entry-p key value)  (funcall iter)
    (when entry-p (list (list key value)) iter)))

(defun lazy-entries (tbl)
  "Converts a hash-table into a lazy sequence of entries"
  (with-hash-table-iterator (my-iterator tbl)
    (let ((get-entry (lambda () (my-iterator))))
      (labels ((aux (f) 
		 (multiple-value-bind (entry-p key value)  (funcall f)
		   (if entry-p 
		       (lazy-cons (list key value)
				  (aux f))))))
	(aux get-entry)))))

(defmethod seq ((obj hash-table))
  (lazy-entries obj))

;;don't have recur implemented yet...
;; (defun* dorun
;;   ((coll) 
;;    (when-let (s (seq coll)) 
;;      (recur (next s))))
;;   ((n coll) 
;;    (when (and (seq coll) (pos? n))
;;      (recur (dec n) (next coll)))))

;(defun doall )

(defun* partition
    ((n offset coll)
     (when-let ((s (seq coll)))
       (lazy-seq 
        (cons (take n coll) (partition n offset (drop offset coll))))))
  ((n coll)
    (lazy-seq 
       (cons (take n coll) (partition n n (drop n coll))))))

(defun seq-count (s)
  (reduce #'+ (map (lambda (x) (declare (ignore x)) 1) s)))

;;Eager Sequence Functions, may be OBE
;;====================================

(defun flatten (expr)
  (labels ((aux (acc xs)
	     (if (atom xs) xs
		 (progn (dolist (x xs)
			  (if (atom x) (push x acc)
			      (let ((res (nreverse (aux (list) x))))
				(mapcar (lambda (x) (push x acc)) res))))
			acc))))
    (nreverse (aux (list) expr))))

(comment 
 (defgeneric take! (n l))
 (defmethod  take! (n (l cons))
   "Takes n elements from a list"
   (do ((remaining l (rest remaining))
        (acc (list))
        (i   n (decf i))) 
       ((or (= 0 i) (null remaining)) (nreverse acc))
     (push (first remaining) acc)))

 (defgeneric drop! (n l))
 (defmethod  drop! (n (l cons))
   "Drops the first n elements from a list"
   (do ((remaining l (rest remaining))
        (acc nil)
        (i n (decf i)))
       ((null remaining) acc)
     (when (zerop i) 
       (progn (setf acc (copy-list remaining))
              (setf remaining nil)))))

 (defun ndrop! (n l)
   "Drops the first n elements from a list.  Returns the sublist 
   of the inputlist, rather than accumulate a copy."
   (do ((remaining l)
        (i n (decf i))) 
       ((or (= 0 i) (null remaining)) remaining)
     (when (not (zerop i))
       (setf remaining (rest remaining)))))

 (defgeneric filter! (f l))
 (defmethod  filter! (f (l cons))
   "Returns a new list l, for all elements where 
   applications of f yield true."
   (do ((remaining l (rest remaining))
        (acc (list)))
       ((null remaining) (nreverse acc))
     (when (funcall f (first remaining))
       (push (first remaining) acc))))
 
 (defgeneric take-while! (f l))
 (defmethod  take-while! (f (l cons))
   "Draws elements from a list while f yields true.
   Returns the resulting list."
   (do ((remaining l (rest remaining))
        (acc (list)))
       ((null remaining) (nreverse acc))
     (if (funcall f (first remaining))
         (push (first remaining) acc)
         (setf remaining nil))))

 

 (defgeneric drop-while! (f l))
 (defmethod  drop-while! (f (l cons))
   "Draws elements from a list while f yields true.
   Returns the resulting list."
   (do ((remaining l (rest remaining))
        (acc (list)))
       ((null remaining) acc)
     (when (not (funcall f (first remaining)))
       (progn (setf acc (copy-list remaining))
              (setf remaining nil))))) 

 (defun ndrop-while! (f l)
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

 (defgeneric partition! (n l &key offset))
 (defmethod  partition! (n (l cons) &key (offset n))
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

 (defun partition-offset! (n offset l)
   "A form of partition, with adjustable offsetting
   that is friendly to the ->> threading macro."
   (partition n l :offset offset))

 (defgeneric interleave!  (xs ys))
 (defmethod  interleave! ((xs cons) (ys cons))
   "Returns a list composed of interwoven values drawn from
   input lists xs and ys.  Stops the interleaving process 
   when either list is exhausted."
   (do ((left xs  (rest left))
        (right ys (rest right))
        (acc nil))
       ((or (null left) (null right)) (nreverse acc))
     (progn 
       (push (first left) acc)
       (push (first right) acc))))
 )
