(defpackage :sequences
  (:use :common-lisp :common-utils)
  (:shadow :rest :map :filter)
  (:export  
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
   :iterate))

(defmacro lazy-cons (x y)
  "Creates a lazy cons-cell from x and y."
  `(cons ,x (delay ,y)))

;;at this point, x is 
;;(3 (some-func))
;;when we eval lazy-cdr, we can destructively 
;;swap out the cdr of the the cons cell with the 
;;newly realized (val thunk)) pair.

;;the only time we have lazy values is in the cdr.
;;As we advance the realization a step at a time, 
;;we can re-cons.
(defun lazy-rest (x)
  "Lazified version of cdr...again, since force 
   can handle eager values, this works for any 
   list."
  (when-let ((more (cdr x)))
    (if (promise? more) 
	(let ((res (force more)))
	  (progn (setf (cdr x)  res)
		 res))
	more)))

(defun seq? (x) (and (listp x) (and (cdr x) (promise? (cdr x)))))
;;an overloaded form of rest, akin to clojure.
(defun rest (xs) (seq? xs) (lazy-rest xs) (cdr xs))

;;general lazy sequence constructors.
(defgeneric seq (xs))

;;an abstract lazy sequence.  
;;This is meant to serve as an interface for anything that 
;;wants to act like a seq.  We stick it in s and pass all the 
;;sequence functions onto s.
(defstruct lazyseq count s pending)

(defmethod seq ((xs lazyseq)) xs)
(defun seq? (s) (= (type-of s) 'lazyseq))

(defun list->seq (lst) 
  (labels ((aux (xs)
 	     (when xs
	       (lazy-cons (first xs) (aux (cdr xs))))))
    (aux lst)))
	     
(defmethod  seq ((xs cons))
  (if (seq? xs) xs  (list->seq xs)))

(defun iterate (f init)
  "Produces a lazy sequence of results, where 
   f is applied repeatedly, first to init, then 
   to the result (f (f (f init)))"
  (let ((res (funcall f init)))
    (if res 
      (lazy-cons res (iterate f res))
      nil)))

(defun map (f seq)
  (labels ((step (s)
	     (if-let ((x (first s))) 
		     (lazy-cons (funcall f x) (step (rest s))))))

    (step seq)))

(defun filter (f seq)
  (labels ((step (s)
	     (if-let ((x (first s)))
		     (if (funcall f x)
			 (lazy-cons x (step (rest s)))
			 (step (rest s))))))
    (step seq)))

(defun take (n seq)
  (labels ((step (k seq)
	     (when (> k 0)
	       (if-let ((res (first seq)))
		       (lazy-cons res (step (1- k) (rest seq)))))))
    (step n seq)))

(defun take-while (pred seq)
  (labels ((step (seq)
	       (when-let ((res (first seq)))
		       (when (funcall pred res)
			 (lazy-cons res (step (lazy-cdr seq)))))))
    (step seq)))

(defun lazy-drop (n seq)
  (labels ((step (k seq)   
	     (if (> k 0)
		 (if-let ((res (first seq)))
			 (step (1- k) (rest seq)))
		 seq)))
    (step n seq)))

(defun drop-while (pred seq)
  (labels ((step (s)   
	     (when-let ((res (first s)))
		     (if (funcall pred res) (step (rest s)) s))))
    (step seq)))

(defun get-entry! (iter) 
  (multiple-value-bind (entry-p key value)  (funcall iter)
    (when entry-p (list (list key value)) iter)))

(defun lazy-entries (tbl)
  "Converts a hash-table into a lazy sequence of entries"
  (with-hash-table-iterator (my-iterator tbl)
    (let ((get-entry (lambda () (my-iterator))))
      (labels ((step (f) 
		 (multiple-value-bind (entry-p key value)  (funcall f)
		   (if entry-p 
		       (lazy-cons (list key value)
				  (step f))))))
	(step get-entry)))))



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
  (do ((left xs (rest left))
       (right ys (rest right))
       (acc nil))
      ((or (null left) (null right)) (nreverse acc))
    (progn 
      (push (first left) acc)
      (push (first right) acc))))