;;this is a simple set of utils I'd like to have around
;;to further my knowledge, I'll stick it in a package
;;using common lisp parlance.
(load "pvector2.lisp")

(defpackage :clojure
  (:use :common-lisp
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
	   :iterate))
;:export -> names of stuff to export.

(in-package :clojure)

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

(defgeneric take (n l))
(defmethod take (n (l cons))
  "Takes n elements from a list"
  (do ((remaining l (rest remaining))
       (acc (list))
       (i   n (decf i))) 
      ((or (= 0 i) (null remaining)) (nreverse acc))
      (push (first remaining) acc)))

(defgeneric drop (n l))
(defmethod drop (n (l cons))
  "Drops the first n elements from a list"
  (do ((remaining l (rest remaining))
       (acc nil)
       (i n (decf i)))
      ((null remaining) acc)
      (when (zerop i) 
	(progn (setf acc (copy-list remaining))
	       (setf remaining nil)))))

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

(defgeneric make-lazy (lst))
(defmethod  make-lazy ((lst cons))
  "Converts a normal list into a lazy list."
  (lazy (when lst
	  (cons (first lst) (make-lazy (rest lst))))))

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

  