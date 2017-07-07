;;A package for useful library utilities that come up during the course
;;of implementing clclojure.
(defpackage :common-utils
  (:use :common-lisp)
  (:export  
   :comment 
   :make-keyword 
   :stringify 
   :str   
   :symb  
   :with-gensyms
   :flatten
   :map-tree
   :filter-tree 
   :reduce-tree
   :filter
   :if-let
   :when-let
   :zip 
   :->>
   :->
   :delay
   :force
   :promise?
   :realized?
   :lambda*
   :defconstant!
   :quote-sym
   :partition!
   :interleave!
   :partition-offset!
   :ndrop!
   :take!
   :drop!
   ))
(in-package :common-utils)


(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)

(defun quote-sym (sym) `(quote ,sym))
  
(defmacro defconstant! (symbol value)
  `(defconstant ,symbol 
     (or (and (boundp ',symbol) 
              (symbol-value ',symbol))
         ,value)))
;;I'm going to rewrite these around a little lazy cons cell data 
;;structure, probably...

(defstruct  promise value thunk)
(defun promise? (x) (eq (type-of x) 'promise))
(defgeneric realized? (x))
(defmethod  realized? ((x promise))
   (null (promise-thunk x)))

(defgeneric force (x))
(defmethod  force ((x promise))
  (if (realized? x) (promise-value x)       
      (let ((res (funcall (promise-thunk x))))
	(progn (setf (promise-value x) res)
	       (setf (promise-thunk x) nil)
	       res))))

;;This is exactly the same as lazy-seq in clojure, or fairly 
;;similar.  Should probably take a look at this guy...
(defmacro delay (body)  
  "Creates a lazy value from v, returning a thunk'd 
   function that, upon evaluation, caches the result."
    `(make-promise :thunk (lambda () ,body)))  

;;same as clojure's comment macro.
(defmacro comment (&rest xs))

;;Converts thing to a keyword representation.  Used in building library funcs.
(defun make-keyword (thing) 
  (values (intern (string-upcase thing) :keyword)))

;;Turn x into a string.  

(defun stringify (x) (format nil "~a" x))
(defun filter (pred xs) (remove-if-not pred xs))
;;some hack functions to help us validate bodies.
(defun group-by (key-func xs)
  (reduce (lambda (acc x) 
	    (let* ((k (funcall key-func x))
		   (vals (gethash k acc (list))))
	      (progn (push x vals)
		     (setf (gethash k acc) vals)
		     acc)))
	  xs :initial-value (make-hash-table)))  
;;this is a hack, will be replaced later.   Thanks common lisp cookbook!
;;I hate loop!
(defun hash-table->entries (tbl)
  (nreverse 
   (loop for key being the hash-keys of tbl
      using (hash-value value)
      collect (list key value))))
(defun str (x) (format nil "~A" x))

;;Courtesy of the Common Lisp Cookbook, thank you kind souls.
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
   is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos))) 

;;Turn xs, assumably strings, into a symbol as if typed at the repl.
(defun symb (&rest xs)
  (eval (read-from-string (concatenate 'string xs))))


;;Need to add with-gensyms here..
;;From PCL.

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym ,(stringify n))))
     ,@body))

(defmacro if-let (binding body &rest false-body)
  (let* ((binds (first binding))
	 (pred (first  binds))
	 (init (second binds)))    
    `(let ((,pred ,init))
       (if ,pred ,body ,@false-body))))

(defmacro when-let (binding body)
  (let* ((binds (first binding))
	 (pred (first  binds))
	 (init (second binds)))    
    `(let ((,pred ,init))
       (when ,pred ,body))))

;;Note -> CL has analogues for some of these built in.
;;Tree manipulators include sublis, subst, and friends.

;;hack.  I think Graham has a better one around somewhere.
(defun flatten (expr)
  (labels ((aux (acc xs)
	     (if (atom xs) xs
		 (progn (dolist (x xs)
			  (if (atom x) (push x acc)
			      (let ((res (nreverse (aux (list) x))))
				(mapcar (lambda (x) (push x acc)) res))))
			acc))))
    (nreverse (aux (list) expr))))



;;maps f to a list of lists, applying f to every leaf in the tree.
(defun map-tree (f tree)
  (labels ((aux (acc xs)
	     (if (null xs) (nreverse acc)
		 (let ((x (first xs)))
		   (if (atom x) 
		       (aux (push (funcall f x) acc) (rest xs))
		       (aux (push (aux (list) x) acc) (rest xs)))))))
  (aux (list) tree)))   
;;filters the leaves of a tree according to f.  An optional branching function
;;may be supplied, which given a list, returns a list of children.
(defun filter-tree (filter tree &key (branch-func #'identity))
  (labels ((aux (acc xs)
	     (if (null xs) (nreverse acc)
		 (let ((x (first xs)))
		   (if (atom x)
		       (aux (if (funcall filter x) (push x acc) acc) (rest xs))
		       (if-let ((children (funcall branch-func x)))
			       (aux (push (aux (list) children) acc) (rest xs))))))))
    (aux (list) tree)))

;; (defun reduce-tree (accum tree initial-value &key (branch-func #'identity) (finalize #'identity))
;;   (labels ((aux (acc xs)
;; 	     (if (null xs) (funcall finalize acc)
;; 		 (let ((x (first xs)))
;; 		   (if (atom x)
;; 		       (aux (funcall accum acc x) (rest xs))
;; 		       (if-let ((children (funcall branch-func x)))	       
;; 			       (aux (funcall accum acc (aux acc children)) (rest xs))			      
;; 			       (aux (funcall accum acc children) (rest xs))))))))
;;     (aux initial-value tree)))

;;This is a cop out.  Okay for small trees, and it's eager.
(defun reduce-tree (f initial-value tree &key (finalize #'identity))
  (funcall finalize (reduce f (flatten tree) :initial-value initial-value)))

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

;;Turn this into a generic function later for seqs.
(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

;;this is good enough for my limited purposes, but is going to be 
;;dogshit slow for large libs.
(defun keys (xs)
  (let ((the-keys (list)))
    (loop for key being the hash-keys of xs
       do (push key the-keys))
    the-keys))

;;Eager Sequence Functions, may be OBE
;;====================================
(comment  (defun flatten (expr)
            (labels ((aux (acc xs)
                       (if (atom xs) xs
                           (progn (dolist (x xs)
                                    (if (atom x) (push x acc)
                                        (let ((res (nreverse (aux (list) x))))
                                          (mapcar (lambda (x) (push x acc)) res))))
                                  acc))))
              (nreverse (aux (list) expr)))))

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
  (do ((remaining l (ndrop! offset remaining))
       (acc (list)))
      ((null remaining) (nreverse acc))
    (let ((nxt (take! n remaining)))
      (if (= (length nxt) n)
          (push nxt acc)
          (setf remaining nil)))))

(defun partition-offset! (n offset l)
  "A form of partition, with adjustable offsetting
   that is friendly to the ->> threading macro."
  (partition! n l :offset offset))

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


;;MetaProgramming Tooling
;;======================


;;One thing that's showing up are functions with multiple bodies, 
;;in which we dispatch on the args passed to the function.  Clojure
;;makes this style idiomatic, so we'd like to have it in CL and clclojure.

;;The most general subset is a macro that applies a function to some args 
;;and determines which body to evaluate based on the result of the dispatch 
;;function, which acts as a key in a case.

(defmacro with-dispatching-body (dispatch-func bodies &optional default-body)
  `(lambda (&rest args)
     (case (funcall ,dispatch-func args)
           ,@bodies
	   (otherwise ,(or default-body '(error 'no-matching-function))))))       


;;given an args list, the multi-form should then dispatch to one or more subforms, 
;;which are then evaluated.
;;So, multi-form is a macro...
;;(with-dispatching-body(lambda (&rest args) (count args)) 
;;    (0 (funcall case-0)
;;    (1 (apply   case-1 args))
;;    (2 (apply   case-2 args))
;;    (3 (apply   case-3 args))
;;    (otherwise (apply variadic-case args)))

;;the 90% case is that we're dispatching on the arity of arguments, and we'll 
;;be doing something with the args in the body.
;;we can then define a case-args for defmacro, which binds *args* in the body, and 
;;invokes the same dispatching body machinery.
(defmacro case-args (args dispatch-func dispatch-bindings)
  (with-gensyms (xs)
    `(let* ((,xs  ,args)
	    (*args* ,xs  ))
       (case (funcall ,dispatch-func ,args)
	 ,@dispatch-bindings))))

(define-condition no-matching-args (error) ((text :initarg :text :reader text)))
;;We can choose an execution body based on a bound set of args.  This will pay off 
;;for our multiple-function body implementation for both fns and generic functions.
(defmacro case-arg-count (args count-cases &optional variadic-case)
  (let ((sorted-cases (sort count-cases (lambda (xs ys) (< (car xs) (car ys))))))
    `(case-args ,args #'length
		,(append  sorted-cases 
			 `((otherwise ,(or variadic-case '(error 'no-matching-args))))))))	      

;;our specs for a mult-bodied lambda are like this:
;; (lambda*
;;    (()    :no-args)
;;    ((x)   (list :one x))
;;    ((x y) (+ x y))
;;    ((x y &rest others) (list x y (first others))))

;; (defparameter spec
;;    '((()    :no-args)
;;      ((x)   (list :one x))
;;      ((x y) (+ x y))
;;      ((x y &rest others) (list x y (first others)))))

(defun variadic? (x)   (member x '(& &rest &optional &key)))
(defun args-type (xs)  (values (length xs) (when xs (filter #'variadic? xs))))
(defun arg-bodies->dispatch-specs (arg-bodies)
  (mapcar (lambda (xs) 
	    (let ((args (first xs))
		  (body (second xs)))
	      (multiple-value-bind (n is-var) (args-type args)
		(list (or (when is-var :variadic) n) (list args body)))))
	  arg-bodies))
(defun spec->arity (spec) (first spec))
(defun spec->body (spec) (second (second spec)))
(defun spec->args (spec) (first (second spec)))
(defun spec->variadic? (spec) (third (second spec)))



(define-condition duplicate-arities (error) ((text :initarg :text :reader text)))

(defun get-arities (groups)
  (-> (mapcar (lambda (kv) (if (= (length (second kv)) 1)
			       (list (first kv) (second (first (second kv))))
			       (error 'duplicate-arities))) groups)
      (sort (lambda (xs ys) 
	      (let ((x (first xs))
		    (y (first ys)))
		(if (eq x :variadic) 'true 
		    (when (and (numberp x) (numberp y)) (< x y))))))))

(defun pull-out-variadic (arities)
  (if (eq (first (first arities)) :variadic)
      (list (rest arities) (second (first arities)))
      (list arities nil)))

;;given a set of dispatch specifications, ensure that no dupes exist in the keys. 
;;Also ensure that only one variadic implementation exists.  Returns a list of 
;;(case-bindings variadic-binding).

(defun parse-dispatch-specs (specs)
  (let* ((cases-variadic  (->> (arg-bodies->dispatch-specs specs)
			       (group-by (lambda (x) (or (when (spec->variadic?  x) :variadic) (first x))))
			       (hash-table->entries)
			       (get-arities)
			       (pull-out-variadic))))
      cases-variadic))

;;creates a lambda that dispatches based on args across multiple bodies, 
;;basically a composite lambda function.
;;Identify which args are variadic, ensure we only have one dispatch value
(defmacro lambda* (&rest args-bodies)
  (if (= (length args-bodies) 1)
       (let ((args-body (first args-bodies)))
 	`(lambda ,(first args-body) ,(second args-body))) ;regular lambda, no dispatch.
       (destructuring-bind (cases var) (parse-dispatch-specs args-bodies)
	 (let ((cases (mapcar (lambda (xs)
				(destructuring-bind (n (args body)) xs
				  (if (= n 0)
				      `(,n (funcall (lambda  nil ,body))) 
				      `(,n (apply (lambda  ,args ,body) *args*))))) cases))
	       (var  (when var `(apply (lambda ,(first var) ,(second var)) *args*))))
	   `(lambda (&rest args) 		    
	      (case-arg-count args ,cases ,var))))))

;;testing
(comment 
 (defparameter the-func
   (lambda* 
    (() 2)
    ((x) (+ x 1))
    ((x y) (+ x y))
    ((&rest xs) (reduce #'+ xs)))))

;; COMMON-UTILS> (funcall the-func 2)
;; 3
;; COMMON-UTILS> (funcall the-func )
;; 2
;; COMMON-UTILS> (funcall the-func 2 3)
;; 5
;; COMMON-UTILS> (funcall the-func 2 3 4 )
;; 9
;; COMMON-UTILS> (funcall the-func 2 3 4 111)
;; 120

;;we may really want this form...
  ;; (labels ((,name (&rest args) (case (dispatch args) 
  ;; 				 (0 (funcall  name0))
  ;; 				 (1 (apply    name1 args))
  ;; 				 (2 (apply    name2 args))
  ;; 				 (otherwise (apply name-var args)))))
  ;;   (lambda (&rest args)
  ;;     (apply ,name args)))
    


;;for instance, fn forms allow a name to be associated with a function, like 
;;(fn the-func [x] (if (< x 2) x (the-func (- x 2)))) 
;;If no name is provided, I assume clojure jins one up...
;;The consequence here, is that 
;;(let [f (fn the-func [x] 
;;           (if (< x 2) x (the-func (- x 2))))]
;;  (f 20))  

;;works fine, the-func, in its body, has a name bound to (lambda (x) (- x 2))
;;so, (fn test-fn [x] (if (< x 2) x (test-fn (- x 2))))
;;in common lisp is 
;;    (labels ((test-fn (x) (if (< x 2) x (test-fn (- x 2)))))
;;     test-fn)

;; (defparameter test-fn
;;   (labels ((test-fn (x) (if (< x 2) x 
;; 			     (progn (print `(:calling ,x))
;; 				    (test-fn (- x 2))))))
;;     (lambda (x) (test-fn x))))

;;a simple macro here is then 
(defmacro named-fn (name args body)
  (multiple-value-bind (arity variadic) (args-type args)
    (declare (ignore arity))
    (if (not variadic) 
	`(labels ((,name ,args ,body)
		  (recur ,args (,name ,@args)))
	   (lambda ,args (,name ,@args))))))
	;; `(labels ((,name ,args ,body)
	;; 	  (recur (&rest xs) (apply ,name xs)))
	;;    (lambda (&rest xs) (apply ,name xs))))))
       
;;base case works....
(defparameter test-fn2
  (named-fn test-fn (x) 
	    (if (< x 2) x 
		(progn (print `(:calling ,x))
		       (test-fn (- x 2))))))
;;recur works just fine....
(defparameter test-fn3
  (named-fn test-fn (x) 
	    (if (< x 2) x 
		(progn (print `(:calling ,x))
		       (recur (- x 2))))))
;;If we want to compose a named function from a set of cooperating 
;;named functions, then we can build a dispatching name, which is 
;;shared amongst the different implementations.
;(defmacro named-fn* (name specs)

;;say we have something like...
;; (fn test-fn ([x]    (+ x 1))
;;             ([x y]  (+ x y))
;; 	    ([& xs] (reduce #'+ xs)))
;;that's valid clojure.
;;we could say.
;; (named-fn test-fn (&rest args)
;;    (let ((test-fn1 (named-fn test-fn1 (x)   (+ x 1)))
;; 	 (test-fn2 (named-fn test-fn2 (x y) (+ x y)))
;; 	 (test-fn-var (named-fn test-fn3 (&rest xs) (reduce #'+ xs))))
;;      (case (count args)
;;        (1 (apply test-fn1 args))
;;        (2 (apply test-fn2 args))
;;        (otherwise (apply test-fn-var args)))))

(defun func-name (name arity)    (read-from-string (format nil "~A-~A" name arity)))
(defmacro named-fn* (name &rest args-bodies)
  (if (= (length args-bodies) 1)
       (let ((args-body (first args-bodies)))
 	`(named-fn ,name ,(first args-body) ,(second args-body))) ;regular named-fn, no dispatch.
       (destructuring-bind (cases var) (parse-dispatch-specs args-bodies)
	 (let* ((args (gensym "args"))
		(funcspecs (mapcar (lambda (xs)
				    (destructuring-bind (n (args body)) xs
				      (let* ((fname (func-name name n))
					     (fbody  `(named-fn ,fname ,args ,body)))
					(if (= n 0)					    
					    `(,n ,fname  ,fbody)
					    `(,n ,fname  ,fbody))))) cases))
		(varspec   (when var 
			     (let* ((fname (func-name name :variadic))
				    (fbody  `(named-fn ,fname ,(first var) ,(second var))))
			       `(:variadic ,fname ,fbody))))
		(specs     (if var (append funcspecs (list varspec)) funcspecs)))
	   `(named-fn ,name (,'&rest ,args) 
		      (let ,(mapcar (lambda (xs) `(,(second xs) ,(third xs))) specs)
			(case (length ,args)
			  ,@(mapcar (lambda (xs)  (let ((n (first xs))
							(name (second xs)))
						    (if (= n 0) 
							`(,n (funcall ,name))
							`(,n (apply ,name ,args))))) funcspecs)
			  (otherwise ,(if var  `(apply ,(second varspec) ,args)
					        `(error 'no-matching-args))))))))))

;;testing
;; (named-fn* test-fn 
;; 	   ((x)    (+ x 1))
;; 	   ((x y)  (+ x y)))
;	   ((& xs) (reduce #'+ xs)))

  

;;we can modify our named fns...
;;the recur name is used




;;this is the expression that matters for fn.
;;(fn name? [params* ] condition-map? exprs*)
;;(fn name? ([params* ] condition-map? exprs*)+)

;;what we'll do is implement a lower-level fn* that works on lists.
;;fn's job will be to enforce that clojure vectors are properly list-ified 

;;so a named function...

;;we want to do the same thing with macros...
  
	 

;;proof of concept.
;; (defun list-eater (xs) 
;;   (labels ((list-eater (x) (if (null (cdr x)) (first x) (list-eater (cdr x)))))
;;     (list-eater xs)))	 

;; (defun random-list (n)
;;   (loop for i from 1 to n
;;        collect i))



);End eval-when
