;;A package for useful library utilities that come up during the course
;;of implementing clclojure.
(defpackage :common-utils
  (:use :common-lisp :cl-murmurhash)
  (:export  
   :comment
   :symbol?
   :symbol=
   :seql
   :scase 
   :make-keyword 
   :to-string
   :str   
   :symb  
   :with-gensyms
   :flatten
   :map-tree
   :filter-tree 
   :reduce-tree
   :filter
   :group-by
   :hash-code
   
   :hash-table->entries
   :hash-table->keys
   :hash-table->vals
   
   :if-let
   :when-let
   :when-not
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
   :defun*
   :nil?
   :odd?
   :even?
   :pos?
   :neg?
   :zero?
   :with-recur
   :detect-recur
   :tail-children
   :categorize-tails
   :summary-tails
   :with-env
   :named-fn
   :named-fn*
   :exception-info
   :try
   :nested-list?
   :vector-expr
   ))
(in-package :common-utils)


(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)

  ;;aux
  ;;bootstrapping hack!
 
  (defun vector-expr (x)
    (and (listp x) (eq (first x) 'persistent-vector)))

  ;;We bring this in now since we're ignoring literals.
  (defun literal? (x) nil)

  (defun nested-list? (x)
    (and x
         (listp x)
         (listp (first x))))

  (defun hash-code (obj) (cl-murmurhash:murmurhash obj))
  (defun quote-sym (sym) `(quote ,sym))
  ;;this keeps args in order....we nreverse all over the place.
  ;;Since we prototyped using lists, and now the vector
  ;;reader is working well, we're in the middle of migrating
  ;;to vectors.  For now, we allow backwards compat with both
  ;;(perhaps allowing CL to define protocols in their native
  ;;tongue, I dunno).  In the future, we'll enforce
  ;;vectors....
  ;;TBD replace with seq

(defun symbol= (l r)
  "Unqualified symbol equality based on symbol-name
   only, rather than compound equality of qualified symbols
   which incorporate name and package."
  (string= (symbol-name l) (symbol-name r)))

(defun symbol? (x)
  "Predicate to discriminate symbols, like symbolp, but
   also excludes keywordp values."
  (and (not (keywordp x)) (symbolp x)))

(defun seql (l r)
  "Defines unqualified symbol equality for l and r.
   If both l and r are symbol?, then checks equality
   using symbol=, else defaults to eql."
  (if (and (symbol? l) (symbol? r))
      (symbol= l r)
      (eql l r)))

(defmacro with-env (ename &rest body &environment env)
  `(let ((,ename ,env))
     ,@body))

(defmacro custom-case (test keyform cases)
  "CASE Test  Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key test to the value of
  Keyform. If a singleton key is T then the clause is a default clause." 
  (sb-impl::case-body 'case keyform cases t test nil nil nil))

(defmacro scase (keyform &rest cases)
  "Like case, but uses the more general seql for symbol-equality, which discriminates
   on the basis of keyword and symbol better.  Rather than symbol equality based on
   eql, compares symbol-name for equality.  does not permit keywords with same
   symbol name as symbol to be equal."
  `(custom-case ,'common-utils::seql ,keyform ,cases))

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
(defmacro comment (&rest xs)
  (declare (ignore xs)))

;;Converts thing to a keyword representation.  Used in building library funcs.
(defun make-keyword (thing) 
  (values (intern (string-upcase thing) :keyword)))


;;Turn x into a string.
;;Designed to support clojure's general behavior:
;; keyword string representation includes :
(defgeneric to-string (obj))
(defmethod to-string (obj)
  (princ-to-string obj))
(defmethod to-string ((obj string))
  obj)

(defun filter (pred xs) (remove-if-not pred xs))
;;some hack functions to help us validate bodies.
(defun group-by (key-func xs &key (test 'eql))
  (reduce (lambda (acc x) 
	    (let* ((k (funcall key-func x))
		   (vals (gethash k acc (list))))
	      (progn (push x vals)
		     (setf (gethash k acc) vals)
		     acc)))
	  xs :initial-value (make-hash-table :test test)))  
;;this is a hack, will be replaced later.   Thanks common lisp cookbook!
;;I hate loop!
(defun hash-table->entries (tbl)
  (nreverse 
   (loop for key being the hash-keys of tbl
         using (hash-value value)
         collect (list key value))))

(defun hash-table->keys (tbl)
  (loop for key being the hash-keys of tbl
        using (hash-value value)
        collect  key ))

(defun hash-table->vals (tbl)
  (loop for key being the hash-keys of tbl
        using (hash-value value)
        collect value))

;;https://stackoverflow.com/questions/26045442/copy-hash-table-in-lisp
;;josh taylor's answer

(defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table 
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
          using (hash-value value)
          do (setf (gethash key ht) value)
          finally (return ht))))

(defun str (x &rest xs)
  (format nil "~{~a~}" (mapcar #'to-string (cons x xs))))

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
  (read-from-string (apply #'str xs)))

;;Need to add with-gensyms here..
;;From PCL.

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym ,(to-string n))))
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

(defmacro when-not (pred body)
  `(when (not ,pred)
     ,body))

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

(defun walk-tree (f tree)
  (labels ((aux (acc xs)
	     (if (null xs) (nreverse acc)
		 (let ((x (first xs)))
		   (if (atom x) 
		       (aux (push (funcall f x) acc) (rest xs))
		       (aux (push (aux (list) x) acc) (rest xs)))))))
    (aux (list) tree))
  )
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
      `(-> (-> ,x ,form) ,@more)))

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

;;makes porting easier.
(defun nil? (obj) (null obj))
(defun pos? (n)   (plusp n))
(defun neg? (n)   (minusp n))
(defun odd? (n)   (oddp n))
(defun even? (n)  (evenp n))
(defun zero? (n)  (zerop n))



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
              (nreverse (aux (list) expr))))))

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
(defmethod partition! (n (l null) &key offset)
  '())

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
	    (args* ,xs  ))
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


(define-condition exception-info (error)
  ((message :initarg :message :reader exception-info-message)
   (data    :initarg :data    :reader exception-info-data)
   (cause   :initarg :cause   :reader exception-info-cause)))

(defmethod print-object ((obj exception-info) stream)
  (with-slots (message data cause) obj
    (format stream "#error~%{:type exception-info~%:message ~A~%:cause ~A~%:data ~A}" message cause data)))

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

(defun variadic? (x)   (member x '(& &rest &optional &key) :test #'seql))
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

;;https://common-lisp.net/project/bdb/qbook/mycl-util/api/function_005FMYCL-UTIL_003A_003AFIND-ANYWHERE.html
(defun find-anywhere (item tree)
  (cond ((eql item tree) tree)
	((atom tree) nil)
	((find-anywhere item (first tree)))
	((find-anywhere item (rest tree)))))

;;we can probably generalize this.
;;we can probably also detect if it's in the
;;tail position.
(defun find-recur (tree)
  (progn ;(pprint tree)
    (cond ((atom tree) nil)
          ((listp tree)
           (scase (first tree)
             (recur tree)
             ((fn loop lambda* lambda) nil)
             (t (cond ((find-recur (first tree)))
                      ((find-recur (rest tree))))))))))

;;naive way to explode the body and search for a lexical recur form
(defun detect-recur (body)
  (find-recur (sb-cltl2:macroexpand-all body)))

;; Tail positions and recur targets
;; ---------------------------------+-------------------------------------------------------------------+---------------
;;             Form(s)              |                            Tail position                          | Recur target?
;; ---------------------------------+-------------------------------------------------------------------+---------------
;; fn, defn                         | (fn [args] expressions tail)                                      | Yes
;; loop                             | (loop [bindings] expressions tail)                                | Yes
;; let, letfn, binding              | (let [bindings] expressions tail)                                 | No
;; do                               | (do expressions tail)                                             | No
;; if, if-not                       | (if test then-tailelse-tail)                                      | No
;; when, when-not                   | (when test expressions tail)                                      | No
;; cond                             | (cond test test tail ...:else else tail)                          | No
;; or, and                          | (or test test... tail)                                            | No
;; case                             | (case const const tail ... default tail)                          | No


;;sbcl 2.0 freaks out on this now, trying to generalize it.
;; (defun tail-children (expr)
;;   (when (listp expr)
;;     (scase (first expr)
;;       ((lambda let let* flet labels with-recur)         
;;          (destructuring-bind (l binds &rest body) expr                
;;            (destructuring-bind (tail &rest xs) (reverse body)
;;              (cons (list :tail tail) (mapcar (lambda (x) (list :non-tail x)) xs)))))
;;       (progn   (destructuring-bind (l &rest body) expr                
;;                  (destructuring-bind (tail &rest xs) (reverse body)
;;                    (cons (list :tail tail) (mapcar (lambda (x) (list :non-tail x)) xs)))))
;;       (if       (destructuring-bind (i pred l &optional r) expr
;;                   (cons (list :tail l) (when r (list (list :tail r))))))
;;       (when     (destructuring-bind (i pred l) expr
;;                   (list (list :tail l))))
;;       ((case ecase ccase)    (destructuring-bind (l binds) expr                
;;                                (mapcar (lambda (lr)
;;                                          (list :tail (second lr))) binds)))
;;       ((or and) (destructuring-bind (l &rest body) expr                
;;                   (destructuring-bind (tail &rest xs) (reverse body)
;;                     (cons (list :tail tail) (mapcar (lambda (x) (list :non-tail x)) xs)))) )
;;       (defun      (destructuring-bind (l nm binds  &rest body)
;;                       (if (and  (stringp (fourth expr)) (listp (fifth expr)))
;;                           `(,@(take! 3 expr) ,@(drop! 4 expr))
;;                           expr)
;;                     (destructuring-bind (tail &rest xs) (reverse body)
;;                       (cons (list :tail tail) (mapcar (lambda (x) (list :non-tail x)) xs)))))
;;       ;;(loop) ;;a lot like let....
;;       (otherwise  (mapcar (lambda (x) (list :non-tail x)) (rest expr))))))

(defun some-symbol (x xs)
  (some (lambda (y)  (seql x y)) xs))

(defun tail-children (expr)
  (when (listp expr)
    (let ((x (first expr)))
      (cond  ((some-symbol x '(lambda let let* flet labels with-recur))
              (destructuring-bind (l binds &rest body) expr                
                (destructuring-bind (tail &rest xs) (reverse body)
                  (cons (list :tail tail) (mapcar (lambda (x) (list :non-tail x)) xs)))))
             ((seql x 'progn)
              (destructuring-bind (l &rest body) expr                
                (destructuring-bind (tail &rest xs) (reverse body)
                  (cons (list :tail tail) (mapcar (lambda (x) (list :non-tail x)) xs)))))
             ((seql x 'if)
              (destructuring-bind (i pred l &optional r) expr
                (cons (list :tail l) (when r (list (list :tail r))))))
             ((seql x 'when)
              (destructuring-bind (i pred l) expr
                (list (list :tail l))))
             ((some-symbol x '(case ecase ccase))
              (destructuring-bind (l binds) expr                
                (mapcar (lambda (lr)
                          (list :tail (second lr))) binds)))
             ((some-symbol x  '(or and))
              (destructuring-bind (l &rest body) expr                
                (destructuring-bind (tail &rest xs) (reverse body)
                  (cons (list :tail tail) (mapcar (lambda (x) (list :non-tail x)) xs)))))
             ((seql x 'defun)
              (destructuring-bind (l nm binds  &rest body)
                  (if (and  (stringp (fourth expr)) (listp (fifth expr)))
                      `(,@(take! 3 expr) ,@(drop! 4 expr))
                      expr)                      
                (destructuring-bind (tail &rest xs) (reverse body)
                  (cons (list :tail tail) (mapcar (lambda (x) (list :non-tail x)) xs)))))
            ;;(loop) ;;a lot like let....
             (t  (mapcar (lambda (x) (list :non-tail x)) (rest expr)))))))

      
      ;;probably need a default case where all children are non-tail.
      ;;should add loop here..or at least loop/recur form.
      

;;given a root expr, all we have to do to ensure tail compliance is..
;;when there are tail-children, with categories :non-tail,
;;ensure that they don't contain recur.
(defstruct callsite kind expr)
(defun ->callsite (k e) (make-callsite :kind k :expr e))

(defun recur-call? (expr)
  (and  (listp expr) (eql (first expr) 'recur)))

;;we got the general idea here...
;;what we probably want to do is have a recursive routine that searches the call
;;graph.  We need to retain the state of the parent.  If the parent was a non-tail,
;;we cannot have recursive tail calls.  That's the invariant.
;;Much cleaner re-implementation of the code-walker using a basic graph search.
;;Note: this should be a DFS implementation, so leaves ought to be processed
;first.
(defun categorize-tails (expr)
  (labels ((aux (acc pending)
             (if-let ((nxt (first pending)))               
               (let* (;(blah  (pprint nxt))
                      (k          (first nxt))
                      (expr       (second nxt))
                      (pending    (append (rest pending) (tail-children expr)) ;(reduce (lambda (l x) (cons x l))  (tail-children expr) :initial-value (rest  pending) )
                                  ))
                 (cond (;; imediate invalid tail call
                        (and (eql k :non-tail) (recur-call? expr) )
                        (aux (cons acc (->callsite :illegal-recur expr))
                                      pending))
                       ((detect-recur expr) ;;maybe doing some extra work here but meh.
                        (aux (cons (->callsite
                                       (case k
                                         (:tail     :recur)
                                         (:non-tail :illegal-recur)) expr) acc) pending))
                       (t   (aux acc pending))))
               acc)))
    (aux '() (tail-children expr))))

;;we want 2 things: is there a recursive call?
;;is there an invalid tail call?
(defun summary-tails (expr)
  "Expands expression, macroexpanding the body and traversing looking for invocations of
   of 'recur, and illegal invocations from non-tail positions.  Returns a pair of
   (recur? illegals), where recur? is t|nil if any recur calls were encountered,
   and illegals is a list of all illegal recur callsites."
  (when-let ((xs (categorize-tails expr)))
    (reduce (lambda (acc x)
              (destructuring-bind (recur illegals) acc
                (case (callsite-kind x)
                  (:recur (list t illegals))
                  (:illegal-recur (list recur (cons  x illegals))))))
            (flatten  xs) :initial-value '(nil nil))))


(define-condition illegal-recur (error) 
  ((data :initarg :data :reader data))
  (:report (lambda (condition stream)
             (format stream "Detected one or more non-tail calls to (recur ..): ~a" (data condition)))))

(define-condition uneven-bindings (error) 
  ((data :initarg :data :reader data))
  (:report (lambda (condition stream)
             (format stream "Detected an uneven number of arguments: ~a" (data condition)))))

;;Beginnings of a foundation for loop/recur,
;;and automated (recur ..) constructs in functions.
;;Need to add support for a variadic form of recur.
;;Normal form is (with-recur (x init-x y init-y) ...)
;;We want to parse a single-binding version:

(defun strip-rest (xs)
  (filter (lambda (x)
            (not (seql x '&REST))) xs))

(defparameter *error-on-recur* nil)

(defmacro with-rest-recur (bindings &rest body)
  "helper macro with with-recur, admits a varargs version
   of recur that defines an &rest function version.
   Only one set of bindings is allowed...
   (with-rest-recur ((x &rest xs) (x &rest xs))
     body)"
  (let* ((e             `(,'with-recur ,bindings ,@body))
         (recur-illegals (summary-tails      e))
         (recurred?      (first recur-illegals))
         (illegals       (second recur-illegals))
         (pairs          (partition! 2 bindings))
         (bind          (first pairs))
         (lhs            (first bind))
         (rhs            (second bind)))
    (if (and recurred? illegals *error-on-recur*)
        (error 'illegal-recur   :data illegals)
        (when illegals (pprint (list :warning-possible-illegal-recur! illegals))))
    (cond ((not (= (length pairs) 1)) (error 'uneven-bindings :data bindings))
          ((not recurred?)               `(destructuring-bind ,lhs ,rhs ,@body))
          (t
           (let* ((args       (strip-rest  lhs))
                  (recur-sym  (intern "RECUR")) ;HAVE TO CAPITALIZE!
                  (res        (gensym "res"))
                  (continue?  (gensym "continuex"))
                  (recur-from (gentemp "recur-from"))
                  (recur-args (mapcar (lambda (x) (gensym (symbol-name x))) (strip-rest  args)))
                  (bindings   (mapcar (lambda (xy)
                                        `(setf ,(car xy) ,(cdr xy)))
                                      (filter (lambda (xy)
                                                (not (seql (first xy) '&REST)))
                                              (pairlis args recur-args)))))
             `(destructuring-bind ,lhs ,rhs
                (let ((,continue? t)
                      (,res)
                      ,@(mapcar (lambda (x) (list x x)) (strip-rest lhs)))
                  (flet ((,recur-sym ,recur-args
                           (progn ,@bindings
                                  (setf ,continue? t))))
                    (tagbody ,recur-from
                       (progn 
                         (setf ,res ,@body)
                         (when ,continue?
                           (setf ,continue? nil)
                           (go ,recur-from))))
                    ,res))))))))

(defmacro with-recur (bindings &rest body)
  "A form that acts akin to clojure's loop/recur, where given a
   sequence of (arg1 init arg2 init ...)  arguments bound to initial
   values, evaluates body in a form with a lexically defined function
   'recur', which has the same args as bindings, and effectively jumps
   back to the beginning of the recur point, carrying arguments
   forward, evaluating body again until a non-recur branch is
   evaluated.

   At compile time, expands body to determine if and instance of 
   recur is invoked.  Enforces tail-call semantics such that 
   recur cannot be used illegally in a non-tail position in body.
   If recur is not invoked, emits a simple let* form with bindings
   bound to lexical vars, and body as body."
  (let* ((e             `(,'with-recur ,bindings ,@body))
         (recur-illegals (summary-tails e))
         (recurred?      (first recur-illegals))
         (illegals       (second recur-illegals))
         (pairs          (partition! 2 bindings)))
    (if (and recurred? illegals *error-on-recur*)
        (error 'illegal-recur   :data illegals)
        (when illegals (pprint (list :warning-possible-illegal-recur! illegals))))
    (cond ((not (evenp (length bindings))) (error 'uneven-bindings :data bindings))
          ((not recurred?)                 `(let* ,pairs ,@body))
          (t
           (let* ((args       (mapcar (lambda (xy) (first xy)) pairs))
                  (recur-sym  (intern "RECUR")) ;HAVE TO CAPITALIZE!
                  (res        (gensym "res"))
                  (continue?  (gensym "continuex"))
                  (recur-from (gentemp "recur-from"))
                  (recur-args (mapcar (lambda (x) (gensym (symbol-name x))) args))         
                  (bindings   (mapcar (lambda (xy)
                                        `(setf ,(car xy) ,(cdr xy))) (pairlis args recur-args))))
             `(let ((,continue? t)
                    (,res)
                    ,@pairs)
                (flet ((,recur-sym ,recur-args
                         (progn ,@bindings
                                (setf ,continue? t))))
                  (tagbody ,recur-from
                     (progn 
                       (setf ,res ,@body)
                       (when ,continue?
                         (setf ,continue? nil)
                         (go ,recur-from))))
                  ,res)))))))

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
				      `(,n (apply (lambda  ,args ,body) args*))))) cases))
	       (var  (when var `(apply (lambda ,(first var) ,(second var)) args*))))
	   `(lambda (&rest args) 		    
	      (case-arg-count args ,cases ,var))))))

(defmacro defun* (name &rest args-bodies)
  (let ((func (gensym "func")))
    `(let ((,func (lambda* ,@args-bodies) ))
       (defun ,name (&rest ,'args) (apply ,func ,'args)))))

;;testing
(comment 
 (defparameter the-func
   (lambda* 
    (() 2)
    ((x) (+ x 1))
    ((x y) (+ x y))
    ((&rest xs) (reduce #'+ xs))))

)
 

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

(defun recur-fn-body (args body)
  (multiple-value-bind (arity variadic) (args-type args)
    (declare (ignore arity))
    (if (not variadic) 
        `(with-recur ,(flatten (mapcar (lambda (arg) `(,arg ,arg)) args))
           ,body)
        `(let ((,'init (list* ,@(strip-rest args))))
           (with-rest-recur (,args ,'init)
             ,body)))))

;;this version doesn't blow the stack.
(defmacro named-fn (name args body)
  (multiple-value-bind (arity variadic) (args-type args)
    (declare (ignore arity))
    (if (not variadic) 
        `(labels ((,name ,args
                    ,(recur-fn-body args body)))
           (function ,name))
        `(labels ((,name ,args
                    ,(recur-fn-body args body)))
           (function ,name)))))

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

;;So, we're getting a problem with forward-declaring the
;;"aux" function within macrolet.
;;It turns out we don't need to do this.
;;We should be able to to everything with labels...

;;We need 2 macros.
;;A recur dispatch
;;and a function dispatch.

;;Rather than evaling the named functions,
;;we can generate their labels definitions.

;;smarter way to do this is with macrolet.
(defun func-name (name arity)    (read-from-string (format nil "~A-~A" name arity)))
;;inline function bodies to ensure outer fn name can be called via labels.
(defmacro named-fn* (name &rest args-bodies)
  (if (= (length args-bodies) 1)
      (let ((args-body (first args-bodies)))
 	`(named-fn ,name ,(first args-body) ,(second args-body))) ;regular named-fn, no dispatch.
      (destructuring-bind (cases var) (parse-dispatch-specs args-bodies)
        (let* ((args (gensym "args"))
               (n-bodies (mapcar (lambda (xs)
				    (destructuring-bind (n (rargs body)) xs				                                            
                                      `(,n (destructuring-bind ,rargs ,args
                                             ,(recur-fn-body rargs body))))) cases))
               (varspec   (when var 
                            `(:variadic
                              (destructuring-bind ,(first var) ,args
                                ,(recur-fn-body (first var) (second var)))))))    
          `(labels ((,name (,'&rest ,args)
                     (case (length ,args)
                       ,@n-bodies
                       (otherwise ,(if var (second varspec)
                                       `(error 'no-matching-args))))))
            (function ,name))))))

;;printers
;;This is janky.  TBD: beter implementation (e.g. lazy seq...)
(defun print-map (m &optional (stream t))
  "Generic map printer."
  (format stream "{~{~s~^ ~}}"
    (nreverse
     (reduce (lambda (acc kv)
               (cons  (cadr kv) (cons (car kv) acc)))  (hash-table->entries m) :initial-value '()))))

;;Courtesy of Doug Hoyte, Let Over Lambda.
;;Precursor to custom gensyms..
;; (defmacro defmacro/g! (name args &rest body)
;;   (let ((syms (remove-duplicates
;;                (remove-if-not #'g!-symbol-p
;;                               (flatten body)))))
;;     `(defmacro ,name ,args
;;        (let ,(mapcar
;;               (lambda (s)
;;                 `(,s (gensym ,(subseq
;;                                (symbol-name s)
;;                                2))))
;;               syms)
;;          ,@body))))

;; try
;; (try expr* catch-clause* finally-clause?)
;; Special Form
;; catch-clause => (catch classname name expr*)
;; finally-clause => (finally expr*)

;; Catches and handles Java exceptions.

;; Please see http://clojure.org/special_forms#try
;; catch-clause => (catch classname name expr*)
;; finally-clause => (finally expr*)

;; Catches and handles Java exceptions.
(defmacro try (expr* catch-clause* &rest finally-clause?)
  (destructuring-bind (some-exception se recover) (rest  catch-clause*)
    (if finally-clause?
        (destructuring-bind ((f fbody)) finally-clause?
          (declare (ignore f))
          (let ((finally (gensym "finally"))
                (res     (gensym "res"))
                (err     (gensym "err")))
            `(let ((,res))
              (restart-case
                  (handler-case
                      ,expr*
                    (,some-exception (,se) (progn  (try ,recover
                                                        (catch t ,err (setf ,res ,err)))
                                                   (invoke-restart (quote ,finally)))))
                (,finally ()
                  (if ,res
                      (progn ,fbody
                             (error ,res))
                      ,fbody))))))
        `(handler-case
             ,expr*
           (,some-exception (,se) ,recover)))))

