(ql:quickload :clclojure)
(defpackage :clclojure.variadic
  (:use  :common-lisp :clclojure.base)
  (:shadowing-import-from :clclojure.base
   :deftype :let))
(in-package :clclojure.variadic)

;;we need to determine the arity of the function...
;;For single arity, current approach works fine.
;;For multiple/variadic...
;;We need more information.

;;Perhaps a higher level generic function?

;;two-layers of type specialization.

;;Generic function dispatches on
;;arg-count, type of first arg
;;  (manytest 1)
;;     invokes specialization
;;        (many :: manytest args)
;;  (manytest 2)

;;protocol -> one or more proto functions.
;;  protocol :: proto-fn*
;;    proto-fn:: proto-method+
;;      proto-method :: proto-body+
;;         proto-body :: type -> args -> something


(arities? proto-fn) ;;should be able to determine concrete arities...
;;functions are registered, along with their arities somewhere...
;;if more than one arity, need a dispatch-fn.

;;dumb solution::
(defun generic-dispatch (obj  &rest args)
  (case (count args)
    0 (0-arity obj)
    1 (apply 1-arity obj args)
    2 (apply 2-arity obj args)
    ;;variadic?
    (apply   variadic obj ,@args)
    ))


(def sat? (clclojure.protocols::protocol-satisfier IMany))

(def spec
    '(IMANY (MANY [OBJ]
             [OBJ MSG])))

;;our satisfier works....
;;detects no set difference in the functions implemented.
;;bet we have 
(def newspec
    '(IMANY (MANY [THIS] :SINGLE)
      (MANY [THIS THAT] THAT)))
[many
{0         many-1
1         many-2
2         many-3
:variadic many-v } ]

(defgeneric many (obj &rest args))


;;need to look at recur as well..
;;need a named lambda.
;;in the case of recur, the recur point is
;;the named lambda.
;;We make the function name available....
(let ((many-1 (lambda (this)      :single))
      (many-2 (lambda (this that) that )))
  (defmethod many ((obj some-type) &rest args)
    (case (count args)
      ;;only valid cases
      1  (apply #'many-1 args)  
      2  (apply #'many-2 args)
      ;;else we ditch if there's not a variadic form!
      )
    ))

(generic-fn* many
             ((this) ;;1
                     (this that) ;;2
                     (this that else &rest args) ;;:variadic
                     ))


;;one quick and dirty way to track information
;;about our generic function, without having to
;;create a wrapper class, is to maintain
;;a registry of info...
(defun qualified-name (s)
  (common-utils::symb  (str  (package-name *package*) "/" (symbol-name s))))
(defparameter *metabase* (make-hash-table :test 'eq))

;;register the generic-fn
(defun push-meta! (name meta)
  (let ((k (qualified-name name)))
    (setf (gethash k *metabase*) meta)))

(defun get-meta! (name)
  (gethash  (qualified-name name)  *metabase*))

(defun compare-arity (l r)
  (let ((lv (find '& l))
        (rv (find '& r))))
  (if (not (and lv rv))
      (< (length l) (length r))
      (case )
      ))

(defun validate-arglists (arglists)
  
  )
(defmacro generic-fn* (name &rest args)
  (let ((gf (gensym "genfun")))
    `(let ((,gf (defgeneric ,name   (,'obj &rest ,'args))))
       )))
   
  

;;constraints:
;;only one variadic body
;;discrete args must be > non-variadic definitions...


;;a, generic function with n bodies.
;;we need to track that information?
;;internal implementation detail....

(generic-methods*
 many            ;gen-fn name 
 some-type
 ((this)         :single)
 ((this that)    that)
 ((this that else &rest args) else))


(defmacro generic-methods* (name specializer &rest methods)
  (let ((obj (gensym "obj"))
        (args (gensym "args"))
        )
    `(let ((many-1 (lambda (this)      :single))
           (many-2 (lambda (this that) that )))
       (defmethod ,name ((,obj ,specializer) ,'&rest ,args)
         (case (count ,args)
           ;;only valid cases
           1  (apply #'many-1 args)  
           2  (apply #'many-2 args)
           ;;else we ditch if there's not a variadic form!
           )
         )))

  )


;;moved from base....

;;Experimentation with function objects...
;;These may be more desireable than the symbol + lambda
;;approach I've been taking, since we can pack info
;;onto the slots...

(comment  ;;a function object...
         (defclass  fob ()
           ((name   :initarg :name   :accessor fob-name)
            (args   :initarg :args   :accessor fob-args)
            (body   :initarg :body   :accessor fob-body)
            (func   :accessor                  fob-func)
            (meta   :initarg :meta   :accessor fob-meta))
           (:metaclass sb-mop::funcallable-standard-class))

         (defparameter spec nil)
         (defmethod initialize-instance :after ((f fob) &key)
           (with-slots (name args body func) f
             (let ((argvec args ;(apply #'persistent-vector args)
                           ))
               (setf spec (list argvec body))
               (setf func
                     (eval `(fn ,argvec 
                                ,body)))
               (sb-mop::set-funcallable-instance-function
                f func))))

         (setq f1 (make-instance 'fob :name "plus"
                                      :meta  []
                                      :args '[x y]
                                 :body '(+ x y)
                                 )))

(comment 

 
 (defclass constructor ()
   ((name   :initarg :name   :accessor constructor-name)
    (fields :initarg :fields :accessor constructor-fields))
   (:metaclass sb-mop::funcallable-standard-class))


 (setq c1 (make-instance 'constructor
                         :name 'position :fields '(x y))))


(comment
 ;;clojure-like let...
 (eval  `(let* ((,'f ,(fn [k] (+ k 1)))
                (,'n 2))
           (declare (special ,'f))
           (unify-values ,'f)
           (,'f ,'n)))

 ;;we have to declare vars special to use them in
 ;;a let context ala clojure, so that we can unify
 ;;the symbols.

 (defun specials (vars)
   `(,@(mapcar (lambda (v)
                 `(declare (special ,v))) vars)))

 ;;Our let macro will just defer to this.... 
 (defmacro clj-let (binds &rest body)
   (let ((bs (vector-to-list binds))
         (vars (mapcar )))))
 )
