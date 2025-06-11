(defpackage :clj.base ;;might change this to clojure.lang at some point.
  (:use :common-lisp :common-utils
        :clclojure.pvector :clclojure.cowmap :clclojure.protocols)
  (:shadow :deftype))
(in-package clj.base)

;;we want protocols early.
;;need some bootstrapping stuff.
;;for now though, we can just work on reader
;;and analyzer.

(defclass CljObj ()
  ((meta :initarg :meta :initform nil)))

(defclass  Var (CljObj)
  ((ns          :initarg :ns)
   (sym         :initarg :sym)
   (value       :initarg :value)
   (dynamic     :initarg :dynamic :initform nil)))

(defclass CljSymbol (CljObj)
  ((ns     :initarg :ns)
   (name   :initarg :name)
   (hasheq :initarg :hasheq)))

(defclass CljKey ()
  ((ns     :initarg :ns :initform nil)
   (name   :initarg :name)
   (hasheq :initarg :hasheq)))

(defclass Namespace ()
  ((name     :initarg :name)
   (aliases  :initarg :aliases)
   (mappings :initarg :mappings)))

;;naive eager version.
;; (defun print-seq (s &optional (stream t))
;;   "Generic vector printer."
;;   (format stream "(~{~s~^ ~})" (seq->list s)))

                                        ;extend printing to both pvecs and subvectors
(defmethod print-object ((obj CljSymbol) stream)
  (with-slots (ns name) obj
    (format stream "~A/~A" ns name)))

(defmethod print-object ((obj Var) stream)
  (with-slots (ns sym) obj
    (let ((outer ns))
      (with-slots (ns name) sym
        (format stream "#'~A/~A" (or outer ns) name)))))

(defmethod print-object ((obj CljKey) stream)
  (with-slots (ns name) obj
    (if ns 
        (format stream ":~A/~A" ns name)
        (format stream ":~A" name))))

(defun vector? (x) (typep x 'clclojure.pvector::pvec))


;;hacky way to accomodate both forms...
;;we know we're in clojure if the args are vector
(defmacro deftype (&rest args)
  (if (vector? (common-lisp:nth 1 args))
      `(clojure-deftype ,@args)
      `(common-lisp::deftype ,@args)))

;;reify is interesting.
;;we generate an instance of an anonymous class,
;;ala deftype, with protocol implementations.
;;TODO: look at the consequences of having bunches of
;;anonymous classes laying around, say evaluating
;;reify several times...Should we garbage collect this?
;;Or does that cut into dynamicity?

(defmacro reify (&rest implementations)
  (let ((classname (gentemp "REIFY"))
        (ctor (gensym "CONSTRUCTOR")))
    `(let ((,ctor (clojure-deftype ,classname (empty-vec) ,@implementations)))
       (funcall ,ctor))))
