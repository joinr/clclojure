;;Defining classes that can help us to wrap
;;built-in classes to allow things like
;;attaching meta to arbitrary objects...

;;this isn't that special....

;;storage and wrapper for our functions...
(defclass  function-object ()
  ((name   :initarg :name   :accessor function-object-name)
   (args   :initarg :args   :accessor function-object-args)
   (body   :initarg :body   :accessor function-object-body)
   (func   :accessor                  function-object-func)
   (meta   :initarg :meta   :accessor function-object-meta))
  (:metaclass sb-mop::funcallable-standard-class))

(defmethod initialize-instance :after ((f function-object) &key)
  (with-slots (name args body func) f
    (setf func
          (eval `(fn ,args
                     ,body)))
    ;;note: this is not portable...SBCL specific,
    ;;may be able to work around this with closer-mop.
    (sb-mop::set-funcallable-instance-function
     f func)))

(def f1 (make-instance 'function-object :name "plus"
                                        :meta  []
                                        :args '[x y]
                       :body '(+ x y)
                       ))

;;original idea for metadata, particularly
;;to capture argument 
;;this is a pretty terrible way to attach meta
;;to objects...
;;A better way would be to define object-wrappers
;;the inherit from the wrappee, providing
;;slots for meta and hash...

;;(defparameter *wrapped-meta* (make-hash-table ))

