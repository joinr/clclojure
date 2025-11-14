;;wherein we define some base classes and structs for our clojure
;;objects to inherit from.  This is for implementation help e.g. with
;;our legacy data structure implementations, e.g.  cowmap and
;;persistent vector structs etc.  We can treat these as superclasses
;;for our implementation level stuff and e.g. pack along metadata and
;;hasheq slots with our core datastructures.
(defpackage :clj-objects ;;might change this to clojure.lang at some point.
  (:use :common-lisp)
  (:export :make-cljstruct :cljclass :cljstruct :cljstruct-_meta :cljstruct-_hasheq :_meta :_hasheq))
(in-package :clj-objects)

(defstruct cljstruct (_meta nil) (_hasheq -1))
;;do we need these? hmm maybe.
(defclass  cljclass ()
  ((_meta
    :initarg :_meta
    :initform nil
    :accessor _meta)
   (_hasheq
    :initarg :_hasheq
    :initform -1
    :accessor _hasheq)))

;;note: we can emit funcallable objects pretty easily with
;;this recipe:

;; (defclass my-funcallable-object ()
;;   ((data :initarg :data :accessor data-of))
;;   (:metaclass sb-mop:funcallable-standard-class))

;; (defmethod initialize-instance  :after ((f my-funcallable-object ) &key)
;;   (sb-mop:set-funcallable-instance-function f (lambda (&rest xs) (apply #'+ xs))))
;; (defparameter f (make-instance 'my-funcallable-object :data 2))
;; (funcall f 1 2 3)
;; ;;6
;;  (setf (symbol-function 'f) f)
;;  (f 1 2 3)
;; ;;6

;;We can think of providing default implementations for the data structure classes,
;;keywords, namespaces, etc. if we want to have funcallability from CL.
;;This isn't a deal breaker - it's probably more useful if you want to have
;;clojure datastructure semantics in CL (e.g. funcall convenience).

;;would be nice to have iterators/iterables somewhere here.
;;so we can extend them to vectors and maps etc.
