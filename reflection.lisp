;;utilities for generic introspection and
;;slot access.
(defpackage :reflection
  (:use :common-lisp))
(in-package :reflection)

;;define a generic method to "get a slot" regardless
;;of symbol-package for slotname nm.
(defgeneric slot (obj nm))
(defgeneric set-slot (obj nm v))

(define-condition slot-reflection-error (error)
  ((message :initarg :message :reader exception-info-message)
   (data   :initarg :data    :reader exception-info-data)
   (cause    :initarg :cause   :reader exception-info-cause)))

(defmethod print-object ((obj slot-reflection-error) stream)
  (with-slots (message data cause) obj
    (format stream "#error~%{:type slot-reflection-error~%:message ~A~%:cause ~A~%:data ~A}" message cause data)))

;; (defgeneric list-slots (obj))
;; (defmethod list-slots ((obj standard-class))
;;   (sb-mop:class-slots obj))
;; (defmethod list-slots ((obj structure-class))
;;   #-sbcl
;;   (sb-pcl::structure-type-slot-description-list  (type-of obj))
;;   (sb-mop:class-slots obj))

;; (defgeneric slot-name (obj))
;; (defmethod slot-name ((obj sb-kernel:defstruct-slot-description))
;;   (slot-value obj 'sb-kernel::name))
;; (defmethod slot-name ((obj sb-pcl:slot-definition))
;;   (sb-pcl:slot-definition-name obj))

;;use the MOP (or the pcl package) to find all direct slots
;;where the symbol possibly matches the slotname.
(defun possible-slots (symbol klass)
  (cl:remove-if-not
   (lambda (x) (string-equal x symbol))
   (sb-mop:class-slots klass)
   :key #'sb-mop:slot-definition-name))

;;#<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>
(defparameter root-class (second  (sb-mop:class-precedence-list
                                (class-of 'x))))
;;either find a single slot that could match
;;the slot symbol's symbol name, or report
;;error with ambiguous matches.
(defun find-slot (obj symbol)
  (let ((res nil)
        (klass (class-of obj)))
    (loop for slot in (sb-mop:class-slots klass )
          do (let ((sname (sb-mop:slot-definition-name slot)))
               (if (string-equal symbol sname)
                   (if res (cl:error 'slot-reflection-error
                                     :data (list :class klass
                                                 :slot symbol
                                                 :sname sname
                                                 :possibles (possible-slots symbol klass))
                                     :cause "ambiguous slot name for reflection"
                                     :message "multiple possible slots")
                       (setf res slot)))))
    res))
;;programmatically emit 2 methods for the slot generic function
;;that allow us to direct generic symbols to a single slot lookup,
;;or dispatch an exact symbol (not sure which is faster, so I added both
;;for now).  Might be able to elide the eql one if it doesn't matter.

;;NOTE: for some reason, there's a mismatch with the method
;;lambda list and the function lambda; like the args for
;;the fn get packed into a pair, with some other arg I don't get.
(defun emit-methods (klass effective-name)
  (let* ((getter #'slot)
         (getter-lambda (lambda (obj-nm other)
                          (declare (ignore other))
                          (slot-value (first  obj-nm) effective-name)))
         (getter-method (make-instance 'standard-method
                                       :qualifiers nil
                                       :specializers (list klass (find-class 'symbol))
                                       :lambda-list '(obj nm)
                                       :function getter-lambda))
         (setter #'set-slot)
         (setter-lambda (lambda (obj-nm-v other)
                          (declare (ignore other))
                          (destructuring-bind (obj nm v) obj-nm-v
                            ;;(declare (ignore nm))
                            (setf  (slot-value obj effective-name) v))))
         (setter-method (make-instance 'standard-method
                                       :qualifiers nil
                                       :specializers (list klass (find-class 'symbol) root-class)
                                       :lambda-list '(obj nm v)
                                       :function setter-lambda))
         #-sbcl
         (exact-method (make-instance 'standard-method
                                      :qualifiers nil
                                      :specializers (list klass (sb-mop:intern-eql-specializer effective-name))
                                      :lambda-list x'(obj nm)
                                      :function method-lambda)))
    #-sbcl
    (add-method gf exact-method)
    (add-method getter getter-method)
    (add-method setter setter-method)))

;;have intermediary for the mapping
;;storing effective-name etc.
;;this would allow us to setf based on slot.
;;right now we can only read.

(defmethod slot ((obj t) (nm symbol))
  (handler-case  (slot-value t nm)
    (simple-error (c)
      (let* ((the-slot           (find-slot obj nm))
             (effective-name (sb-pcl:slot-definition-name the-slot))
             (res (slot-value obj effective-name)))
        (emit-methods (class-of obj) effective-name)
        res))))

(defsetf slot set-slot)

#-sbcl
(defun find-slot (symbol klass)
(cl:find 'test-fun
         (sb-pcl:class-slots
          (find-class 'hash-table))
         :key #'sb-pcl:slot-definition-name
         :test #'string-equal))
