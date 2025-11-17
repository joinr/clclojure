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
            (slot-specializer (sb-mop:intern-eql-specializer effective-name))
         (getter-lambda (lambda (obj-nm other)
                          (declare (ignore other))
                          (slot-value (first  obj-nm) effective-name)))
         (getter-method (make-instance 'standard-method
                                       :qualifiers nil
                                       :specializers (list klass  slot-specializer #-sbcl(find-class 'symbol))
                                       :lambda-list '(obj nm)
                                       :function getter-lambda))
         (setter #'set-slot)
         (setter-lambda (lambda (obj-nm-v other)
                          (declare (ignore other))
                          (destructuring-bind (obj nm v) obj-nm-v
                            (declare (ignore nm))
                            (setf  (slot-value obj effective-name) v))))
         (setter-method (make-instance 'standard-method
                                       :qualifiers nil
                                       :specializers (list klass slot-specializer #-sbcl (find-class 'symbol) root-class)
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

;;the basic idea here is to try to get a normal
;;slot-value if a symbol is provided, and then
;;if we fail, we do the more involved search for slots,
;;cache the effective slot name (if a single one exists),
;;then emit a getter and setter for the object symbol 
(defmethod slot ((obj t) (nm symbol))
  (handler-case  (slot-value t nm)
    (simple-error (c)
      (declare (ignore c))
      (let* ((the-slot           (find-slot obj nm))
             (effective-name (sb-pcl:slot-definition-name the-slot))
             (res (slot-value obj effective-name)))
        (emit-methods (class-of obj) effective-name)
        res))))

(defmethod set-slot ((obj t) (nm symbol) v)
  (handler-case  (setf  (slot-value t nm) v)
    (simple-error (c)
      (declare  (ignore c))
      (let* ((the-slot           (find-slot obj nm))
               (effective-name (sb-pcl:slot-definition-name the-slot))
               (res (setf  (slot-value obj effective-name) v)))
        (emit-methods (class-of obj) effective-name)
        res))))

(defsetf slot set-slot)

;;sb-int has some useful stuff like keywordicate and symbolicate...
;;we can introspect an object's fields using the mop facilities.
(defun slots (obj)
  (mapcar (lambda (slt) (sb-int:symbolicate (sb-pcl:slot-definition-name slt))) 
          (sb-mop:class-slots (class-of obj))))

(defun qualified-slots (obj)
  (mapcar (lambda (slt)  (sb-pcl:slot-definition-name slt)) 
          (sb-mop:class-slots (class-of obj))))

;;we could define lenses on slots in the future....hmmmm.
;;these could be actual places instead of just lists.
;;possibly useful for now though. we can get something akin to a BEAN or map
;;abstraction in the future.
(defun slot-entries (obj)
  (mapcar
   (lambda (v)
     (list v  (slot obj v)))
   (slots obj)))

(defun qualified-slot-entries (obj)
  (mapcar
   (lambda (v)
     (list v  (slot obj v)))
   (qualified-slots obj)))
