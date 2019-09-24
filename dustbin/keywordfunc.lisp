(defpackage :clclojure.keywordfunc
  (:use  :common-lisp  ;:clclojure.base
         :common-utils)
  ;; (:shadowing-import-from :clclojure.base
  ;;  :deftype :let)
  (:export :keyfn? :key-accessor :->keyaccess :keyaccess-func :keyaccess-key :with-keyfn))
(in-package :clclojure.keywordfunc)

(defparameter keyfns (make-hash-table))
(defun keyfn? (k)
  (gethash k keyfns))

;;this is the general template for implementing
;;keyword access...
;; (defun :a (m)    (gethash :a m))
;; (defun (setf :a) (new-value m)
;;   (setf (gethash  :a m)
;;         new-value))

(defun key-accessor (k)
  (let ((m (gensym "map"))
        (v (gensym "newval")))
    `(progn (defun ,k (,m) (gethash ,k ,m))
            (defun (,'setf ,k) (,v ,m)
              (,'setf (,'gethash ,k ,m) ,v))
            ;(,'setf (gethash ,k keyfns) ,k)
            )))

;;for localized keyaccess, i.e. inside
;;lets and friends....
(defclass  keyaccess ()
  ((key    :initarg :key   :accessor keyaccess-key) 
   (func   :accessor                 keyaccess-func))
  (:metaclass sb-mop::funcallable-standard-class))

(defmethod initialize-instance :after ((obj keyaccess) &key)
  (with-slots (key func) obj    
    (setf func (lambda (ht)
                 (gethash  key ht)))
    (sb-mop::set-funcallable-instance-function
     obj func)
    (eval (key-accessor key))
    (setf (gethash key keyfns) obj)
    ))

;;keyaccessors print like keywords.
(defmethod print-object ((obj keyaccess) stream)
  (prin1 (keyaccess-key obj) stream))

(defun ->keyaccess (k)
  (or 
   (gethash k keyfns)
   (make-instance 'keyaccess :key k)))

;;now, to get the last step of "real" keyword access, we need to
;;detect when keyword literals used, and create keyword accessors for
;;them.  One dirty way of doing that, is to use a reader macro for
;;keywords, and ensure that every single keyword that's read has a
;;commensurate keyaccess obj created.

;;That's effective, maybe not efficient, since we're duplicating our
;;keywords everywhere.  A more efficient, but harder to implement,
;;technique is to macroexpand and walk the code inside a unified-let*.  In
;;theory, we can detect any forms used in the function position, and
;;if they're keywords, compile them into keyword accessors.

(defmacro with-keyfn (expr)
  (let ((k (first expr))
        )
    (if (keywordp k)
        (if  (not (keyfn? k))
             (progn (format nil "adding keyword access for: ~a " k )
                    (eval (key-accessor k))
                    `,expr))
        `,expr)))


;;dumb testing
;; (defparameter ht (make-hash-table))

;; (with-keyfn  (:a ht))
;; (with-keyfn  (:b ht))

;; (setf (:a ht)  :bilbo)
;; (setf  (:b ht) :baggins)

;; (with-keyfn  (:a ht))
;; (with-keyfn  (:b ht))


