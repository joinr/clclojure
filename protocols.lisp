;;a simple implementation of clojure protocols, and deftype.
;;this will help with building libraries, particularly 
;;the seq libraries.

;;If we can bolt on a few fundamental operations, we can take 
;;advantage of the bulk of the excellent bootstrapped clojure 
;;defined in the clojurescript compiler.  
(defpackage :clclojure.protocols  
  (:use :common-lisp :common-utils)
  (:export :defprotocol
	   :extend-protocol
	   :satisfies?
	   :protocol-exists?
	   :list-protocols))

(in-package :clclojure.protocols)
;;protocols are used extensively, as is deftype.  There are a 
;;few additional data types that we need to provide.

;Protocol specifications take on the form below:
;(protocolname 
;  (function-name1 (args) &optional doc)
;   function-name2 (args) &optional doc))

;; (defparameter samplespec 
;;   '(ISeq
;;     (next (coll) 
;;      "Gets the next element from the sequence")
;;     (more (coll) 
;;      "Gets the rest of the sequence.")))

(defun spec-name (protocolspec)
  (car protocolspec))

(defun spec-functions (protocolspec)
  (remove-if-not #'listp protocolspec))

(defun function-names (protocolspec)
  (mapcar #'first (spec-functions protocolspec)))
	  
(defun make-satisfier (protocolspec)
  "From a list of function specs, builds a function that compares a
   list of function specs to ensure both specifications have the same 
   function names.  If no function specs are provided, the identity
   function is returned."
  (let ((names (function-names protocolspec)))
    (if (null names)
	(lambda (x) 
	  (declare (ignore x))
	  t)
	(lambda (newspec) 
	  (null (set-difference names (function-names newspec)))))))

;a sample implementation for ISeqs...
(defparameter sampleimp 
  '(ISeq 
    (next (coll) (car coll))
    (more (coll) (cdr coll))))

;A protocol is a name, a set of generic functions, and a set of
;types that implement the protocol.
(defstruct protocol name functions satisfier (members (list)))
(defun ->protocol (name functions &optional satisfier)
  (make-protocol :name name :functions functions :satisfier satisfier))

(defun protocol-to-spec (p)
  (with-slots (name functions) p
    (list name functions)))

;Debating whether to keep this around, 
;I may not need it...
(defparameter *protocols* 
  (make-hash-table :test #'eq))

;Probably deprecated soon...
(defun get-protocol (name)
  (gethash name *protocols*))

(defun add-protocol-member (pname membername)
  "Identifies membername as an implementor of protocol 
   pname"
  (multiple-value-bind (p exists?) (get-protocol pname)
    (when exists? 
      (push  membername (protocol-members p) ))))

;Probably deprecated soon...
(defun protocol-exists? (name)
  (not (null (get-protocol name))))

(defun drop-protocol (name) 
  "Eliminates any bindings to the quoted protocol name, 
   including generic functions."
  (if (protocol-exists? name)
      (let ((p (get-protocol name)))
	(progn	 (dolist (n (protocol-functions p))
		     (unintern n))
		 (remhash name *protocols*)))))

(defun list-protocols () 
  "Lists all known protocols."
  (loop for k being the hash-keys in *protocols* collect k))

(defun satisfies? (pname x)
  "Predicate to determine if protocol pname has an
   an implementation for objects of type x."
  (let ((p (get-protocol pname)))
    (not (null (find (type-of x) (protocol-members p))))))

(define-condition protocol-exists (error) 
  ((text :initarg :text :reader text)))

(define-condition malformed-protocol (error) 
  ((text :initarg :text :reader text)))

(define-condition missing-implementations (error) 
  ((text :initarg :text :reader text)))

(define-condition name-collision (error) 
  ((text :initarg :text :reader text)))


;when we add protocols, we just want to ensure that the 
;right generic functions are implemented.
;We let Common Lisp sort out whether the generic functions 
;are actually correct.
(defun add-protocol (p)
  (with-slots (name) p 
    (if (null (get-protocol name))
	(setf (gethash name *protocols*) p)
	(error 'protocol-exists))))

(defun build-generic (functionspec)
  (let ((docs (if (= (length functionspec) 3)
		  (third functionspec)
		  "Not Documented")))	
    `(defgeneric ,(first functionspec) ,(second functionspec)
       (:documentation ,docs))))

(defun quoted-names (xs)
  (mapcar (lambda (x) (list 'quote x))
	  (function-names xs)))
(defun spec-to-protocol (protocolspec)
  `(progn ,@(mapcar #'build-generic (spec-functions protocolspec))
	     (->protocol (quote ,(spec-name protocolspec))
			 (list ,@(mapcar (lambda (x) (list 'quote x)) (function-names protocolspec)))
			 (make-satisfier (quote ,protocolspec)))))

(defmacro defprotocol (name &rest functions)
  (let ((p (gensym))
	(spec (cons name functions)))
    `(handler-case 
	 (cond ((protocol-exists? (quote ,name))
		(error 'protocol-exists))
	       ;; ((null ,(list functions))
	       ;;  (error 'malformed-protocol))
	       ((boundp (quote ,name))
		(error 'name-collision))
	       (t
		(let ((,p (eval (spec-to-protocol (quote ,spec)))))
		  (progn (add-protocol ,p)))))
       (protocol-exists () ;(progn (drop-protocol (quote ,name))
				;  (defprotocol ,name ,functions)))
	                  (error 'protocol-exists))
       (name-collision () (error 'name-collision)))))

;; (defmacro defprotocol (name &rest functions)
;;   (let ((p (gensym))
;; 	(spec (cons name functions)))
;;     `(cond ((protocol-exists? (quote ,name))
;; 	    (error 'protocol-exists))
;; 	   ;; ((null ,(list functions))
;; 	   ;;  (error 'malformed-protocol))
;; 	   ((boundp (quote ,name))
;; 	    (error 'name-collision))
;; 	   (t
;; 	    (let ((,p (eval (spec-to-protocol (quote ,spec)))))
;; 	      (defvar ,name ,p))))))

;extends protocol defined by name to 
;each type in the typespecs, where typespecs 
;are of the form...
;(typename1 (func1 (x) (body))
;           (func2 (x) (body))           
; typename2 (func1 (x) (body))
;           (func2 (x) (body))) 
;bascially converts the implementations into a
;defmethod..
(defun parse-implementations (x)
  (labels ((get-spec (acc specs)
	     (if (null specs)
		 acc
		 (let ((arg (first specs)))
		   (if (symbolp arg)
		       (get-spec (cons (list arg) acc) (rest specs))
		       (let ((currentspec (first acc)))
			 (get-spec (cons (cons arg currentspec) (rest acc))
				   (rest specs))))))))
    (mapcar #'nreverse (get-spec (list) x))))

;; (defparameter samplext
;;   '(pvec 
;;     (next (coll) (nth coll 0))
;;     (more (coll) (subvec coll))
;;     cons
;;     (next (coll) (first coll))
;;     (more (coll) (rest  coll))))
		 
(defun implement-function (typename spec)
  (let* ((args (second spec))
	 (newargs (cons (list (first args) typename) (rest args)))
	 (body (third spec)))
    `(defmethod ,(first spec) ,newargs  ,body)))

(defmacro extend-protocol (name &rest typespecs)
  (let ((imps (parse-implementations typespecs))
	 (satisfies? (gensym)))
    `(let ((,satisfies? (protocol-satisfier (get-protocol (quote ,name)))))
       (dolist (imp (quote ,imps))
	 (if (funcall ,satisfies? imp)
	     (let ((typename (first imp)))
	       (progn (add-protocol-member (quote ,name)  typename)
		      (dolist (spec (rest imp))
			(eval (implement-function typename spec)))))
	     (error 'missing-implementation))))))	      


;Extend-type is also particularly useful.
;;Pending -> implement deftype.

(comment 
;Testing....
(defprotocol INamed 
    (get-name (thing) "gets the name of the thing!")
    (say-name (thing) "Says the name of the thing!"))

(extend-protocol 
 INamed 
 cons (get-name (thing) (car thing))
      (say-name (thing) (pprint (format nil "The name is: ~A" (get-name thing)))))

(defun test ()
  (let ((data '(:tom)))
    (when (satisfies? 'INamed data)
      (pprint (get-name data)))
      (say-name data)))
)

;;Deftype implementation.
;;Once we have protocols, deftype is pretty easy.
;;deftype is a hook into the type definition or object system of 
;;the host environment.  We'll use it to generate CLOS classes via
;;defclass.  I may include an option to use deftype to build structs
;;which would likely kick ass for performance.

;;A deftype form is pretty easy: 
;;(deftype name-of-type (field1 field2 ... fieldn)
;;   Protocol1
;;   (function1 (args) body1)
;;   Protocol2
;;   (function2 (args)  body2))
;;
;;Should expand into:
;;(progn
;;  (defclass name-of-type
;;    ((field1 :init-arg :field1)
;;     (field2 :init-arg :field2)))
;;  (extend-protocol Protocol1 name-of-type
;;     (function1 (args) body1))
;;  (extend-protocol Protool2 name-of-type
;;     (function2 (args) body2)))

(defun emit-class-field (s)
 `(,s :init-arg ,(make-keyword s)))

(defun emit-protocol-extension (proto-name type-name imps)
  `(extend-protocol ,proto-name ,type-name ,@imps)) 

;(defun emit-type-constructor (type-name fields)
;  `(defun (concatenate 'string "->"

;;Deftype exists in common lisp.  
(defmacro clojure-deftype (name fields &rest implementations)
  `(progn 
     (defclass ,name () ,fields 
       ,@(mapcar #'emit-class-field fields))
     ()))