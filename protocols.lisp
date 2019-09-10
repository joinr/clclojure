;;a simple implementation of clojure protocols, and deftype.
;;this will help with building libraries, particularly 
;;the seq libraries.

;;If we can bolt on a few fundamental operations, we can take 
;;advantage of the bulk of the excellent bootstrapped clojure 
;;defined in the clojurescript compiler.  
(defpackage :clclojure.protocols  
  (:use :common-lisp :common-utils :clclojure.reader :clclojure.pvector)
  (:export :defprotocol
           :extend-protocol
           :extend-type
	   :satisfies?
	   :protocol-exists?
           :list-protocols
           :clojure-deftype))

(in-package :clclojure.protocols)

;;aux
;;bootstrapping hack!
(defun vector? (x) (typep x 'clclojure.pvector::pvec))
(defun vector-expr (x)
  (and (listp x) (eq (first x) 'persistent-vector)))

;;this keeps args in order....we nreverse all over the place.
;;Since we prototyped using lists, and now the vector
;;reader is working well, we're in the middle of migrating
;;to vectors.  For now, we allow backwards compat with both
;;(perhaps allowing CL to define protocols in their native
;;tongue, I dunno).  In the future, we'll enforce
;;vectors....
(defun as-list (xs)
  (if (vector? xs) (nreverse  (vector-to-list xs))
      (if (vector-expr xs) (rest xs)
          xs)))

;;changed this since we have lists now...
(defun drop-literals (xs)
  (nreverse (filter (lambda (x) (not  (or (literal? x) (stringp x)))) (as-list  xs))))

;;Note-> we need to add support for variadic functions, 
;;and variadic protocols members.

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
  (remove-if-not   #'listp  (as-list  protocolspec)))

;;bombing out since we can't extend SEQUENCE to
;;our own types (thanks hyperspec!)
;;We can, however, enforce that one must use persistent
;;vectors....or....we can coerce the vectors to lists, which
;;are acceptable sequences....
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
(comment
 (defparameter sampleimp 
   '(ISeq 
     (next (coll) (car coll))
     (more (coll) (cdr coll)))))

;;From stack overflow.  It looks like the compiler needs a hint if we're 
;;defining struct/class literals and using them as constants.
(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  ;;A protocol is a name, a set of generic functions, and a set of
  ;;types that implement the protocol.
  (defstruct protocol name functions satisfier (members (list)))
  (defmethod make-load-form ((v protocol) &optional env)
    (declare (ignore env))
    (make-load-form-saving-slots v)))

(defun ->protocol (name functions &optional satisfier)
  (make-protocol :name name :functions functions :satisfier satisfier))

(defun protocol-to-spec (p)
  (with-slots (name functions) p
    (list name functions)))

;;We won't keep a central listing of protocols.  They'll be first class objects, 
;;as in Clojure on the JVM.

;Debating whether to keep this around, 
;I may not need it...
(defparameter *protocols* (make-hash-table :test #'eq))
;Probably deprecated soon...
(defun get-protocol (name)
  (gethash name *protocols*))
  
;;we can replace this using CLOS.  We just add a generic function that 
;;tells us if an object satisfies a protocol.  
;;like (satisfies-protocol? (protocol obj)) 
;;Since protocols are actual objects (structs in this case), we call 
;;(satisfies? the-protocol-obj the-obj)
;;which delegates to 
;;((get-slot 'satisfier the-protocol-obj) the-obj) 
;;So we let the protocol tell us if an object satisfies its protocol.

;;When we do defprotocol then, we add an implementation of 

(defun add-protocol-member (pname membername)
  "Identifies membername as an implementor of protocol 
   pname"
  (multiple-value-bind (p exists?) (get-protocol pname)
    (when exists? 
      (push  membername (protocol-members p)))))

;Probably deprecated soon...
(defun protocol-exists? (name)
  (not (null (get-protocol name))))

(defun drop-protocol (name) 
  "Eliminates any bindings to the quoted protocol name, 
   including generic functions."
  (if (protocol-exists? name)
      (let ((p (get-protocol name)))
	(progn	 (unintern (protocol-name p))
	         (dolist (n (protocol-functions p))
		     (unintern n))
		 (remhash name *protocols*)))))

(defun list-protocols () 
  "Lists all known protocols."
  (loop for k being the hash-keys in *protocols* collect k))

;;we should cache this....
(defgeneric satisfies? (p x))
(defmethod  satisfies? ((p protocol) x)
  (not (null (find (type-of x) (protocol-members p)))))

(defmethod  satisfies? ((p protocol) x)
  (not (null (find (type-of x) (protocol-members p)))))

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
    (if (null (get-protocol name)) (setf (gethash name *protocols*) p)
;       (error 'protocol-exists))))
	(progn (print (format nil "Overwriting existing protocol ~A" name))
	       (drop-protocol `(quote ,name))
	       (setf (gethash name *protocols*) p)))))

;;We need to add the multiple-dispatch function that's in bootstrap at the moment.
;;A multiple-body protocol implementation could be...

(defparameter proto-spec
  '(defprotocol ITough
      (get-toughness [x] [x y] 
       "gets the toughness of x, or if x is compared to y, the relative toughness"))) 

;;If we want to use generic functions to mirror single-dispatch implementations of 
;;protocols, we have to allow for multiple-body functions. 
;;So, there's probably a protocol dispatch function..
;;Just like our fn macro...



;;If we have multiple specs, [x] [x y], in this case, we need a generic function 
;;that dispatches based on the first arg of the spec.
;;Alternately....we can just use the fn body from before...
;;This only ever matters if there are multiple function bodies.  If there's only one, 
;;we're golden (that's the current situation).

;;note: dealing with reader-literals and how macros parse stuff, like pvectors,
;;so we're just filtering them out of arglists.

;; (defun build-generic (functionspec)
;;   (let* ((args (drop-literals (second functionspec)))
;;          (name (first functionspec))
;;         (docs (if (= (length functionspec) 3)
;; 		  (third functionspec)
;; 		  "Not Documented")))
;;     `(progn (defgeneric ,name  ,args (:documentation ,docs))
;;             ;;lets us use protocol fns as values...
;;             (defparameter ,name  (function ,name))
;;             (setf (symbol-function (quote ,name)) (symbol-value (quote ,name)))
;;             ,name)))

(defun build-generic (functionspec)
  (let* ((args (drop-literals (rest functionspec)))
         (name (first functionspec))
         (docs (if (stringp (last functionspec))
                   (last functionspec)
                   "Not Documented")))
    (case (length args)
      (1 (let ((args (drop-literals  (first args))))
           `(progn (defgeneric ,name  ,args (:documentation ,docs))
                   ;;lets us use protocol fns as values...
                   (defparameter ,name  (function ,name))
                   (setf (symbol-function (quote ,name)) (symbol-value (quote ,name)))
                   ,name)))
      (otherwise `(progn (defgeneric ,name (,'this ,'&rest ,'args) (:documentation ,docs))
                         ;;lets us use protocol fns as values...
                         (defparameter ,name  (function ,name))
                         (setf (symbol-function (quote ,name)) (symbol-value (quote ,name)))
                         ,name))
      )))

(defun quoted-names (xs)
  (mapcar (lambda (x) (list 'quote x))
	  (function-names xs)))

(defun spec-to-protocol (protocolspec)
  `(progn ,@(mapcar #'build-generic (spec-functions protocolspec))
	     (->protocol (quote ,(spec-name protocolspec))
			 (list ,@(mapcar (lambda (x) (list 'quote x)) (function-names protocolspec)))
			 (make-satisfier (quote ,protocolspec)))))

;;we'll have to update this guy later, but for now it's okay.
;;Added that a symbol gets created in the current package.
(defmacro defprotocol (name &rest functions)
  (let ((p (gensym))
	(spec (cons name functions)))
    `(let ((,p (eval (spec-to-protocol (quote ,spec)))))
       (progn (add-protocol ,p)
	      (defparameter ,name ,p)))))

;extends protocol defined by name to 
;each type in the typespecs, where typespecs 
;are of the form...
;(typename1 (func1 (x) (body))
;           (func2 (x) (body))           
; typename2 (func1 (x) (body))
;           (func2 (x) (body))) 
;bascially converts the implementations into a
                                        ;defmethod..
;;takes a list of
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
  (let* ((args    (cond ((vector? (second  spec))
                         (vector-to-list (second spec)))
                        ;this is a crappy hack.
                        ((vector-expr (second spec))
                         (rest (second spec)))
                        (t
                         (drop-literals (second spec)))))
	 (newargs (cons (list (first args) typename) (rest args)))
	 (body (third spec)))
    `(defmethod ,(first spec) ,newargs  ,body)))

;; (defmacro implement-function (typename spec)
;;   (let* ((args    (if (vector? (second  spec))
;;                       (vector-to-list (second spec))
;;                       (drop-literals (second spec))))
;; 	 (newargs (cons (list (first args) typename) (rest args)))
;; 	 (body (third spec)))
;;                                         ;(print spec)
;;     `(defmethod ,(first spec) ,newargs  ,body)))

(defmacro emit-method (name typename imp)
  `(progn (add-protocol-member (quote ,name)  (quote ,typename))
            ,@(mapcar (lambda (spec)                   
                        (implement-function typename spec))
                      (rest imp))))

(defun emit-implementation (name satvar imp)
  (let ((quoted-imp (gensym "quotedimp")))
    `(let ((,quoted-imp (quote ,imp)))
       (if (funcall ,satvar ,quoted-imp)
           (emit-method ,name ,(first imp) ,imp)
           (error 'missing-implementations (str `(,,name ,,quoted-imp)))))))
  
(defmacro extend-protocol (name &rest typespecs)
  (let ((imps       (parse-implementations typespecs))
        (satisfies? (gensym)))
    `(let ((,satisfies? (protocol-satisfier (get-protocol (quote ,name)))))
       ,@(mapcar  (lambda (imp) (emit-implementation name satisfies? imp))
                  imps))))

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
    (when (satisfies? INamed data)
      (pprint (get-name data)))
    (say-name data))

  (defmacro cljmacro (name argvec & body)
    (let ((args (if (vector? argvec argvec)
                    (eval `(clclojure.reader/quoted-children ,argvec)))))
      `(,@body)))
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

)

(defun symbolize (x) (read-from-string x))

(defun emit-class-field (nm s)
  `(,s :initarg ,(make-keyword s)
       :accessor ,(symbolize (str nm "-" s))))

(defun emit-protocol-extension (proto-name type-name imps)
  `(extend-protocol ,proto-name ,type-name ,@imps))

;;impl has protocol (pfn ...) (pfn ...)
(defmacro extend-type (typename  &rest impls)
  (let ((imps       (parse-implementations impls))
        ;(name       (gensym))
        ;(the-imp    (gensym))
        )
    `(progn ,@(mapcar (lambda (the-imp)
                  (let ((expr `(emit-protocol-extension (quote ,(first the-imp))
                                                        (quote ,typename)
                                                        (quote ,(rest the-imp)))))
                    (eval  expr)))
                
                imps))
      
         ;; (if (funcall ,satisfies? imp)             
         ;;     (progn (add-protocol-member (quote ,name)  typename)
         ;;            (dolist (spec (rest imp))
         ;;              (eval (implement-function ,typename spec))))
         ;;     (error 'missing-implementation))
             ))

;;the goal here is to define "instance-local" operations
;;at the method level, where fields refer to slots on the object.
;;so, we may have a object like {:a 2 :b 3}
;;fields [a b],
;;our implementations could be
;;(blah [obj] a) ;;undefined!
;;(blee [a] a) ;;defined, shadowing, poor form, but meh.

;;we need to extend the lexcial environment to include
;;field access...
;;(blah [obj] a) =>
;;(blah [obj]
;;  (with-slots ((a obj))
;     a))

;;we can be more efficient if we walk the implementations
;;to detect field usage.  For NOW, we'll just
;;bind all the fields in the lexical environment
;;of body, less the field names that are shadowed
;;by protocol args.

;;TODO: walk the body and collect fields to determine
;;the final set of fields to use (tailored).
(defun with-fields (fields method args &rest body)
  (let* ((arglist (as-list args))
         (var  (first arglist))
         (flds (set-difference (as-list fields) arglist)))
    ;;naive implementation is just bind all the slots....
    (if flds
        `(,method ,args
                  (with-slots ,flds ,var
                    ,@body))
        `(,method ,args ,@body))))

;;we need to mod this.  If the implementations refer to a field (and
;;the field is NOT shadowed as an argument to their method impl), we
;;need a call to with-slots to pull the referenced fields out to
;;mirror clojure's behavior.
(defmacro clojure-deftype (name fields &rest implementations)
  (let* ((flds (cond ((vector? fields) 
                     (vector-to-list fields))
                    ((vector-expr fields)
                     (rest fields))
                    (t 
                     fields)))
         (impls (mapcar (lambda (impl)                               
                          (if (atom impl) impl
                              (apply #'with-fields (cons flds impl)))) implementations)))
    `(progn 
       (defclass ,name ()
         ,(mapcar (lambda (f) (emit-class-field name f) ) flds))
       ;;we need to parse the implementations to provide
       ;;instance-level fields...
       (extend-type ,name ,@impls)
       ;;debugging 
       (defun ,(symbolize (str "->" name)) ,flds
         (make-instance ,`(quote  ,name) ,@(flatten  (mapcar (lambda (f) `(,(make-keyword f) ,f)) flds ))))
       
       )))


;;Deftype exists in common lisp.  
(comment 
 

 ;;An experimental class-bassed approach; putting this on ice for now.

 ;;This is our interface, which is a base class all protocols will 
 ;;derive from.
 ;; (defclass IProtocol () 
 ;;   (name 
 ;;    functions 
 ;;    satisfier 
 ;;    (members 
 ;;     :initform (list))))

 ;;Defining a protocol is just a matter of defining a new class that inherits 
 ;;from IProtocol.

 ;; (defmacro defprotocol-1 (name functions &optional satisifer)
 ;;  `(defclass ,name (IProtocol)
 ;;     ((name :initform ,name)
 ;;      (functions :initform functions)
 ;;      (
 ;;   )
 )
