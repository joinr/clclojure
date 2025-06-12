(defpackage :clj.base ;;might change this to clojure.lang at some point.
  (:use :common-lisp :common-utils
        :clclojure.pvector :clclojure.cowmap :clclojure.protocols)
  (:shadow :deftype :keyword))
(in-package clj.base)

(defun vector? (x) (typep x 'clclojure.pvector::pvec))

;;hacky way to accomodate both forms...
;;we know we're in clojure if the args are vector
;;we should allow deftype to implement generic functions directly too...
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
    `(let ((,ctor (clojure-deftype ,classname ,'() ,@implementations)))
       (funcall ,ctor))))

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
   (hasheq :initarg :hasheq :initform -1)))

(defclass CljKey ()
  ((ns     :initarg :ns :initform nil)
   (name   :initarg :name)
   (hasheq :initarg :hasheq)))

(defclass NameSpace ()
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
    (if ns 
        (format stream "~A/~A" ns name)
        (format stream "~A" name))))

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

(defmethod print-object ((obj Namespace) stream)
  (with-slots (name) obj
    (format stream "#<Namespace ~A>" name)))

(defprotocol ISymbolic
    (as-symbol (this)))

(defprotocol ISymbol
  (sym-name  (this))
  (sym-ns    (this)))

(extend-protocol
 ISymbol
 CljSymbol
 (sym-name (this) (slot-value this 'name))
 (sym-ns   (this) (slot-value this 'ns)))


(defun string->symbol (x)
  (let ((res  (uiop:split-string x :separator "/")))
    (if (second res)
        (make-instance 'CljSymbol :ns (first res) :name (second res))
        (make-instance 'CljSymbol :ns nil :name x))))

;;clojure allows symbols to convert to keys.
(extend-protocol
 ISymbolic
 String
 (as-symbol (this) (string->symbol this))
 CljSymbol
 (as-symbol (this) this))

;;We plan to maintain our own registry of namespaces
;;and keywords.  Ideally, we "could" try to inherit
;;from CL's stuff, but that route was already rough.
;;We'll just encapsulate our own stuff and figure out
;;how to interop later.

(defun* clj-symbol
  ((name)    (as-symbol name))
  ((ns name) (make-instance 'CljSymbol :name name :ns ns :meta nil)))

(defprotocol IObj
    (hashcode (this)))

(extend-protocol
 IObj
 T
 (hashcode (this) (sxhash this)))

(defprotocol IHasheq
    (hasheq (this)))

(extend-protocol
 IHasheq
 T
 (hasheq (this) (common-utils::hash-code this))
 CljSymbol
 (hasheq (this)
         (let ((hc (slot-value this 'hasheq)))
           (if (> hc -1)
               hc
               (let ((newc 
                       (common-utils::hash-code (list (sym-ns this) (sym-name this)))))
                 (setf (slot-value this 'hasheq) newc)
                 newc)))))

(defun symbol-equal (l r)
  (and (string-equal (sym-name l) (sym-name r))
       (string-equal (sym-ns l) (sym-ns  r))))

;;global registry of keywords.
;;really irresponsible for now, we just maintain
;;a hashmap.  later we'll use locking or concurrent
;;map to handle stuff better.

;;Also with cl's goofy handling of hash tables,
;;we'll leverage sbcl's extensions to provide custom tests.
;;We could leverage equiv here eventually.

(sb-ext:define-hash-table-test symbol-equal hashcode)

(defparameter *keys* (make-hash-table :test 'symbol-equal))

;;keywords are interned (cached) based on the symbol
;;symbols can have meta though, so we want them without meta.
(defun* clj-keyword
    ((name)    (make-instance 'CljKey :name name :ns nil))
    ((ns name) (make-instance 'CljKey :name name :ns ns)))

;;we're hand-waving concurrency and meta at the moment.
(defun intern-key (symb)
  (let ((res (gethash  symb *keys*)))
    (if res res
        (let ((kw (if (sym-ns symb)
                      (clj-keyword (sym-ns symb) (sym-name symb))
                      (clj-keyword (sym-name symb)))))
          (setf (gethash symb *keys*) kw)
          kw))))

(defun*  keyword
    ((name)    (intern-key (clj-symbol name)))
    ((name ns) (intern-key (clj-symbol name ns))))
