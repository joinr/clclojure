(defpackage :clclojure.base ;;might change this to clojure.lang at some point.
  (:use :common-lisp :common-utils
   :clclojure.pvector :clclojure.cowmap :clclojure.protocols
   :clclojure.lexical :clj-con
   :parse-float)
  ;;todo, migrate these to shadowing-import-from.
  (:shadow :deftype :keyword :atom :realized? :deref :char :str
   :let :defmacro :map :reduce :first :rest :second :dotimes :nth :cons :count :do :get :assoc :when-let
   :vector :odd? :even? :zero? :identity :filter :loop :if-let :throw :list* :cond := ;:defmethod
   :some :merge :pop :step :apply :case :class :satisfies? :set) ;;forgot about shadowing-import-from....
  ;;(:shadowing-import-from :sequences x:apply)
  (:shadowing-import-from :clj-re :re-find :re-groups :re-matcher :re-matches :re-pattern :re-seq)
  (:local-nicknames
       (:re :clj-re)
       (:mbind :metabang-bind)
       (:parse :clj-parse)
       (:clj-objects :clj-objects))
  (:export :apply :def :defn :fn :meta :with-meta :str :symbol? :instance? :first :rest :second :next :char
   :deftype :defprotocol :reify :extend-type :nil? :identical?
   :extend-protocol :let :into :take :drop :filter :seq :vec :empty :conj :concat :map :reduce :dotimes :nth :cons :count
   :do :get :assoc :when-let   :if-let :ns :even? :pos? :zero? :odd? :vector :hash-map :inc :dec :identity :loop  :chunk-first
   :doall  :chunk-buffer :every? :chunk-rest :interleave :ffirst :partition :seq->list :fnext :chunk-cons :nthrest
   :dorun  :chunked-seq? :->iterator :chunk-append :throw :ex-info :ex-cause :ex-message :ex-data :list* :cond :try := :true :false
   :defmulti :defmethod-clj :isa? :equiv :nnext :dissoc :implements? :partition-all :name :keyword? :val :key :when-not
   ;;mostly (except atom) from clj-con 
   :atom :atom? :compare-and-set! :deliver :deref :future :future-call :future-cancel :future-cancelled? :future-done? :future?           
   :promise :realized? :reset! :reset-vals! :swap! :swap-vals! :ex-info :throw :defrecord :pr-writer
   :keyword? :symbol? :string? :vector? :list? :map? :number? :aget :aset :set! :some :merge :disj :subs :object-array :update :update-in :declare-clj :frequencies :set? :seq? :repeat :hash-set :juxt :seqable? :interpose
   :partial :list? :cond :peek :pop :re-find :re-groups :re-matcher :re-matches :re-pattern :re-seq :parse-float :== :case :transient :persistent! :char? :sequencep :slurp :binding :satisfies? :extends? :extenders :class :supers
   :bases :class? :namespace :->string-builder :lazy-seq :empty? :counted? :take-nth :keys :vals :set :doto))
(in-package clclojure.base)


;;for portions of the code ported from jvm clj (primarily core library functions, macros, docstrings),
;;where they are direct copies, they fall under the following legacy license
;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;convenience for clj-re
(named-readtables:in-readtable clj-re:readtable)
;;define our own defmacro....weird
;;OUTDATED
(common-lisp:defmacro defmacro (name args &rest body)
  `(common-lisp:defmacro ,name ,args ,@body))

;;note:
;;we use a common-lisp:apply lazyseq compatible replacement
;;from sequences:apply.

;;determine verbosity of def/defn emissions.
;;specifically if we muffle sbcl.
(eval-when  (:compile-toplevel :load-toplevel :execute)
  ;;defvar?
  (defparameter *clj-verbose* nil))

(defun vector? (x) (typep x 'clclojure.pvector::pvec))
;;need to expand this to persistent lists later...
(defun list? (x) (typep x 'common-lisp:cons))
(defun as-list (xs)
  (if (vector? xs)  (vector-to-list xs)
      (if (vector-expr xs) (rest xs)
          xs)))

;;Let's hack let to allow us to infer vector-binds
;;or non-vector but "flat" list (ala clojure's let)
;;as a clojure compatible let definition...
;;we don't have destructuring yet.
;; (defmacro let (bindings &body body)
;;   (if   ;(eq (common-lisp:first bindings) 'persistent-vector)
;;    (or  (vector? bindings)
;;         (not (common-utils::nested-list?  bindings)))
;;    `(unified-let* (,@(partition! 2 (as-list  bindings))) ,@body)
;;    `(cl:let  ,bindings ,@body)))

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
  (cl:let ((classname (gentemp "REIFY"))
           (ctor (gensym "CONSTRUCTOR")))
    `(cl:let ((,ctor (clojure-deftype ,classname ,'() ,@implementations)))
       (funcall ,ctor))))

;;we're destructuring in let now, so no more interop with cl:let.
(defmacro let (bindings &body body)
  (cl:let* ((pairs (partition! 2 (as-list  bindings))))
    (assert (evenp (length bindings)))
    (assert (cl:= (length pairs) (cl:/ (length bindings) 2)))
    `(clclojure.lexical::unified-let* (,@pairs) ,@body)))

;;we want protocols early.
;;need some bootstrapping stuff.
;;for now though, we can just work on reader
;;and analyzer.

;;maybe revisit this later.
(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (defclass CljObj ()  ;;this is probably now in clj-objects, we should use that.
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
     (hasheq :initarg :hasheq))))

;;naive eager version.
;; (defun print-seq (s &optional (stream t))
;;   "Generic vector printer."
;;   (format stream "(~{~s~^ ~})" (seq->list s)))

(defmethod print-object ((obj CljSymbol) stream)
  (with-slots (ns (nm  name)) obj
    (if ns 
        (format stream "~A/~A" ns nm)
        (format stream "~A" nm))))

(defmethod print-object ((obj Var) stream)
  (with-slots (ns sym) obj
    (cl:let ((outer ns))
      (with-slots (ns (nm name)) sym
        (format stream "#'~A/~A" (or outer ns) nm)))))

(defmethod print-object ((obj CljKey) stream)
  (with-slots (ns (nm  name)) obj
    (if ns 
        (format stream ":~A/~A" ns nm)
        (format stream ":~A" nm))))

(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (defprotocol ISymbolic
      (as-symbol (this)))

  (defprotocol ISymbol
      (sym-name  (this))
    (sym-ns    (this))))

(extend-protocol
 ISymbol
 CljSymbol
 (sym-name (this) (slot-value this 'name))
 (sym-ns   (this) (slot-value this 'ns))
 CljKey
 (sym-name (this) (slot-value this 'name))
 (sym-ns   (this) (slot-value this 'ns))
 ;;right now we interop with cl symbols by mapping
 ;;to their package names, except for keywords.
 ;;will probably revisit this.  maybe we can encode
 ;;cl packages into an imported namespace like
 ;;cl.the.package.name so we can resolve it. hmm.
 common-lisp:symbol
 (sym-name (this) (symbol-name this))
 (sym-ns   (this) (when-not (keywordp this)
                            (package-name (symbol-package this)))))

(defun string->symbol (x)
  (cl:let ((res  (uiop:split-string x :separator "/")))
    (if (common-lisp:second res)
        (make-instance 'CljSymbol :ns (common-lisp:first res) :name (common-lisp:second res))
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

(EVAL-WHEN (:compile-toplevel :load-toplevel :execute) 
  ;;copping some fundamental protocols for bootstrapping symbol/key/ns support.
  (defprotocol IHashcode
      (-hashcode (this)))
  (defprotocol IHasheq
      (-hasheq (this)))
  (defprotocol IMeta
      (-meta (o)))

  (defprotocol IWithMeta
      (-with-meta  (o meta))))

(extend-protocol
 IHashcode
 T
 (-hashcode (this) (sxhash this)))

;;shouldn't matter if hashcode is synchronized,
;;it's ideal not to be actually.
(extend-protocol
 IHasheq
 T
 (-hasheq (this) (common-utils::hash-code this))
 CljSymbol
 (-hasheq (this)
          (cl:let ((hc (slot-value this 'hasheq)))
            (if (> hc -1)
                hc
                (cl:let ((newc 
                           (common-utils::hash-code (list (sym-ns this) (sym-name this)))))
                  (setf (slot-value this 'hasheq) newc)
                  newc)))))

(extend-type
 T
 IMeta
 (-meta (o) nil))

 ;;meta stuff 
 (extend-type
  CljSymbol
  IMeta
  (-meta (o) (slot-value o 'meta))
  IWithMeta
  (-with-meta (sym mnew)
      (with-slots (ns (nm name) (m meta)) sym
          (cl:let ((s (clj-symbol  ns nm)))
            (progn 
              (setf (slot-value s 'meta) mnew)
              s)))))

;; (defprotocol IDeref
;;     (-deref (o)))

;; (defprotocol IDerefWithTimeout
;;     (-deref-with-timeout (o msec timeout-val)))

(defun symbol-equal (l r)
  (and (string-equal (sym-name l) (sym-name r))
       (string-equal (sym-ns   l) (sym-ns   r))))

;;global registry of keywords.
;;really irresponsible for now, we just maintain
;;a hashmap.  later we'll use locking or concurrent
;;map to handle stuff better.

;;Also with cl's goofy handling of hash tables,
;;we'll leverage sbcl's extensions to provide custom tests.
;;We could leverage equiv here eventually.

(sb-ext:define-hash-table-test symbol-equal -hasheq)

(defun symbol-hashtable ()
  (make-hash-table :test 'symbol-equal))
;;we can also define synchronized hash tables in sbcl,
;;or alternately use a library (at least one exists)
(defparameter *keys* (symbol-hashtable))

;;keywords are interned (cached) based on the symbol
;;symbols can have meta though, so we want them without meta.
(defun* clj-keyword
    ((name)    (make-instance 'CljKey :name name :ns nil))
    ((ns name) (make-instance 'CljKey :name name :ns ns)))

;;we're hand-waving concurrency and meta at the moment.
(defun intern-key (symb)
  (cl:let ((res (gethash  symb *keys*)))
    (if res res
        (cl:let ((kw (if (sym-ns symb)
                      (clj-keyword (sym-ns symb) (sym-name symb))
                      (clj-keyword (sym-name symb)))))
          (setf (gethash symb *keys*) kw)
          kw))))

(defun throw (e)
  (error e))
;;Right now, common lisp keywords are distinct.
;;We can blur them a bit for interop.
;;we treat this as identity if passed a CL keyword.
;;Maybe the semantics are that unqualified clj keys are equiv to
;;CL keys.
(defun*  keyword
    ((name)    (typecase name
                 (CljKey name)
                 (string  (intern-key (clj-symbol name)))
                 (common-lisp:keyword name)
                 (otherwise (throw (ex-info "unknown symbol-string-or-key!" name)))))
  ((name ns) (intern-key (clj-symbol name ns))))

(defun hash (this) (-hasheq this))

;;namespaces and vars....
;;namespaces is a map of symbol->Namespace
;;a Namespace contains multiple symbol maps
;;-- aliases (symbols mapping an aliased ns/symbol to a Var from another ns)
;;-- mappings (direct mappings of symbols to vars in "this" namespace)


(eval-when  (:compile-toplevel :load-toplevel :execute)
  ;;need to start porting clojure.lang.Namespace here.
  (defclass NameSpace ()
    ((name     :initarg :name)
     (aliases  :initarg :aliases)
     (mappings :initarg :mappings)))

  (defmethod print-object ((obj Namespace) stream)
    (with-slots ((ns-name  name)) obj
      (format stream "#<Namespace ~A>" ns-name)))

  ;;this should be a concurrent hashtable.
  (defparameter *namespaces* (common-utils:->hash-table))
  )

;;note: we can pull in a bunch of the stuff from proto clojure and use
;;that for implementing the reader.

;;move this later...
(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  ;;temporary hacks...
  (define-symbol-macro true 't)
  (define-symbol-macro false nil)
  (defmacro  set! (&rest args)
    `(common-lisp:setf ,@args))
  ;;cljs core uses lookup-sentinel for several datastructure implementations.
  ;;we can get an equivalent to jsObj by gensyming unique symbols.
  (defparameter lookup-sentinel (gensym))
  ;;convenient placeholders
  ;;OUTDATED
  (defun ns (name &rest opts)
    (throw (ex-info "namespaces not implemented fully" nil))
    (eval `(progn (defpackage ,name
                    (:use :clclojure.base :common-lisp)
                    (:shadowing-import-from :clclojure.base :let :deftype :defmacro :map :reduce :first :rest :second :dotimes :nth :cons :count :do :get :assoc :when-let :vector))
                  (in-package ,name))))
  
  ;;TBD redefine this.  If s is a Var, we should look for its
  ;;macro flag....
  (defun macro?    (s) (when (macro-function s) 't))
  ;;TBD redefine this.  If f is a Var, we should look for its
  ;;macro flag....
  (defun function? (s) (fboundp s))
  ;;weak hack around lack of read-time vector creation.
  (defun vector-form? (expr)
    (and (not (common-lisp:atom expr) )
         (or (vector? expr) (eq (common-lisp:first expr) 'persistent-vector)))))

(define-condition not-implemented (error) ())
(define-condition uneven-arguments (error) ())

(defgeneric destructure (bindings))

;;a single function definition
(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (defstruct fn-def  name args body)
  ;;a macro definition -- later
  (defstruct macro-def name args body))

;;OUTDATED
;;At compile-time, [x y] -> (persistent-vector x y).
;;This is upsetting us...
(defmacro quoted-vec (v)
  (if (vector? v)
      `(quote ,v);;`(persistent-vector ,@(mapcar #'quote-sym  (vector-to-list v))) 
      `(persistent-vector ,@(mapcar #'quote-sym  (rest v)))))

(defun variadic (v) (member '& (as-list v)))

;;Todo: move this out to CLOS?
;;parse a clojure style function definition.
;; (defmacro read-fn (arg-vec body)
;;   `(make-fn-def :args   (quoted-vec ,arg-vec) 
;; 		:body   (quote ,body)))

;;we do some minor processing of the function body here.
;;since we have a list coming out of the parse,
;;we look to see if it's a single entry.  If so,
;;we can just return that (or else get an illegal
;;fn call), otherwise we splice an implicit progn in.
(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (defun read-fn (arg-vec body &optional name)
    (cl:let ((new-body body #-sbcl(if (cl:= (length body) 1)
                           (cl:first body)
                           body
                           #-sbcl
                        (cl:cons 'progn body))))
      (make-fn-def :name   name 
                   :args   arg-vec
                   :body   new-body))))

(defgeneric arity (fd))
(defmethod  arity ((fd sequence))  
  (values (length  fd) (variadic fd)))
(defmethod  arity ((fd clclojure.pvector::pvec))  
  (values (vector-count  fd) (variadic fd)))
(defmethod  arity ((fd fn-def)) (arity (slot-value fd 'args)))

;;since clojure allows multiple bodies, with fixed arity for each body, we 
;;compose multiple function (arg body) pairs into a list of function definitions.
;;We should then be able to dispatch on the count of args, simply invoking 
;;the appropriate function matched to arity.

(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (defun parse-fn (expr)
    (cl:let ((res (parse:parse! (parse:.fn-expr) expr)))
      (when res
        (mbind:bind
            (((( _ fn-name )  (_  (fn-type fn-tail)))    res)
             (fn-name (or fn-name  (symb (symbol-name (gensym "fn_"))))))
          (if (seql fn-type :normal)
              (mbind:bind (( ((_ args) (_ body))     fn-tail))
                (read-fn args body  fn-name))
              (mapcar (lambda (spec)
                        (mbind:bind (( (_  ((_ args) (_ body))) spec))
                          (read-fn args body fn-name)))
                      fn-tail))))))
  
  (defun fn* (name &rest specs)
    `(,@(mapcar (lambda (vb) (read-fn (common-lisp:first vb)
                                      (if (cadr vb)                            
                                          (common-lisp:cons 'progn (common-lisp:rest vb))
                                          (common-lisp:second vb)) name)) specs)))

  (defstruct arg-parse lambda-list outer-let)

  (define-condition no-matching-function         (error) ())
  (define-condition multiple-variadic-functions  (error) ())

  ;;this is going to be somewhat tricky, since we'll probably have a little state 
  ;;machine that parses the args, possibly destructuring recursively.  Don't know all 
  ;;the cases yet, but we'll need to be able to destructure vectors and maps into 
  ;;corresponding lambda lists.
  (defun parse-args (args)
    (make-arg-parse :lambda-list
       (mapcar (lambda (x)
                 (if (and (symbolp x)
                          (string-equal (symbol-name x) "&"))
                     '&rest
                                               x))
               (as-list args)))))
;;Compile a clojure fn special form into a common lisp lambda
(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  ;;parse a list of function definitions into an n-lambda dispatching function.
  (defgeneric fndef->sexp (fd))
  
  (defmethod  fndef->sexp ((fd fn-def))
    (with-slots (args body (nm name)) fd
      (with-slots (lambda-list outer-let) (parse-args args)
        (cl:let* ((body     (cl:cond ((and  (listp body)
                                             (cl:atom (cl:first body))
                                             (cl:= (length body) 1))
                                      (cl:first body))
                                     ((cl:= (length body) 1) (cl:first body))
                                     (t                               body)))
                  (interior (if outer-let `(let* ,outer-let ,body)
                                body)))
          ;(pprint (list :fndef->sexp :body body :args args))
          `(named-fn ,nm ,lambda-list ,interior)))))

  (defmethod fndef->sexp ((fd common-lisp:cons))
    (if (common-lisp:= (length fd) 1)  (fndef->sexp (common-lisp:first fd)) ;simple case
        ;;case with multiple function definitions.
        (cl:let ((name (fn-def-name (common-lisp:first fd))))
          `(common-utils:named-fn*
            ,name
            ,@(mapcar (lambda (body)
                        (common-lisp:rest (common-lisp:rest (fndef->sexp body)))) fd)))))

  ;;since we're ditching custom eval on this side, we can go back to regular bindings
  ;;and let*.
  ;;we allow backwards compatibility with cl, so you can pass in
  ;;list formed args instead of vectors and stell get variadic
  ;;function definitions.
  ;;we have to guard against empty arg lits now....which resolve to
  ;;null, which is also a symbol.  so we can get confusion in named
  ;;function parsing (since we now admit common lisp function defs with
  ;;possibly empty arg lists).
  ;;OBE, DELETE THIS
  (defun actual (x) (and (not (null x)) (symbolp x))) 

  ;;we can implement destructuring now by leveraging metabang-bind.
  ;;our test for multiple bodies is different now, since we can
  ;;have a nested list for a single arg version, since the args
  ;;can be destructuring.  So we detect if we have a list of 2-entry
  ;;lists as our multiple-body criteria.
  ;;Then, for each of the arglists, we see if they are normal lambda
  ;;lists, or destructing forms.  We consolidate the destructuring
  ;;forms into discrete args, then emit binding forms for them.

  ;;either we have a 2 element list, or
  ;;we have a nested list of (args body), where
  ;;count of args is distinct.

  ;;If wee want to destructure
  ;;(fn ((x y) z &rest (a b c (d e f) &rest more)))
  ;;it is equivalent to
  ;;(fn (xy z &rest restargs)
  ;;  (let ((x y) xy
  ;;        (a b c (d e f) &rest more) restargs)
  ;; ....)
  ;;so we can naively pick up destructuring forms
  ;;by partitioning left and right of the baseline &
  ;;then assigning forms.  If we admit general lambda lists
  ;;from CL in this form, then that adds a slight burden.
  ;;We can leverage metabang-bind's lambda-destructuring
  ;;form.
  ;;OBE, DELETE THIS
  (defun function-bodies (specs)
    (common-lisp:cond
      ((cl:= (length specs) 2) ;;possibly ambiguous case.
       (cl:let ((l (cl:first specs))
                (r (cl:second specs)))
         (if (and (nested-list? l)
                  (nested-list? r)
                  (not (cl:=  (length (cl:first l))
                              (length (cl:first r)))))
             2
             1)))
      ((every (lambda (xs) (common-lisp:= (length xs) 2)) specs)
       (length specs))
      ;;if first arg is a list,
      ((and (cl:listp (cl:first specs))
            (> (count specs) 2))
       :single-spread)
      (:else (length specs))))

  ;;we can leverage lambda-bind to build out or destructured fns...
  ;;alexandria:parse-ordinary-lambda-list can tell us if this is
  ;;a common lisp ll or if we're deviating.
  ;;if it's a normal lambda-list, then we can parse fn binds
  ;;as is.
  ;;if it's a destructuring arg-body spec, then we can
  ;;parse the form using lambda-bind's expansion.
  (defun dbinding-spec (args body)
    (if (common-utils:normal-lambda? args)
        (list args body) ;;unaltered
        (mbind:bind (((_ newargs binding) (macroexpand-1 `(mbind:lambda-bind ,args ,body))))
          (list newargs `(clclojure.lexical:unified-let* ,@(rest binding)))))) ;;destructured

  ;;if we have a binding form a, if it has to be destructured, we get a
  ;;local form b, where the params of a are gensymed as the list PARENTS,
  ;;the bindings from a are bound to corresponding PARENT,
  (defun rest? (x)
    (and (symbolp x)
         (char= (cl:char (symbol-name x) 0)
                #\&)))
  ;;we need to handle restargs like (& msg)
  ;;so we scan to detect restargs, note it,
  ;;then dbind-fn can splice in the rest arg when building
  ;;the args list back up.
  (defun arg-binds (params) ;;return a list of (old new) args.
    (cl:let* ((rest-arg  (->> params
                             (common-utils:partition-offset! 2 1)
                             (common-utils:filter (lambda (xy)
                                                    (rest? (cl:first xy))))
                             cl:first
                             cl:second))
              (parents  (->  (cl:reduce (lambda (acc x)
                                          (if (rest? x)
                                              acc
                                              (if (cl:atom x)
                                                  (cl:cons (list  x x) acc)
                                                  (cl:cons (list  (gensym "arg") x) acc))))
                                        params :initial-value '())
                             (nreverse)))
              (compound (->> parents
                          (mapcar (lambda (x)
                                    (list (cl:second x) (cl:first x))))
                          (common-utils:filter (lambda (xy)
                                                 (or (not (symbolp xy))
                                                     (not (char= (cl:char (symbol-name (cl:first xy)) 0)
                                                                 #\&)
                                                          #-sbcl
                                                          (seql (cl:first xy) '&)))))
                          )))
      (->hash-table :rest-arg rest-arg
                    :mapping  parents
                    :parents  (mapcar #'cl:first parents)
                    :compound compound)))
  
  ;;helper for destructuring binding fn forms.
  ;;we need these for other bindings like let/for/loop and friends.
  (defun dbind-fn (args body)
    (mbind:bind (((:keys parents compound rest-arg) (arg-binds args)))
      (if (null compound)
          (cl:let ((tl (if (cl:=  (length body) 1)
                           body
                           (cons 'progn  body)))) ;;if we have multiple exprs in body, it's progn.
            ;;(pprint (list :no-compound :args args :tl tl :body body))
            (cl:list args tl)) ;;this condition will probably never be hit.  FIX
          (cl:let ((newargs (if rest-arg
                             (mapcan (lambda (x) (if (seql x rest-arg)
                                                     (list '&rest x)
                                                     (list  x)))
                                     parents)
                             parents))
                   (tl (cl:cond ((cl:atom body) (list body)) ;;implicit progn, splice.
                                ;;imiplicit progn, spliceable.
                                (t body))))
            `(,newargs (clclojure.lexical::unified-let* (,@compound) ,@tl))))))
  
  ;;so clojure simplifies the dbinding process e.g. with loop/recur,
  ;; (loop ((x y) '(1 2) acc 0)
  ;;       body)
  ;; ;;becomes
  ;; (let (g14 '(1 2)
  ;;       (x y) g14
  ;;       acc 0)
  ;;   (loop* (g14 g14
  ;;           acc acc)
  ;;     (let ((x y) g14
  ;;           acc acc)
  ;;       body)))
  ;;so with-recur would pick this up as well,
  ;;same with function bindings
  
  ;;either (arg1 arg2) body | [arg1 arg2] body |
  ;;( ((arg1 arg2) body1)
  ;;  ((arg1 arg2 arg3) body2))
  ;;or if we have vector literals
  ;; (([arg1 arg2] body)s
  ;;  ([arg1 arg2 arg3] body))

  ;;very close to destructuring fn forms + unified forms.
  ;;we need to mode common-utils:named-fn and named-fn* to get the
  ;;behavior we want wired in.  Right now, they are returning function
  ;;object from a labels definition.  We may just return a unified
  ;;lambda that invokes the labels function for us.

  ;;we'll handle odd cases, like nil args (:EMPTY-LIST), etc.
  (defun dbind-fndef (fndef)
    (with-slots (name args body) fndef
      (cl:let* ((nil-args (if (keywordp (cl:first args))
                           nil
                           args)))
        (destructuring-bind (new-args new-body) (dbind-fn nil-args body)
          (make-fn-def :name name :args new-args :body new-body)))))
  
  (defmacro fn (&rest specs)
    (let* ((fndef (parse-fn (cl:list* 'fn specs)))
           (res 
             (if (not (consp fndef))
                 (fndef->sexp (dbind-fndef fndef))
                 (fndef->sexp (mapcar #'dbind-fndef  fndef)))))
      `(,@res))))

;;def 
;;===

;;Experimental.  Not sure of how to approach this guy.
;;for now, default to everything being public / exported.
;;that should be toggled via metadata in real implementation.
;;UPDATE - we should also think about how interop will work,
;;e.g. will there be a corresponding package presence for clojure
;;vars in namespaces? does def mirror things by default?  hmm....

;;we can probably just unify function and value cl ns here
;;by default instead of checking for functionp....
(eval-when (:compile-toplevel :load-toplevel :execute)
  
  ;;establishes a non-dynamic toplevel binding. this
  ;;unscrews us from using defparameter by default,
  ;;and allows symbol-macrolet ala with-slots to work
  ;;equivalently to clojure, if the slot-name collides with
  ;;a top-level binding.  under the prior way, we'd get
  ;;an error since the symbol was declared special (dynamic)
  ;;via defparameter, so symbol-macrolet crapped out.
  (defmacro normal-var (name v)
    `(with-suppressed
         (setq ,name ,v)))

  (defmacro declare-clj (&rest body)
    (assert (every #'symbolp body) () "all forward declarations should be symbols!")
    `(progn ,@(mapcar
               (lambda (x) `(clclojure.base::normal-var ,x :unbound
                                                       ))
               body)))

  ;;Since we can't muffle inside progn, we'd like to have muffled
  ;;as an option.  e.g, for the core libs we're sure are okay,
  ;;we can muffle them.  Then for new functions being built out,
  ;;we can provide unmuffled versions to get warnings.  Can't
  ;;do more until I learn the sbcl compiler policies better to
  ;;handle fwd referencing like this.  It's intended to provide
  ;;compile time security (which is badass), but we want flexibility
  ;;to say "I'm setqing this thing, so we know it will exist, don't
  ;;tell me about it.  DO tell me about everything else though"

  ;;ideally we'd just have a meta form to dispatch on.
  
  ;;we probably want a way to control this in the future, but I'd
  ;;like warnings by default for now.
  ;;we'll have it look at the current package for a *verbose*
  ;;setting and default to that or nil if unbound.
  (defmacro def (var &rest init-form)
    (let (vname (common-utils:str var)
          dyn?  (char= #\*
                       (common-lisp:char vname 0) 
                       (common-lisp:char vname (1- (length vname))))
          initializer (if (and (boundp 'clclojure.base::*clj-verbose*)
                               (not (null clclojure.base::*clj-verbose*)))
                          'setq
                          'normal-var))
      `(progn (,(if dyn? 'defparameter initializer) ,var ,@init-form)
              (with-meta (quote ,var) '((SYMBOL .  T) (DOC . "none")))
              (when (functionp (symbol-value (quote  ,var)))
                (setf (symbol-function (quote ,var)) (symbol-value (quote  ,var))))
              (export ',var)
              (quote ,var)))))

;;A CHEAP implementation of defn, replace this...
(defmacro defn (name args &rest body)
  `(def ,name (fn ,name ,args ,@body)))

;;Clojure Transformations (PENDING)
;;================================
;;we need some basic transformations....
;;I guess we can write a simple clojure reader by swapping some symbols around..
;;maybe even use read macros...

;;Clojure      --  Common Lisp 
;;@x (deref x) ->   used in quasiquoted expression, splice-collection ,@
;;~x (insert x)? used to escape a quasiquote -> ,x 

;;one simple transform is to scan the clojure expression, and change the following:
;;~  -> , 
;;~@ -> ,@ 

;;@x                    -> (deref   x)     ;;need to implement deref
;;(x)                   -> (pvector x)    ;;more or less implemented
;;{x y}                 -> (hash-map x y) 
;;(Blah. x)             -> (make-Blah x) ;;CLOS constructor
;;(. obj method args)   -> ((slot-value obj method) args)
;;(. obj (method args)) -> ((slot-value obj method) args)
;;(. obj method)        -> ((slot-value obj method))
;;(.method obj args)    -> (slot-value obj method) ;;CLOS accessor
;;(def x val)           -> (defparameter x val)
;;(let (x y) expr)      -> (let* ((x y)) expr)
;;(some-namespace-alias/the-function x) -> (some-package-alias::the-function x)
;;(ns blah)             -> (defpackage blah)
;;#"some-regex"         -> (make-regex "some-regex")  ;;need to use cl-ppre probably...

(defmacro doc (v) `(pprint (rest (common-lisp:assoc 'DOC (meta ,v)))))
(defmacro do (&rest exprs)
  `(progn ,@exprs))

;;Meta Data
;;=========

;;I think we want to use persistent maps for meta data, as clojure does.
;;I want to get the stubs in place, and am using property lists with a 'meta 
;;entry pointing at an assoc list for now.

;;These should be pulled out into a protocol.
;;if we retain this, maybe coerce to a map.
(defmacro symbol-meta (symb)        `(common-lisp:get (quote ,symb) 'meta))
(defmacro with-symbol-meta (symb m) `(setf (common-lisp:get (quote ,symb) 'meta) ,m))

(eval-when  (:compile-toplevel :load-toplevel :execute)
  (defun meta (obj)        (-meta obj))
  (defun with-meta (obj m) (-with-meta obj m)))

;;One thing about metadata, and how it differs from property lists: 
;;You can call meta on datastructures, or objects, and get a map back.
;;Symbols can have meta called on them with (meta #'the-symbol), which 
;;uses sharp-quote to get the symbol, vs the symbol-name.

;;Clojure is a lisp-1, so we need to ensure that everything, even 
;;functions, gets bound into the a single namespace. 

;;another way to do this is to have clojure-specific symbols be actual 
;;clos objects, which have meta data fields automatically.  Then we 
;;lose out on all the built in goodies from common lisp though.

;;Clojure Core (PENDING)
;;======================


;;;;;;;;;;;;;;;;;;;;;;;;;;; core protocols ;;;;;;;;;;;;;

;;Note - with variadic protocol fns, we currently have to pass them
;;as ((arg1 (body 1)) (args2 body2))  etc. since the implementation
;;of the generic fn is a varargs guy, which dispatch to the variadic
;;function machinery.

;;We might make this more consistent with clojure (or handle it from the
;;clojure side maybe), by recognizing that discrete arities for a single
;;protocol fn can be supplied and collected into a varargs implementation
;;on the caller's behalf.  I forget how it works in clojure right now.
;;The biggest example is IFn though.

;;Need to get back to this guy...multiple arity is not yet implemented
;;perfectly for protocol fns.  if we just want (-invoke (this) :return)
;;we currently have to do (-invoke (this &rest args) :return) to get
;;0-arity invocation.  SAD.  discrete arities work fine though, just
;;not 0-arity. TODO
(eval-when (:compile-toplevel :load-toplevel :execute) 

  (defprotocol IFn
      (-invoke
       (this)
       (this a)
       (this a b)
       (this a b c)
       (this a b c d)
       (this a b c d e)
       (this a b c d e f)
       (this a b c d e f g)
       (this a b c d e f g h)
       (this a b c d e f g h i)
       (this a b c d e f g h i j)
       (this a b c d e f g h i j k)
       (this a b c d e f g h i j k l)
       (this a b c d e f g h i j k l m)
       (this a b c d e f g h i j k l m n)
       (this a b c d e f g h i j k l m n o)
       (this a b c d e f g h i j k l m n o p)
       (this a b c d e f g h i j k l m n o p q)
       (this a b c d e f g h i j k l m n o p q s)
       (this a b c d e f g h i j k l m n o p q s t)
       (this a b c d e f g h i j k l m n o p q s t rest)))


  ;;These work
  (defprotocol ICounted
      (-count (coll) "constant time count"))

  (defprotocol IEmptyableCollection
      (-empty (coll)))

  (defprotocol ICollection
      (-conj (coll o)))

  (defprotocol IOrdinal
      (-index (coll)))

  ;;this will break.  current implementation of defprotocol doesn't allow for 
  ;;multiple arity functions like this.  Need to handle variadic functions...
  (defprotocol IIndexed
      (-nth (coll n) (coll n not-found)))

  (defprotocol ASeq)

  (defprotocol ISeq
      (-first (coll))
    (-rest (coll)))

  (defprotocol INext
      (-next (coll)))

  (defprotocol ILookup
      (-lookup (o k) (o k not-found)))

  (defprotocol IAssociative
      (-contains-key? (coll k))
    (-entry-at (coll k))
    (-assoc (coll k v)))

  (defprotocol IMap
      (-assoc-ex (coll k v))
    (-dissoc (coll k)))

  (defprotocol IMapEntry
      (-key (coll))
    (-val (coll)))

  (defprotocol ISet
      (-disjoin (coll v)))

  (defprotocol IStack
      (-peek (coll))
    (-pop (coll)))

  (defprotocol IVector
      (-assoc-n (coll n val)))

  (defprotocol IDeref
      (-deref (o)))

  (defprotocol IDerefWithTimeout
      (-deref-with-timeout (o msec timeout-val)))

  ;; (defprotocol IMeta
  ;;     (-meta (o)))

  ;; (defprotocol IWithMeta
  ;;     (-with-meta (o meta)))

  (defprotocol IReduce
      (-reduce (coll f)
               (coll f start)))

  (defprotocol IKVReduce
      (-kv-reduce (coll f init)))

  (defprotocol IEquiv
      (-equiv (o other)))

  (defprotocol IHash
      (-hash (o)))
  (defprotocol ISeqable
      (-seq (o)))
  


  (defprotocol ISequential
    "Marker interface indicating a persistent collection of sequential items")

  (defprotocol IList
    "Marker interface indicating a persistent list")

  (defprotocol IRecord
    "Marker interface indicating a record object")

  (defprotocol IReversible
      (-rseq (coll)))

  (defprotocol ISorted
      (-sorted-seq (coll ascending?))
    (-sorted-seq-from (coll k ascending?))
    (-entry-key (coll entry))
    (-comparator (coll)))

  ;; (defprotocol ^:deprecated IPrintable
  ;;   "Do not use this.  It is kept for backwards compatibility with existing
  ;;    user code that depends on it, but it has been superceded by IPrintWithWriter
  ;;    User code that depends on this should be changed to use -pr-writer instead."
  ;;   (-pr-seq (o opts)))

  (defprotocol IWriter
      (-write (writer s))
    (-flush (writer)))

  (defprotocol IPrintWithWriter
    "The old IPrintable protocol's implementation consisted of building a giant
   list of strings to concatenate.  This involved lots of concat calls,
   intermediate vectors, and lazy-seqs, and was very slow in some older JS
   engines.  IPrintWithWriter implements printing via the IWriter protocol, so it
   be implemented efficiently in terms of e.g. a StringBuffer append."
    (-pr-writer (o writer opts)))

  (defprotocol IPending
      (-realized? (d)))

  (defprotocol IWatchable
      (-notify-watches (this oldval newval))
    (-add-watch (this key f))
    (-remove-watch (this key)))

  (defprotocol IEditableCollection
      (-as-transient (coll)))

  (defprotocol ITransientCollection
      (-conj!       (tcoll val))
      (-persistent! (tcoll)))

  (defprotocol ITransientAssociative
      (-assoc! (tcoll key val)))

  (defprotocol ITransientMap
      (-dissoc! (tcoll key)))

  (defprotocol ITransientVector
      (-assoc-n! (tcoll n val))
    (-pop! (tcoll)))

  (defprotocol ITransientSet
      (-disjoin! (tcoll v)))

  (defprotocol IComparable
      (-compare (x y)))

  (defprotocol IChunk
      (-drop-first (coll)))

  (defprotocol IChunkedSeq
      (-chunked-first (coll))
    (-chunked-rest (coll)))

  (defprotocol IChunkedNext
      (-chunked-next (coll)))

  (defprotocol INamed
      (-name (thing)))
  ;;Not sure if CLJS has this, but it's useful
  ;;here.
  (defprotocol IString
      (-to-string (this)))
  ;;deviate from common-utils here on purpose.
  ;;for now...we listify this.
  ;;apply is eager.
  ;;need to make apply work with seqables outright, right
  ;;now sequences lib doesn't know about seqable.
  (defun apply (f arg &rest args)
    (if (null args)
        (common-lisp:apply f (if (consp arg)
                                 arg
                                 (sequences::seq->list (-seq arg))))
        (cl:let ((arglist (if (consp arg)
                           (list*  arg args)
                           (list*  (sequences::seq->list (-seq arg)) args))))
          (common-lisp:apply f arglist)))))
  
  
  ;;Extending types to native structures and clojure literals:
  ;;==========================================================
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (extend-protocol
     IString
     t
     (-to-string (this) (princ-to-string this))
     string
     (-to-string (this) this)
     common-lisp:symbol
     (-to-string (this)
       (if (typep this 'common-lisp:keyword)
           (prin1-to-string this)
           (princ-to-string this)))
     cljkey
     (-to-string (this) (prin1-to-string this))
     cljsymbol
     (-to-string (this) (princ-to-string this))
     )
    (extend-protocol
     IFn
     Function
     (-invoke ((this &rest args)
               (apply this args))))
    (extend-protocol
     IEquiv
     t (-equiv (this that) (eq this that))
     )
    (extend-type
     null
     ICounted
     (-count (c) 0)
     IEmptyableCollection
     (-empty (c) nil)
     ICollection
     (-conj (coll itm) (common-lisp:cons itm nil))
     IStack
     (-peek (coll) nil)
     (-pop  (coll) nil)
     ISeqable
     (-seq (coll) nil)
     IHash
     (-hash (o) (sxhash nil))
     IEquiv
     (-equiv (o other) (error 'not-implemented))
     ISeq
     (-first (o) nil)
     (-rest  (o) nil)
     IReversible
     (-rseq (coll) nil))

    ;;We got a ton of goodies from
    ;;sb-sequences namespace to leverage here.
    ;;good opportunity for iterator-seq...
    (extend-type
     sequence
     ICounted
     (-count (c) (common-lisp:length c))
;;   IEmptyableCollection
;;   (-empty (c) (sb-sequence:make-sequence-like c 0))
     ;; ICollection
     ;; (-conj (coll itm) (cons itm nil))
     IStack
     (-peek (coll) (elt coll 0))
     (-pop  (coll) (cl:subseq coll 1))
     IIndexed
     (-nth  (coll n) (elt coll n))
     (-nth  (coll n not-found)
            (if (<= n (cl:length coll))
                (elt coll n)
                not-found))
     ISeqable
     (-seq (coll)
           (if (typep coll 'sequences::indexed)
               (sequences::indexed-seq coll)
               (error 'not-implemented)))
     IHash
     (-hash (o) (sxhash o))
     
     ;; IEquiv
     ;; (-equiv (o other) (error 'not-implemented))

     ISeq
     (-first (o) (elt o 0))
     ;;TODO pull this over...
     ;;Probably identical to array-seqs

     (-rest  (o) (error 'not-implemented))
     IReversible
     (-rseq (coll) (reverse coll)))

    (extend-type
     clclojure.pvector::pvec
     
     ICounted
     (-count (c) (vector-count c))
     IIndexed
     (-nth  (coll n) (nth-vec coll n))
     (-nth  (coll n not-found) (nth-vec coll n))
     ILookup
     (-lookup (coll idx) (if (< -1 idx (vector-count coll))
                             (nth-vec coll idx)
                             nil))
     (-lookup (coll idx not-found)
              (if (< -1 idx (vector-count coll))
                  (nth-vec coll idx)
                  not-found))
     
     IEmptyableCollection
     (-empty (c) +empty-pvec+)
     ICollection
     (-conj (coll itm) (vector-conj coll itm))
     IVector
     (-assoc-n (coll n val) (vector-assoc coll n val))
     IStack
     (-peek (coll)
            (when (not (zerop (-count coll) )) (nth-vec coll 0)))
     (-pop  (coll)  (subvec coll 1))
     ISeqable
     (-seq (coll) (vector-to-list coll ))
     IHash
     (-hash (o)   (error 'not-implemented))
     IMeta
     (-meta (this) (clclojure.pvector::pvec-_meta this))
     IWithMeta
     (-with-meta (this m) (let (vnew (clclojure.pvector::copy-pvec this))
                            (setf (clclojure.pvector::pvec-_meta vnew) m)
                            vnew))
     IMapEntry
     (-key (coll) (-nth coll 0))
     (-val (coll) (-nth coll 1))
     IEquiv
     (-equiv (o other) (error 'not-implemented))

     IKVReduce
     (-kv-reduce (coll f init) (error 'not-implemented))
     
     IReversible
     (-rseq (coll) (error 'not-implemented))
     IChunk
     (-drop-first (coll) (error 'not-implemented))
     IChunkedSeq
     (-chunked-first (coll) (error 'not-implemented))
     (-chunked-rest (coll) (error 'not-implemented))
     IChunkedNext
     (-chunked-next (coll) (error 'not-implemented))))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (extend-type  symbol 
                  IMeta
                  (-meta (obj) (symbol-meta obj))
                  IWithMeta
                  (-with-meta (obj m) (with-symbol-meta obj m) obj)
                  IEquiv
                  ;;dirty implementation....
                  ;;We need to unify qualified and unqualified symbols..
                  ;;in clojure, symbol equality is a bit more complex
                  ;;since they're equiv iff unqualified.
                  ;;unless we hack the reader to reader qualified
                  ;;symbols as unqual, the preponderance of clojure
                  ;;symbol comparisons will not be strict, so
                  ;;we end up with a lot of unqualified symbols.
                  ;;This is just to paper over the bootstrapping
                  ;;process....
                  (-equiv (l r)
                          (or (eq l r)
                              (when  (not (or (keywordp l) (keywordp r)))
                                (common-lisp:= (sxhash l) (sxhash r)))
                              ))
                  INamed
                  (-name (this) (symbol-name this))
                  )

    ;;not applicable.
    (extend-type
     CljKey
     IEquiv
     ;;dirty implementation....
     ;;We need to unify qualified and unqualified symbols..
     ;;in clojure, symbol equality is a bit more complex
     ;;since they're equiv iff unqualified.
     ;;unless we hack the reader to reader qualified
     ;;symbols as unqual, the preponderance of clojure
     ;;symbol comparisons will not be strict, so
     ;;we end up with a lot of unqualified symbols.
     ;;This is just to paper over the bootstrapping
     ;;process....              
     (-equiv (l r) (or (eq l r)))
     IHash
     (-hash (k) (hash-code k))
     INamed
     (-name (k) (slot-value k 'name))
     )
    (extend-type
     CljSymbol
     IEquiv
     ;;same as above...this a dirty hack for now.
     (-equiv (l r) (or (eq l r)))
     IHash
     (-hash (k) (hash-code k))
     INamed
     (-name (k) (slot-value k 'name))
     )
    (extend-type
     Namespace
     IEquiv
     ;;same as above...this a dirty hack for now.
     (-equiv (l r) (or (eq l r)))
     IHash
     (-hash (k) (hash-code k))
     INamed
     (-name (k) (slot-value k 'name))
     )

    ;;subvector impls...
    (extend-type
     clclojure.pvector::subvector
     
     ICounted
     (-count (c) (vector-count c))

     IEmptyableCollection
     (-empty (c) +empty-pvec+)
     ICollection
     (-conj (coll itm) (vector-conj coll itm))
     IVector
     (-assoc-n (coll n val) (vector-assoc coll n val))
     IStack
     (-peek (coll)
            (when (not (zerop (-count coll) )) (nth-vec coll 0)))
     (-pop  (coll)  (subvec coll 1))
     ISeqable
     (-seq (coll) (vector-to-list coll)) ;poorly implemented.  should be arrayseq
     IHash
     (-hash (o)   (error 'not-implemented))
     IMeta
     (-meta (this) (clclojure.pvector::subvector-_meta this))
     IWithMeta
     (-with-meta (this m) (let (vnew (clclojure.pvector::copy-subvector this))
                            (setf (clclojure.pvector::subvector-_meta vnew) m)
                            vnew))
     IMapEntry
     (-key (coll) (-nth coll 0))
     (-val (coll) (-nth coll 1))
     IEquiv
     (-equiv (o other) (error 'not-implemented))
     IKVReduce
     (-kv-reduce (coll f init) (error 'not-implemented))

     IReversible
     (-rseq (coll) (error 'not-implemented))
     IChunk
     (-drop-first (coll) (error 'not-implemented))
     IChunkedSeq
     (-chunked-first (coll) (error 'not-implemented))
     (-chunked-rest (coll) (error 'not-implemented))
     IChunkedNext
     (-chunked-next (coll) (error 'not-implemented))
     ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;list operations.
  (extend-type
   common-lisp:cons
   ICounted
   (-count (c) (length c))

   IEmptyableCollection
   (-empty (c) '())
   ICollection
   (-conj (coll itm) (common-lisp:cons itm coll))
   IStack
   (-peek (coll)  (common-lisp:first coll))
   (-pop  (coll)  (common-lisp:rest coll))
   ISeqable
   (-seq (coll) coll)
   IHash
   (-hash (o)   (sxhash o))
   IEquiv
   (-equiv (o other) (error 'not-implemented))
   IMapEntry
   (-key (coll) (common-lisp:first coll))
   (-val (coll) (common-lisp:second coll))
   ISeq
   (-first (coll)  (common-lisp:first coll))
   (-rest  (coll)  (cdr coll))
   )

  (extend-type
   sequences::lazyseq
   ICounted
   (-count (c) (sequences:seq-count c))

   IEmptyableCollection
   (-empty (c) (error 'not-implemented)) ;;should be persistent list.
   ICollection
   (-conj (coll itm) (sequences::cons itm coll))
   IStack
   (-peek (coll)  (sequences::first coll))
   (-pop  (coll)  (sequences::rest coll))
   ISeqable
   (-seq (coll) (sequences::seq coll))
   IHash
   (-hash (o)   (sxhash o)) ;;poorly implemented...
   IEquiv
   (-equiv (o other) (error 'not-implemented))
   IMapEntry
   (-key (coll) (sequences::first coll))
   (-val (coll) (sequences::rest coll))
   ISeq
   (-first (coll)  (sequences::first coll))
   (-rest  (coll)  (sequences::rest coll))
   IMeta
   (-meta (o) (clj-objects:_meta o))
   IWithMeta
   (-with-meta (o meta)
               (with-slots (pending val clj-objects:_meta) o 
                 (make-instance 'sequences::LazySeq :pending pending :val val :_meta clj-objects:_meta)))

   )

  (extend-type
   sequences::funcseq
   ICounted
    (-count (c) (sequences:seq-count c))

   IEmptyableCollection
   (-empty (c) (error 'not-implemented)) ;;should be persistent-list!
   ICollection
   (-conj (coll itm) (sequences::cons itm coll))
   IStack
   (-peek (coll)  (sequences::first coll))
   (-pop  (coll)  (sequences::rest coll))
   ISeqable
   (-seq (coll) (sequences::seq coll))
   IHash
   (-hash (o)   (sxhash o)) ;;poorly implemented...
   IEquiv
   (-equiv (o other) (error 'not-implemented))
   IMapEntry
   (-key (coll) (sequences::first coll))
   (-val (coll) (sequences::rest coll))
   ISeq
   (-first (coll)  (sequences::first coll))
   (-rest  (coll)  (sequences::rest coll))
   IMeta
   (-meta (o) (clj-objects:_meta o))
   IWithMeta
   (-with-meta (o meta)
               (with-slots (sequence sval seed clj-objects:_meta) o 
                 (make-instance 'sequences::FuncSeq :sequence sequence :sval sval :seed seed :_meta clj-objects:_meta)))
   )

  (extend-type
   clclojure.cowmap::cowmap

   ICounted
   (-count (c) (map-count c))

   IEmptyableCollection
   (-empty (c) clclojure.cowmap::+empty-cowmap+)

   ICollection
   (-conj (coll itm)
          (if (typep itm 'clclojure.cowmap::cowmap)
              ;;merge all the keys.  I missed that conj acts like this for maps man.
              (->> (-seq itm)
                   (sequences:reduce
                    (fn (acc itm)
                        (map-assoc acc (common-lisp:first itm) (common-lisp:second itm)))
                    coll))
              (map-assoc coll (common-lisp:first itm) (common-lisp:second itm))))

   ISeqable
   (-seq (coll) (map-seq coll))
   
   ILookup
   (-lookup (o k) (map-get o k))
   (-lookup (o k not-found)
            (or (map-get o k) not-found))  

   IAssociative
   (-contains-key? (coll k) (map-contains? coll k))
   (-entry-at (coll k)      (map-entry-at coll k))
   (-assoc (coll k v)       (map-assoc coll k v))

   IMap
   (-assoc-ex (coll k v)  (error 'not-implemented)) ;;apparently vestigial
   (-dissoc   (coll k)    (map-dissoc coll k))

   IMeta
   (-meta (this) (clclojure.cowmap::cowmap-_meta this))
   IWithMeta
   (-with-meta (this m) (let (mnew (clclojure.cowmap::copy-cowmap this))
                          (setf (clclojure.cowmap::cowmap-_meta mnew) m)
                          mnew))
   
   IHash
   (-hash (o)   (error 'not-implemented))
   IEquiv
   (-equiv (o other) (error 'not-implemented))
   IKVReduce
   (-kv-reduce (coll f init) (error 'not-implemented)))

  (extend-type
   common-lisp:hash-table
   
   ICounted
   (-count (c) (hash-table-count c))

   IEmptyableCollection
   (-empty (c) (common-utils:->hash-table))
   
   ;; ICollection ;;not writeable for now.
   ;; (-conj (coll itm)
   ;;        (if (typep itm 'clclojure.cowmap::cowmap)
   ;;            ;;merge all the keys.  I missed that conj acts like this for maps man.
   ;;            (->> (-seq itm)
   ;;                 (sequences:reduce
   ;;                  (fn (acc itm)
   ;;                      (map-assoc acc (common-lisp:first itm) (common-lisp:second itm)))
   ;;                  coll))
   ;;            (map-assoc coll (common-lisp:first itm) (common-lisp:second itm))))

   ISeqable
   (-seq (coll)
         (let (rator (common-utils:hash-table-iterator coll)
               step  (fn step (it)
                            (lazy-seq
                             (let (entry (it))
                               (when entry
                                   (cons entry  
                                         (step it)))))))
           (step rator)))
   
   ILookup
   (-lookup (o k)
            (gethash k o))
   (-lookup (o k not-found)
            (gethash k o not-found))  
   IAssociative
   (-contains-key? (coll k)
    (let ((:values _ present?) (gethash k coll))
      present?))
   (-entry-at (coll k)
              (let ((:values v present?) (gethash k coll))
                (when present? (list k v))))
   (-assoc (coll k v)  (do  (setf (gethash k coll) v) coll))

   IMap
   (-assoc-ex (coll k v)  (error 'not-implemented)) ;;apparently vestigial
   (-dissoc   (coll k)    (do  (remhash coll k) coll))

   IMeta
   (-meta (this) nil)
   IWithMeta
   (-with-meta (this m) (error 'not-implemented))
   
   IHash
   (-hash (o)  (hash o))
   IEquiv ;;TBD, probabl should be.
   (-equiv (o other) (error 'not-implemented))
   IKVReduce ;;SHOULD be implemented fwiw.
   (-kv-reduce (coll f init) (error 'not-implemented)))

  (extend-type  number
                IEquiv
                (-equiv (l r)  (when (numberp r) (common-lisp:= l r)))
                IHash
                (-hash (n) (hash-code n)))

  (extend-type
   String
   INamed
   (-name (x) x)
   IIndexed
   (-nth (coll n) (common-lisp:char coll n))  ;;TODO: schar optimization option?
   (-nth (coll n not-found)
         (if (< n (length coll))
             (common-lisp:char coll n)
             not-found))
   ISeqable
   (-seq (coll) (sequences::seq coll))
   ISeq
   (-first (coll)  (elt coll 0))
   (-rest  (coll)  (sequences::rest coll))
   IEquiv
   (-equiv (this other)
     (and (stringp other) (string-equal this other))))
  )
;; IChunk
;; (-drop-first (coll) (error 'not-implemented))
;; IChunkedSeq
;; (-chunked-first (coll) (error 'not-implemented))
;; (-chunked-rest (coll) (error 'not-implemented))
;; IChunkedNext
;; (-chunked-next (coll) (error 'not-implemented))

;;friendly map printing
(defmethod print-object ((obj hash-table) stream)
  (common-utils::print-map  obj stream))

;;map printing compatibility
(defmethod print-object ((obj clclojure.cowmap::cowmap) stream)
  (common-utils::print-map (cowmap-table obj) stream))

;;Core Lib
;;========

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro if-let (binding body &rest false-body)
    (let (binding (-seq binding)        
          arg     (-first binding)
          expr    (-first (-rest  binding))
          tst     (gensym "tst")) 
      `(let ,(list tst  (common-lisp:second binding))
         (if ,tst
             (let ,(list arg  tst)
               ,body)
             ,@false-body))))
  (declaim (inline equiv))
  (defn str
      (() "")
      ((x &rest xs)
       (format nil "~{~a~}" (mapcar #'-to-string (cl:cons x xs)))))
  ;;this isn't great....
  ;;it's possible that the concrete type isn't covered under the protocol,
  ;;but a base type is.  so we might want more of an exhaustive look through
  ;;the precedence hierarchy.  Alternately, we can look for method-combinations.
  (defn class  (obj) (class-of obj))
  (defn class? (obj) (or  (typep obj 'common-lisp:standard-class)
                          (typep obj 'common-lisp:structure-class)
                          (typep obj 'common-lisp:built-in-class)))
  (defn supers (cls) (sb-mop:class-precedence-list cls))
  (defn bases  (cls) (sb-mop:class-direct-superclasses))
  ;;since protocols are all generic functions, with some
  ;;type registration, we check the protocol's chain of custody.
  ;;we will need to revise protocol implementation later,
  ;;due to the added ways implementations can be defined.
  ;;in clj jvm, there is a map of class->implementation, as well
  ;;as an interface.  there's also an option to allow metadata
  ;;implementations.  right now, we limit to direct extension
  ;;by subtyping (through clos generic functions).  that information
  ;;is stored simply in a members list on the protocol struct.
  ;;we want to see if the class of x directly satisfies the protocol,
  ;;e.g. is in the members slot of the protocol (a set encoded as a list),
  ;;or if any of x's superclasses satisfy the protocol.
  ;;we need to cache/memoize going forward as well.
  ;;right now repeated lookups will be fine.  should be able to cache
  ;;based on the protocol struct identity, and the class of x.x
  (defn find-protocol-impl (protocol x)
    (let (c       (if (class? x) x  (class x))
          ;;just  list of class syms, converted to hash-table:: k -> true|T
          impls   (uiop/utility:list-to-hash-set
                    (protocol-members protocol))
          impl    (fn (cls) (gethash cls impls)))
      (or (impl (class-name  c))
          (and c (or (cl:loop for cls in (cl:rest (supers c))
                           when   (impl (class-name  cls))
                           return cls)))
          (impl t))))
  ;;unclear if a pair is fast to cache.  maybe.  meh.
  ;;TODO migrate to nested hashtable.
  (def cached-proto
      (let (outer (make-hash-table :test 'eq)) ;;other option is nested ht.
        (fn (proto cls)
            (if-let (inner (-lookup outer proto))
              (if-let (res (-lookup inner cls))
                res
                (let (res (find-protocol-impl proto cls))
                  (do  (-assoc inner cls res)
                       res)))
              (let (inner (make-hash-table :test 'eq)
                    res   (find-protocol-impl proto cls))
                (do (setf (gethash proto outer) inner)
                    (setf (gethash cls inner) res)
                  res))))))
  ;;we probably want our own satisfies? that caches implementation
  ;;and wraps defprotocol:satisfies?

  ;;memoize our implementation cache.
  ;;naive hashtable with eq semantics.
  
  (defn satisfies? (p obj)
    (cached-proto p (if (class?  obj) obj (class obj))))
  
  ;;cljs defines implements?, which we just wrap around our
  ;;cached satisfies? implementation.
  (defn implements? (p obj)  (satisfies? p obj))
  (defn extenders   (p)      (protocol-members p))

  (defn extends? (p cls) (member (class-name cls) (protocol-members p)))
  
  (defn seq  (coll) (-seq coll))
  (defn seq? (coll) (implements? ISeq coll))
  (defn seqable? (coll) (implements? ISeqable coll))
  (defn vec (coll)
    (if (vector? coll) coll
        (sequences:apply #'persistent-vector (seq coll))))
  (defn vector (& xs)
    (sequences:apply #'persistent-vector (seq xs)))
  
  (defn hash-map (& xs)
    (if (null xs)
        clclojure.cowmap:+empty-cowmap+
        (sequences:apply #'persistent-map (seq xs))))
  
  (defn identical? (l r)
    (common-lisp:eq l r))
  ;;TBD replace with cl:null, might be faster.
  (defn nil? (x)
    (identical? x nil))
  ;;need to implement arrayseq...
  ;;These are lame but easy, we really want to
  ;;get chunked-first and friends up and running.
  ;;Also want to bake in typecases for quick
  ;;dispatch...

  (defn first  (coll)  (-first (seq coll)))
  (defn rest   (coll)  (-rest  (seq coll)))
  ;; "Returns the substring of s beginning at start inclusive, and ending
  ;; at end (defaults to length of string), exclusive."
  (def subs #'subseq)
  ;;TBD fix this for an actual -next implementation.
  ;; "Returns a seq of the items after the first. Calls seq on its
  ;; argument.  If there are no more items, returns nil"

  ;;tbd : get metadata reader working...
                                        ;^seq
  (defn next
    (coll)
    (when-not (nil? coll)
              (if (implements? INext coll)
                  (-next coll)
                  (seq (rest coll)))))
  (defn nnext
    (coll)
    (next (next coll)))
  
  (defn second (coll)  (first (rest coll)))
  (defn ffirst (coll)  (first (first coll)))
  ;;Inaccurate...

  (defn fnext  (coll) (first (rest coll)))

  (defn get
      ((m k) (-lookup m k))
    ((m k not-found) (-lookup m k not-found)))

  (def odd? #'common-utils:odd?)
  (def even? #'common-utils:even?)
  (def zero? #'common-utils:zero?)
  (defn inc (x) (1+ x))
  (defn dec (x) (1- x))
  ;;TODO look at optimizing this.
  ;;We are probably waaaaay slow.
  ;;guessing this is a Good Thing  
  (defn equiv (x y)
    (if (and (numberp x) (numberp y))
        (common-lisp:= x y)
        (or (identical? x y)
            (-equiv x y))))
  
  (defn =
      ((x)   true)
      ((x y)
       (equiv x y))
      ((x y & more)
       (if (-equiv x y)
           (if (next more)
               (recur y (first more) (next more))
               (- y (first more)))
           nil)))
  
  ;;just leverage cl's numeric specialized form.
  (setf (fdefinition '==) #'cl:=)
  (defn key (e) (-key e))
  (defn val (e) (-val e))
  (defn namespace (this) (sym-ns this))
  (defn name (x) (-name x)))

(defmacro when-let (binding &rest body)
  (let (binding (seq binding)        
    arg     (-first binding)
    expr    (-first (-rest  binding))
    tst     (gensym "tst")) 
    `(let ,(list tst  (common-lisp:second binding))
       (when ,tst
         (let ,(list arg tst)
           ,@body)))))

;;try-catch-finally...

(defn ex-info
  ((msg map)
   (make-instance 'exception-info :data map :cause msg  :message msg))
  ((msg map cause)
   (make-instance 'exception-info  :data map :cause cause  :message msg)))

(defn ex-data (e)
  (common-utils::exception-info-data e))
(defn ex-cause (e)
  (common-utils::exception-info-cause e))
(defn ex-message (e)
  (common-utils::exception-info-message e))

;;using cl macros for now to get behavior in place..
;; "defs name to have the root value of the expr iff the named var has no root value,
;;   else expr is unevaluated"
;; {:added "1.0"}
;;Slight deviation from clojure.core implementation, which uses def
;;internally...may revisit.
(defmacro defonce (name expr)
  `(when-not (boundp (quote ,name))
             (def ~name ~expr)))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defn count (coll)  (-count coll))
  ;; "Takes a set of test/expr pairs. It evaluates each test one at a
  ;;   time.  If a test returns logical true, cond evaluates and returns
  ;;   the value of the corresponding expr and doesn't evaluate any of the
  ;;   other tests or exprs. (cond) returns nil."
  ;; {:added "1.0"}

  ;;need to implement throw for most of the stdlib.
  (defmacro cond  (&rest clauses)
    (when (seq  clauses)
      (list 'if (first clauses)
            (if (next clauses)
                (common-lisp:second clauses)
                (throw (ex-info "cond requires an even number of forms"  clclojure.cowmap:+empty-cowmap+)))
            (cl:cons 'clclojure.base:cond (next (next clauses))))))
  ;;maybe implement clj-case?
  ;;clj just un-nests the case clauses, so we can transform it into
  ;;a cl case by re-nesting them.
  ;;in clj, if case has even number of args, we just pack them into cons.
  ;;if odd, last arg is (otherwise arg).
  ;;there's an interesting problem here; clojure allows mixing test literals in a case
  ;;macro.  cl defaults to eql (I think).  so you can do symbols, numbers, chars, but not
  ;;strings or structurally equal things.  We will have to implement our own version
  ;;at some point.
  (defmacro case (keyform &rest clauses)
    (cl:let* ((default (when (oddp (length clauses))
                         (list 'otherwise  (cl:first  (cl:last clauses)))))
              (knowns (common-utils:partition! 2 clauses))
              (args  (if default  (append  knowns (list  default))
                         knowns)))
      `(cl:case ,keyform ,@args )))
  
  (defmacro loop* (bindings &rest body)
    (assert (or  (vector? bindings)
                 (not (nested-list? bindings))))
    (assert (even? (count bindings)))
    `(with-recur ,(seq->list bindings)
       ,@body))

  ;; "Evaluates the exprs in a lexical context in which the symbols in
  ;;   the binding-forms are bound to their respective init-exprs or parts
  ;;   therein. Acts as a recur target."

  ;; (defmacro loop
  ;;   "Evaluates the exprs in a lexical context in which the symbols in
  ;;   the binding-forms are bound to their respective init-exprs or parts
  ;;   therein. Acts as a recur target."
  ;;   {:added "1.0", :special-form true, :forms '((loop (bindings*) exprs*))}
  ;;   (bindings & body)
  ;;   (assert-args
  ;;    (vector? bindings) "a vector for its binding"
  ;;    (even? (count bindings)) "an even number of forms in binding vector")
  ;;   (let (db (destructure bindings))
  ;;     (if (= db bindings)
  ;;         `(loop* ~bindings ~@body)
  ;;         (let (vs (take-nth 2 (drop 1 bindings))
  ;;           bs (take-nth 2 bindings)
  ;;           gs (map (fn (b) (if (symbol? b) b (gensym))) bs)
  ;;           bfs (reduce1 (fn (ret (b v g))
  ;;                            (if (symbol? b)
  ;;                                (conj ret g v)
  ;;                                (conj ret g v b g)))
  ;;                        () (map vector bs vs gs)))
  ;;           `(let ~bfs
  ;;              (loop* ~(vec (interleave gs gs))
  ;;                     (let ~(vec (interleave bs gs))
  ;;                       ~@body)))))))

  (defmacro loop (bindings &rest body)
    `(loop* ,bindings ,@body))) 

(defn nth
  ((coll index)
     (-nth coll index))
  ((coll index not-found)
     (-nth coll index not-found)))

(defn take (n coll)
  (sequences:take n (seq  coll)))

;; "Returns a lazy seq of every nth item in coll.  Returns a stateful
;;   transducer when no collection is provided."
(defn take-nth
    #-sbcl
    ((n)
     (fn (rf)
         (let (iv (volatile! -1))
           (fn
            (() (rf))
            ((result) (rf result))
            ((result input)
             (let (i (vswap! iv inc))
               (if (zero? (rem i n))
                   (rf result input)
                   result)))))))
    ((n coll)
      (lazy-seq
       (when-let (s (seq coll))
         (cons (first s) (take-nth n (drop n s)))))))

(defn drop (n coll)
  (sequences:drop n (seq coll)))

(defn conj
  (() +empty-pvec+)
  ((coll) coll)
  ((coll x) (-conj coll x))
  ((coll x & xs)
         (if (seq xs)
             (recur  (-conj  coll x) (first xs) (rest xs))
             (conj coll x))))

;; "disj[oin]. Returns a new set of the same (hashed/sorted) type, that
;;   does not contain key(s)."
(defn disj
  ((coll x & xs)
   (if (seq xs)
       (recur  (-disjoin  coll x) (first xs) (rest xs))
       (-disjoin coll x))))

;;need to define hierarchies.
;;these should resolve to classes or something
;;a hierarchy can pick up.
;;protocols are structs, not classes.  since they
;;don't have inheritance semantics, we can look to see
;;if the class satisfies the protocol.

;;So after reviewing CL types they're pretty cool.
;;(typep (vector 1 2) (cons 'or (protocol-members IHash)))
;;We can define protocol membership by the above..
;;We already have this equivalent in satsfies? though.
;;Can we used common-lisp:deftype to provide derivative
;;hierarchies?
;;hmm...since symbols can denote types....
;;we have to allow them.  fack
(defn isa?
    ((child parent)
     (or (identical? child parent)
         (typecase  parent
           (protocol (member child (protocol-members parent)))
           (otherwise       (common-lisp:subtypep child parent)))))
    ((h child parent)
     (throw (ex-info "Hierarchies are not implemented bro!" (hash-map :in (vector child parent))))))

(defn instance? (c x)
  (isa? (type-of x) c))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defn chunked-seq? (x) nil)
  (defn chunk-first  (coll)  (-chunked-first coll))
  (defn chunk-rest   (coll)  (-chunked-rest coll))
  (defn chunk-buffer (coll)  nil)
  (defn seq->list (xs) (sequences::seq->list (seq xs)))
  (defmacro lazy-seq (&rest body)
    `(sequences::lazy-seq ,@body))
  
  ;;generic seq printing...
  (defn print-seq
      ((s strm)
       (do (write-char #\( strm) 
           (loop (xs (-seq s))
                 (when (-seq xs)
                   (let (nxt (-rest xs))
                     (write (-first xs) :stream strm :readably nil)
                     (when (-seq  nxt) (write-char #\space strm))
                     (recur nxt))))
         (write-char #\) strm)
        nil))
    ((s) (print-seq s *standard-output*)))

  ;;looks like we need persistent lists now,
  ;;since we use metadata when parsing forms.
  ;;we have no way to derive from cons since it's
  ;;a built-in class.

  ;;we CAN wrap cons cells though, in a struct,
  ;;which has metadata, and we can define
  ;;a constant empty-list ala clojure.

  (clojure-deftype
   PersistentList (v more size _meta _hasheq)
   ISeq
   (-first (coll) v)
   (-rest (coll)  more)
   INext
   (-next (coll) (when (> size 1) more))
   ICollection
   (-conj (coll o)
          (PersistentList. o coll (inc size) nil -1))
   IMeta
   (-meta (this)  _meta)
   IWithMeta
   (-with-meta  (this newmeta)
                (PersistentList. v more size newmeta _hasheq))
   ISeqable
   (-seq (this) (when (pos? size) this))
   ICounted
   (-count (this) size)
   IString
   (-to-string (this)
               (with-output-to-string (res)
                 (print-seq this res)))) 

   (defmethod print-object ((obj PersistentList) stream)
     (print-seq obj stream))

   (def +empty-list+ (PersistentList. nil nil 0 (hash-map) -1))

  ;;temporary lame placeholder until we get better implementation.

  (defn persistent-list (&rest args)
    (if (null args)
        +empty-list+
        (let (in (nreverse args))
          (loop (remaining in
                           acc +empty-list+)
                (if remaining
                    (recur (cdr remaining)
                           (conj acc (car remaining)))
                    acc)))))

  ;;unlike persistentlist, cons is an O(1) prepend onto
  ;;a possibly unrealized lazy sequence.  We have to
  ;;realize to find out stuff.  So that means
  ;;invoking -seq on more.  similarly, size is -1
  ;;unless we compute and cache it.

  (clojure-deftype
   CljCons (v more size _meta _hasheq)
   ISeq
   (-first (coll) v)
   (-rest (coll) (if more (-seq more) +empty-list+))
   INext
   (-next (coll) (when more (-seq more)))
   ICollection
   (-conj (coll o)
          (CljCons. o coll -1 nil -1))
   IMeta
   (-meta (this)  _meta)
   IWithMeta
   (-with-meta  (this newmeta)
                (CljCons. v more size newmeta _hasheq))
   ISeqable
   (-seq (this) this)
   ICounted
   (-count (this) (if (neg? size)
                      (let (res (inc  (-count more)))
                        (set! size res)
                        res)))
   IString
   (-to-string (this)
               (with-output-to-string (res)
                 (print-seq this res))))
  

  (defmethod print-object ((obj CljCons) stream)
    (print-seq obj stream))  

  ;;"Returns a new seq where x is the first element and coll is the rest."
  (defn cons (x coll)
    (cond
      (nil? coll)             (persistent-list x)
      (implements? ISeq coll) (CljCons. x coll -1 nil -1)
      :default                (CljCons. x (seq  coll) -1 nil -1)))

  ;;we have a minor booboo, since we're blending our seq protocols
  ;;with the existing layer in sequences, we have some duplication of
  ;;effort that shows up between the generic functions from 2013 lol.
  ;;the better solution will be to define the fundamental sequence
  ;;protocols elsewhere, and refactor sequences.lisp to then leverage
  ;;a shared protocol with clclojure.base.  for now, we can work
  ;;around the legacy bolt-on problem by providing implementations for
  ;;the sequences stuff.  we could also go the clj jvm route with
  ;;inheritance, but meh.
  ;; (cl:defmethod sequences::seq ((xs CljCons))
  ;;   (-seq xs))
  ;; (cl:defmethod sequences::seq-first ((obj CljCons))
  ;;   (-first obj))
  ;; (cl:defmethod sequences::seq-rest ((obj CljCons))
  ;;   (-rest obj))

  ;;to patch around this for the time being, we'll just extend
  ;;legacy seq implementations to object and have it dispatch
  ;;on our seq protocol here. muahahaahah.
  (cl:defmethod sequences::seq ((xs t))
    (-seq xs))
  (cl:defmethod sequences::seq-first ((xs t))
    (-first xs))
  (cl:defmethod sequences::seq-rest  ((xs t))
    (-rest xs))
  ;;(defmethod sequences::empty?    ((xs t)) (-empty xs))
  (cl:defmethod sequences::internal-reduce ((xs t) f)
    (-reduce xs f))
  (cl:defmethod sequences::init-reduce   ((xs t) f init)
    (-reduce xs f init))
  )

;;TODO, since we have cons in place, we need to shadow
;;list with persistent-list, and ensure every reference
;;above is for cl:list.

(defn chunk-cons (chunk rest)
    (error 'not-implemented))

(defn chunk-append (b x)
  (error 'not-implemented))

(defn empty (coll) (-empty coll))

;; {:private true
;; :static true}
(defn spread
  (arglist)
  (cond
    (nil? arglist) nil
    (nil? (next arglist)) (seq (first arglist))
    :else (cons (first arglist) (spread (next arglist)))))

;; "Creates a new seq containing the items prepended to the rest, the
;;   last of which will be treated as a sequence."
;; {:added "1.0"
;; :static true}
;;TBD Revisit this definition, it's a bit off.
;;Since we have actual lists in common lisp...
;;do we want to lift these to seqs?  Maybe we
;;do to keep the semantics separate.
(defn list*
  ((args) (seq args))
  ((a args) (cons a args))
  ((a b args) (cons a (cons b args)))
  ((a b c args) (cons a (cons b (cons c args))))
  ((a b c d & more)
   (cons a (cons b (cons c (cons d (spread more)))))))

;; "When lazy sequences are produced via functions that have side
;;   effects, any effects other than those needed to produce the first
;;   element in the seq do not occur until the seq is consumed. dorun can
;;   be used to force any effects. Walks through the successive nexts of
;;   the seq, does not retain the head and returns nil."
;; {:added "1.0"
;; :static true}

;;TCO version is tripping here...
;; (defn dorun
;;     ((coll)
;;      (when-let (s (seq coll))
;;        (recur (next s))))
;;   ((n coll)
;;       (when (and (seq coll) (pos? n))
;;         (recur (dec n) (next coll)))))

;;temporary work around while I patch tail cail detection
;;so we stop getting false positives.
;;BEGIN COMMENT

(defn dorun
    ((coll)
     (let (s (seq coll))
       (when s
         (recur (next s)))))
  ((n coll)
      (when (and (seq coll) (pos? n))
        (recur (dec n) (next coll)))))

;; "When lazy sequences are produced via functions that have side
;;   effects, any effects other than those needed to produce the first
;;   element in the seq do not occur until the seq is consumed. doall can
;;   be used to force any effects. Walks through the successive nexts of
;;   the seq, retains the head and returns it, thus causing the entire
;;   seq to reside in memory at one time."
;; {:added "1.0"
;; :static true}

;;this is ambiguous for us right now.
(defn doall
    ((coll)
     (dorun coll)
     coll)
  ((n coll)
   (dorun n coll)
   coll))

;; "Returns the nth rest of coll, coll when n is 0."
;; {:added "1.3"
;; :static true}
(defn nthrest
  (coll n)
  (loop (n n
        xs coll)
        (if-let (xs (and (pos? n) (seq xs)))
          (recur (dec n) (rest xs))
          xs)))

;; "Returns a lazy sequence of lists of n items each, at offsets step
;;   apart. If step is not supplied, defaults to n, i.e. the partitions
;;   do not overlap. If a pad collection is supplied, use its elements as
;;   necessary to complete last partition upto n items. In case there are
;;   not enough padding elements, return a partition with less than n items."
;; {:added "1.0"
;; :static true}

;;TODO our implementation / behavior
;;of lazy seq is not identical to clojure's.
;;It's either an implementation problem or an eval problem
;;with out recursive functions.
(defn partition
    ((n coll)
        (partition n n coll))
  ((n step coll)
      (lazy-seq
       (when-let (s (seq coll))
         (let (p (doall (take n s)))
           (when (= n (count p))
             (cons p (partition n step (nthrest s step))))))))
  ((n step pad coll)
      (lazy-seq
       (when-let (s (seq coll))
         (let (p (doall (take n s)))
           (if (= n (count p))
               (cons p (partition n step pad (nthrest s step)))
               (list (take n (concat p pad)))))))))

;; "Returns a lazy sequence of lists like partition, but may include
;;   partitions with fewer than n items at the end.  Returns a stateful
;;   transducer when no collection is provided."
;; {:added "1.2"
;; :static true}
;; ((^long n)
;;         (fn (rf)
;;             (let (a (java.util.ArrayList. n))
;;               (fn
;;                (() (rf))
;;                ((result)
;;                 (let (result (if (.isEmpty a)
;;                                  result
;;                                  (let (v (vec (.toArray a)))
;;                                    ;;clear first!
;;                                    (.clear a)
;;                                    (unreduced (rf result v)))))
;;                   (rf result)))
;;                ((result input)
;;                         (.add a input)
;;                         (if (= n (.size a))
;;                             (let (v (vec (.toArray a)))
;;                               (.clear a)
;;                               (rf result v))
;;                             result))))))



(defn partition-all
  ((n coll)
      (partition-all n n coll))
  ((n step coll)
      (lazy-seq
       (when-let (s (seq coll))
         (let (seg (doall (take n s)))
           (cons seg (partition-all n step (nthrest s step))))))))


;; An iteration state value.
;; A value describing the limit of iteration, if any.
;; The from-end value.
;; A step function of three arguments: the sequence, the state value, the from-end value. The function should return the new state value.
;; An end predicate of four arguments: the sequence, the state value, the limit value, the from-end value. The function should return a generalised boolean describing whether the iteration has reached the end of the sequence.
;; An element read function of two arguments: the sequence, the state value.
;; An element write function of three arguments: the new value to store, the sequence, the state value.
;; An index function of two arguments: the sequence, the state value. The function should return the current iteration index, starting from zero.
;; An iterator copy function of two arguments: the sequence, the state value. The function should return a "fresh" iteration value.

;;We can build on this, with common-utils:hash-table-iterator, etc.
;;Might make sense to flesh out Iterables at some point, since they're generic and we
;;appear to have support for them in the built-ins.
(defn ->iterator (s)
  (multiple-value-bind
        (state from-end step end? read-elt write-elt index copy)
      (sb-sequence:make-sequence-iterator s)
    (hash-map  :state state
               :from-end from-end
               :step step
               :end? end?
               :read-elt  read-elt
               :write-elt write-elt
               :index     index
               :copy      copy)))


;;need early return.  we have this implemented in sequences.lisp..
(defn seq-reduce
    ((f coll)
     (loop (acc (first coll)
            xs  (rest coll))
           (if (seq xs)
                       (recur (f acc (first xs))
                              (rest xs))
                       acc)))
  ((f init coll)
   (loop (acc init
          xs  coll)
         (if (seq xs)
             (recur (f acc (first xs))
                    (rest xs))
             acc))))

(extend-protocol
 IReduce
 PersistentList
 (-reduce (coll f)
          (seq-reduce f coll))
 (-reduce (coll f start)
          (seq-reduce f  start coll))
 CljCons
 (-reduce (coll f)
          (seq-reduce f coll))
 (-reduce (coll f start)
          (seq-reduce f  start coll))
 )
                 
;;we temporarily wrap the implementation in sequences.lisp.
;;can also define our seq-reduce here on our protocols...
;;this goes back to rewriting/refactoring sequences.lisp,
;;it's legacy cruft.
;;maybe we unify under iterables.
(defn reduce
    ((f coll)
        (if (sequences:internal-reduce? coll)
            (sequences:reduce f coll)
            (sequences:reduce f (seq coll))))
  ((f init coll)
      (if (sequences:init-reduce? coll)
          (sequences:reduce f init coll)
          (sequences:reduce f init (seq coll)))))

;; "Returns a new coll consisting of to-coll with all of the items of
;;   from-coll conjoined. A transducer may be supplied."
;; {:added "1.0"
;; :static true}
(defn into
    (() +empty-pvec+)
  ((to) to)
  ((to from)
       (if nil ;(instance? clojure.lang.IEditableCollection to)
           (with-meta (persistent! (reduce conj! (transient to) from)) (meta to))
           (reduce conj to from)))
  ;; ((to xform from)
  ;;      (if nil ;(instance? clojure.lang.IEditableCollection to)
  ;;          (with-meta (persistent! (transduce xform conj! (transient to) from)) (meta to))
  ;;          (transduce xform conj to from)))
  )
;; "Returns a lazy sequence consisting of the result of applying f to
;;   the set of first items of each coll, followed by applying f to the
;;   set of common-lisp:second items in each coll, until any one of the colls is
;;   exhausted.  Any remaining items in other colls are ignored. Function
;;   f should accept number-of-colls arguments. Returns a transducer when
;;   no collection is provided."
;; {:added "1.0"
;; :static true}


(defn map
    ;;temporarily on hold while we fix tail recur detection.
    ;; ((f)
    ;;  (fn (rf)
    ;;      (fn
    ;;       (() (rf))
    ;;       ((result) (rf result))
    ;;       ((result input)
    ;;                (rf result (f input)))
    ;;       ((result input & inputs)
    ;;                (rf result (apply f input inputs))))))
    ((f coll)
        (sequences:map f (seq coll)))
  ((f c1 c2)
      (sequences:map f (seq c1) (seq c2)))
  ((f c1 c2 c3)
      (sequences:map f (seq c1) (seq c2) (seq c3)))
  ((f c1 c2 c3 & colls)
      (sequences:map f (seq c1) (seq c2) (seq c3) colls)
      ))

;;lame, but a little exercise in bindings...
(defmacro dotimes (binding &rest body)
  `(common-lisp:dotimes (,(first binding) ,(second binding))
     ,@body))

;; "Returns a lazy sequence of the items in coll for which
;;   (pred item) returns logical true. pred must be free of side-effects.
;;   Returns a transducer when no collection is provided."
;; {:added "1.0"
;; :static true}
;;(defn filter
;; ((pred)
;;  (fn (rf)
;;      (fn
;;       (() (rf))
;;       ((result) (rf result))
;;       ((result input)
;;                (if (pred input)
;;                    (rf result input)
;;                    result)))))
;; (pred coll)
;; (lazy-seq
;;  (when-let (s (seq coll))
;;    (if (chunked-seq? s)
;;        (let (c (chunk-first s)
;;          size (count c)
;;          b (chunk-buffer size))
;;          (dotimes (i size)
;;            (let (v (nth c i))
;;              (when (pred v)
;;                (chunk-append b v))))
;;          (chunk-cons (chunk b) (filter pred (chunk-rest s))))
;;        (let (f (first s)
;;              r (rest s))
;;          (if (pred f)
;;              (cons f (filter pred r))
;;              (filter pred r)))))))

;;lame filter for now.

(defn filter (predicate coll)
  (lazy-seq
   (when-let (s (seq coll))
     (let (f  (first s)
           r  (rest s))
       (if (funcall predicate f)
           (cons f (filter predicate r))
           (filter predicate r))))))

(defn concat
    (() nil)
  ((x) x)
  ((x y) (sequences:concat (seq x) (seq y)))
  ((x y & zs)
      (sequences:apply #'sequences:concat
                       (sequences:map #'seq
                                      (common-lisp:list* x y zs)))))

(defn every? (pred xs)
  (sequences::every? pred xs))

(def identity #'common-lisp:identity)

;; "Returns a lazy seq of the first item in each coll, then the common-lisp:second etc."
;; {:added "1.0"
;; :static true
(defn interleave
  (() nil)
  ((c1) (lazy-seq (seq  c1)))
  ((c1 c2)
       (lazy-seq
        (let (s1 (seq c1) s2 (seq c2))
          (pprint (list :s1 (first s1) :s2 (first s2)))
          (when (and s1 s2)
            (cons (first s1) (cons (first s2) 
                                   (interleave (rest s1) (rest s2))))))))
  ((c1 c2 & colls)        
       (let (ss (map seq (conj colls c2 c1)))
         (when (every? identity ss)
           (concat (map first ss) (lazy-seq  (apply interleave (map rest ss))))))))

;;need destructure...

;;TBD update these to new classes...
(defn symbol? (x)  (or (typep x 'cljsymbol)
                       (symbolp x)))
(defn keyword? (x) (or (typep x 'cljkey)
                       (keywordp x)))
(defn string? (x) (stringp x))
(defn map? (x) (implements? IMap x)) ;;don't like this.  seems too loose.
(defn set? (x) (implements? ISet x))
(defn number? (x) (numberp x))
;;this is overloaded for cljs though. hmm.
;;aref is more generic; svref is probably closer in semantics...
;;TODO: this should be symbol macro'd or inlined maybe?
(declaim (inline aget))
(defn aget (x idx) (aref x idx))
(declaim (inline aset))
(defn aset (x idx v)
  (setf (aref x idx) v))
(declaim (inline peek pop))
(defn peek (coll) (-peek coll))
(defn pop (coll) (-pop coll))
;;very lame placeholders.
;;this is similar to what cljs does, although
;;these are ctors for KeySeq and ValSeq.
;;we'll port those later.
(defn keys (m)
  (->> m
       (-seq)
       (map -key)))
(defn vals (m)
  (->>  m
        (-seq)
        (map -val)))
;;for now, we don't have qualified keywords...
;;we "could" encode that information in the
;;keyword name somewhere (like a central db),
;;but it seems better to implement the actual
;;clojure qualified keywords...
;;wonder if we can inherit from symbol..(nope!).


;; (defn namespace (s)
;;   (when (symbol? s)
;;     (when-let (p (symbol-package s))
;;       (package-name p))
;;     ))


;; (comment 

;; (defmacro fn  (& sigs) 
;;   (let* ((name (if (symbol? (first sigs)) (first sigs) nil)
;;          sigs (if name (next sigs) sigs)
;;          sigs (if (vector? (first sigs)) 
;;                  (list sigs) 
;;                  (if (seq? (first sigs))
;;                    sigs
;;                    ;; Assume single arity syntax
;;                    (throw (IllegalArgumentException. 
;;                             (if (seq sigs)
;;                               (str "Parameter declaration " 
;;                                    (first sigs)
;;                                    " should be a vector")
;;                               (str "Parameter declaration missing"))))))
;;           psig (fn* (sig)
;;                  ;; Ensure correct type before destructuring sig
;;                  (when (not (seq? sig))
;;                    (throw (IllegalArgumentException.
;;                             (str "Invalid signature " sig
;;                                  " should be a list"))))
;;                  (let ((params & body) sig
;;                        _ (when (not (vector? params))
;;                            (throw (IllegalArgumentException. 
;;                                     (if (seq? (first sigs))
;;                                       (str "Parameter declaration " params
;;                                            " should be a vector")
;;                                       (str "Invalid signature " sig
;;                                            " should be a list")))))
;;                        conds (when (and (next body) (map? (first body))) 
;;                                            (first body))
;;                        body (if conds (next body) body)
;;                        conds (or conds (meta params))
;;                        pre (:pre conds)
;;                        post (:post conds)                       
;;                        body (if post
;;                               `((let (~'% ~(if (< 1 (count body)) 
;;                                             `(do ~@body) 
;;                                             (first body)))
;;                                  ~@(map (fn* (c) `(assert ~c)) post)
;;                                  ~'%))
;;                               body)
;;                        body (if pre
;;                               (concat (map (fn* (c) `(assert ~c)) pre) 
;;                                       body)
;;                               body))
;;                    (maybe-destructured params body)))
;;           new-sigs (map psig sigs))
;;       (with-meta
;;         (if name
;;           (list* 'fn* name new-sigs)
;;           (cons 'fn* new-sigs))
;;         (meta &form))))
;; )

;;conflicts with :common-lisp

(defn rest-arg? (x)
  (seql x '&))

;;need to implement "new" eventually
;;it's a low-level interop construct.

(defmacro new (klass &rest args)
  `(make-instance (quote ,klass) ,@args))

;;I think we can implement . and .. in terms of
;;slot-value and friends.
;;we can't get . because of the reader right now,
;;but we can implement it.
;; BASE> (-.  (TransientHashSet. (transient (hash-map :a true :b true))) transient-map)
;; {:A T :B T}

;;implementation of clojure's . special form on top of CLOS.
;;with the generic slot-access provided by reflection::slot,
;;we can have interchangeable code with clojure where
;;we don't care about interned symbols (rather, we can
;;handle the case where we have symbol-name = symbols
;;for a slot, e.g. invoking from a symbol interned in a different
;;package).
;;e.g., we can access the internal table slot for a cowmap via:
;;(-. (hash-map :a 2) :table)
;;or
;;(-. (hash-map :a 2) table)
;;and similarly, pull out nested slots:
;; (-.. (hash-map :a 2) :table :test)
;; (-.. (hash-map :a 2) table test)
(defmacro -. (instance member &rest args)
  (if (null args)
      `(reflection::slot ,instance ',member)
      `(funcall (reflection::slot ,instance ',member) ,@args)))

;; "form => fieldName-symbol or (instanceMethodName-symbol args*)

;;   Expands into a member access (.) of the first member on the first
;;   argument, followed by the next member on the result, etc. For
;;   instance:

;;   (.. System (getProperties) (get \"os.name\"))

;;   expands to:

;;   (. (. System (getProperties)) (get \"os.name\"))

;;   but is easier to write, read, and understand."
;;I think this is equivalent, could be wrong.
(defmacro -.. (x form &rest more)
  (labels ((aux (x frm remaining)
             (if (null remaining)
                 `(-. ,x ,frm)
                 (aux `(-. ,x ,frm) (cl:first remaining) (cl:rest remaining)))))
    (aux x form more)))

;;interesting note on slot-value.  we can try to do runtime reflection
;;on instances if the package-local symbol can't be resolved, e.g. if the
;;type is unspecified.  This equates to using sb-mop:class-direct-slots
;;to try to find a slot-name compatible with the symbol-name value,
;;which elides the package-name junk.  We can handle this at compile
;;time if we know the type of course (assuming nothing in CLOS changes
;;out from under us with the classes).
;;clojure will end up doing this a lot, since untyped reflection
;;is a thing.

;;lame multimethods?
;;we need a methodcache
;;dispatch fn

(defparameter *default-hierarchy* nil)
;;we can pack the multimethod into a struct.
;;if we put this in class, we can make it funcallable.  hmm.
;;don't really care to right now, but it's possible...
(defstruct multimethod name doc meta default dispatch-fn methodcache hierarchy)
;;ignore hierarchy for now, keep it simple with doc and meta.
(defun make-multi (name dispatch-fn &key doc meta default hierarchy)
  (make-multimethod :name name
                    :doc (or doc "")
                    :default (or default :default)
                    :dispatch-fn dispatch-fn
                    :meta (or meta +empty-cowmap+)
                    :methodcache +empty-cowmap+
                    :hierarchy (or hierarchy *default-hierarchy*)))

(extend-protocol
 IFn
 multimethod
 (-invoke ((this &rest args)
           (with-slots (methodcache dispatch-fn default) this
             (let (dv          (apply dispatch-fn args)
                   method-impl (or (get methodcache  dv)
                                   (get methodcache default)))
               (if method-impl
                   (apply method-impl args)
                   (throw (ex-info "no dispatch value found and no default for multimethod!"
                                   (hash-map :name (multimethod-name this)
                                             :args args)))))))))

;;we'll scrape this out better later, for now we'll just force a name and dispatch
;;fn.
(defmacro defmulti (name dispatch &key default hierarchy)
  (let (dfn     (gensym "dfn")
        multifn (gensym "multifn")
        args (gensym "args"))
    `(let (,dfn ,dispatch
           ,multifn (make-multi ',name ,dfn
                                :default   (or ,default :default)
                                :hierarchy (or ,hierarchy *default-hierarchy*)))
       (def ,name ,multifn)
       (setf (symbol-function (quote ,name))
             ;;terrible compromise but meh.  we were nesting args too
             ;;much during apply/invoke....we just inline it here for now
             ;;until I get smarter.
             (fn (&rest ,args)
                 (with-slots (,'methodcache ,'dispatch-fn ,'default) ,name
                   (let (,'dv          (apply ,'dispatch-fn ,args)
                         ,'method-impl (or (get ,'methodcache  ,'dv)
                                           (get ,'methodcache ,'default)))
                     (if ,'method-impl
                         (apply ,'method-impl ,args)
                         (throw (ex-info "no dispatch value found and no default for multimethod!"
                                         (hash-map :name (multimethod-name ,name)
                                                   :args ,args))))))))
       ,name)))

(defun push-method (mf k func)
  (with-slots (methodcache) mf
    (setf methodcache (assoc methodcache k func))))

;;need to define/wrap defmethod as we did with deftype at
;;some point.  this is fine for now.
(defmacro defmethod-clj (name dispatch-val args &rest body)
  (let (df (gensym "dispatch-fn"))
    `(let (,df (fn (,@args) ,@body))
       (push-method ,name ,dispatch-val ,df))))

(comment ;;testing my precious
   (defmulti mf (fn (x) (type-of x)))
   (defmethod-clj mf :default (x) (+ x 1))
   (-invoke mf 1)
   (mf 1)
   )

;;atoms and reference types from clj-con
;;looks like most of these can be exported directly.
(common-lisp:deftype clclojure.base:atom () 'clj-con:atom)

(extend-protocol
 IDeref
 clj-con:future
 (-deref (this) (clj-con:deref this))
 clj-con:atom
 (-deref (this) (clj-con:deref this))
 clj-con:promise
 (-deref (this) (clj-con:deref this))
 )

(extend-protocol
 IPending
 clj-con:future
 (-realized? (this) (clj-con:realized? this))
 clj-con:atom
 (-realized? (this) (clj-con:realized? this))
 clj-con:promise
 (-realized? (this) (clj-con:realized? this))
 )

(defn deref     (this) (-deref this))
(defn realized? (this) (-realized? this))
(defn atom      (v)    (clj-con:atom v))

;;exported directly from clj-con for us.
;; atom?             
;; compare-and-set!  
;; deliver           
;; deref             
;; future            
;; future-call       
;; future-cancel     
;; future-cancelled? 
;; future-done?      
;; future?           
;; promise           
;; realized?         
;; reset!            
;; reset-vals!       
;; swap!             
;; swap-vals! 

(defn char (x)
  (typecase x
    (common-lisp:standard-char x)
    (integer (code-char x))
    (otherwise (throw (ex-info "cannot coerce to char!") (hash-map :in x)))))

(defn char? (x) (typep x 'common-lisp:standard-char))

;; (defn keys (x)
;;   (->> x seq (map first)))

(defn emit-copy-instance (name old args)
  `(make-instance ',name
                  ,@(->> args
                         (map (fn (x) (list (alexandria:make-keyword x)
                                            `(slot-value ,old ',x)) ))
                         (reduce (fn (acc xy)
                                     (-> acc
                                         (-conj (first xy))
                                         (-conj (second xy)))) +empty-pvec+)
                         (as-list))))

;;fn is having a hard time in some meta programming, probably due to
;;labels and self-naming for recur.
;;we already have slots accessible, I forgot that when I originally
;;implemented this.  Since clojure-deftype takes care of slot value
;;access via with-slots, we already have them available via lexical scoping.
;;We're just redundant (not hurting anything) with the longer (slot-value this 'fld)
;;idiom.
(defun emit-record-impls (nm args)
  (let (this     (gensym "this")
        res      (gensym "newrec")
        v        (gensym "v")
        ext      '_ext
        meta     '_meta
        all-args (into '()  (concat args (list ext meta)))
        lookups  (into '() (map (fn (x) (list  (alexandria:make-keyword x)
                                               `(slot-value ,this ',x))) args))
        adds     (into '() (map (lambda (x)
                                    (list  (alexandria:make-keyword x)
                                           `(let (,res ,(emit-copy-instance `,nm `,this all-args))
                                              (setf (slot-value ,res ',x) ,v)
                                              ,res))) args))
        nargs (count args))
    (with-gensyms (k not-found exists res newmeta)
      `(ILookup
        (-lookup (,this ,k)
                 (cl:case ,k
                   ,@lookups              
                   (otherwise (get ,ext ,this))))
        (-lookup (,this ,k ,not-found)
                 (cl:case ,k
                   ,@lookups             
                   (otherwise
                    (multiple-value-bind (,v ,exists)
                        (get (slot-value ,this ',ext) ,k)
                      (if ,exists ,v ,not-found)))))
        IAssociative
        (-contains-key? (,this ,k)
                        (let (,res (-lookup ,this ,k :not-found) )
                          (not (eq res :not-found)))) ;;brittle
        (-entry-at (,this k)
                   (let (,res (-lookup ,this ,k :not-found))
                     (when (not (eq ,res :not-found))
                         (vector ,k ,res))))
        (-assoc (,this ,k ,v)
                (cl:case ,k
                  ,@adds
                  (otherwise (let (,res ,(emit-copy-instance `,nm `,this all-args))
                               (setf (slot-value ,res ',ext) (assoc (slot-value ,this ',ext) ,k ,v))
                               ,res))))
        IMap
        (-assoc-ex (,this ,k ,v)
                   (throw (ex-info "not-implemented" nil)))
        (-dissoc   (,this ,k)
                   (if (member ,k  (list ,@(mapcar #'alexandria:make-keyword  args)))
                       (throw (ex-info "dissoc on core fields is WIP" (hash-map)))
                       (let (,res ,(emit-copy-instance `,nm `,this all-args))
                         (setf (slot-value ,res ',ext) (dissoc (slot-value ,this ',ext) ,v))
                         ,res)))
        IMeta
        (-meta (,this) (slot-value ,this ',meta))

        IWithMeta
        (-with-meta  (,this ,newmeta)
          (let (,res ,(emit-copy-instance `,nm `,this all-args))
            (setf (slot-value ,res ',meta) ,newmeta)
            ,res))
        ISeqable
        (-seq (,this) ;;we get a little nag with type specificity for hash entries for now.
              (concat (list ,@(mapcar (lambda (x) (list 'list  (alexandria:make-keyword x)
                                                        `(slot-value ,this ',x))) args))
                      (common-utils:hash-table->entries (cowmap-table (slot-value ,this ',ext) )))
              )
        ICounted
        (-count (,this) (+ ,nargs (count (slot-value ,this ',ext))) )))))


;;limited defrecord impl.
;;full impl would be in
;;https://github.com/clojure/clojurescript/blob/master/src/main/clojure/cljs/core.cljc#L1837
;;we might want to start binding classes to vars when we clojure-deftype...
;;we want to eliminate the original ctor from deftype prior to eval.
;;it's spooking the compiler.
(defmacro defrecord (name args &rest impls)
  (let (all-args (nreverse  (into '() (concat args '(_ext _meta))))
        ctor (intern  (str  "->" name ))
        ks   (map (fn (x) (alexandria:make-keyword x)) args))
    `(progn  (clojure-deftype ,name
                              ,all-args
                              ,@impls
                              ,@(emit-record-impls name args)
                              )
             (defn ,ctor (,@args)
               (make-instance ',name ,@(into '() (interleave  args ks))
                              :_ext +empty-cowmap+
                              :_meta +empty-cowmap+))
             (defmethod print-object ((,'obj ,name) ,'stream) ;;mildy janky.
               (format stream "#~A.~A{~{~s~^ ~}}" (namespace ',name) ',name
                  (mapcan identity (reverse  (into '()  (seq ,'obj))))))
             ',name)))

;;moved to later so we build on partition-all and reduce.
(defn assoc
  ((m k v)
     (-assoc m k v))
  ((m k v &rest kvs)
   (reduce (fn (acc kv)
               (-assoc acc (first kv) (second kv)))
           (-assoc m k v) (partition-all 2 kvs))))

(defn dissoc
    ((m k)       (-dissoc m k))
  ((m k & ks)
   (reduce (fn (acc k)
               (-dissoc acc k))
           (-dissoc m k) ks)))

;;pretty sure this is print-readably aka pr in clojure.
;;unsure what opts are at the moment.  probably
;;stuff like flush-on-newline and friends....
(defn pr-writer (obj writer opts)
  (-pr-writer obj writer opts))

(extend-protocol
 IWriter
 STREAM
 (-write (writer s)  (common-lisp:write  s :stream  writer))
 ;;I think this is correct, dunno.
 (-flush (writer)     (common-lisp:finish-output writer)))


;; "Takes a set of functions and returns a fn that is the juxtaposition
;;   of those fns.  The returned fn takes a variable number of args, and
;;   returns a vector containing the result of applying each fn to the
;;   args (left-to-right).
;;   ((juxt a b c) x) => [(a x) (b x) (c x)]"

;;this works, but fn doesn't like having a non-list for a body
;;with 0 args.
(defn juxt (f &rest fs)
  (let (all (list* f fs))
    (fn  (&rest xs)
         (vec  (map (lambda (f) (apply f xs)) all)))))

;; "Returns the first logical true value of (pred x) for any x in coll,
;;   else nil.  One common idiom is to use a set as pred, for example
;;   this will return :fred if :fred is in the sequence, otherwise nil:
;;   (some #{:fred} coll)"
(defn some (pred coll)
  (when-let (s (seq coll))
    (or (funcall pred (first s)) (recur pred (next s)))))

;; "Returns a map that consists of the rest of the maps conj-ed onto
;;   the first.  If a key occurs in more than one map, the mapping from
;;   the latter (left-to-right) will be the mapping in the result."
(defn merge (&rest maps)
  (when (some identity maps)
    (reduce (fn (acc m) (conj (or acc (hash-map)) m)) maps)))

;;let's define a stringbuilder to support reader ops.
(clojure-deftype
 StringBuilder (buff)
 IString
 (-to-string (this) buff)
 ICounted
 (-count (this) (length buff))
 INamed
 (-name (x) buff)
 IIndexed
 (-nth (coll n) (common-lisp:char buff n)) ;;TODO: schar optimization option?
 (-nth (coll n not-found)
       (if (< n (length buff))
           (common-lisp:char buff n)
           not-found))
 ISeqable
 (-seq (coll) (sequences::seq buff))
 ISeq
 (-first (coll)  (cl:char buff 0))
 (-rest  (coll)  (sequences::rest buff))
 IEquiv
 (-equiv (this other)
         (and (stringp other) (string-equal buff other)))
 ICollection
 (-conj (this v)
        (set! buff (str buff v))
        this))

(defn ->string-builder (&rest args)
  (stringbuilder. (apply #'str args)))

;;"Creates an array of objects"
(defn object-array (size-or-seq)
  (if (numberp size-or-seq)
      (make-array size-or-seq)
      (let (n (count size-or-seq) 
            idx -1)
        (->> size-or-seq
             (reduce (fn (acc x)
                         (aset acc (incf idx) x)
                         acc)
                     (make-array n))))))

;; "'Updates' a value in an associative structure, where k is a
;;   key and f is a function that will take the old value
;;   and any supplied args and return the new value, and returns a new
;;   structure.  If the key does not exist, nil is passed as the old value."
(defn update
  ((m k f)
      (assoc m k (funcall f (get m k))))
  ((m k f x)
      (assoc m k (funcall f (get m k) x)))
  ((m k f x y)
      (assoc m k (funcall f (get m k) x y)))
  ((m k f x y z)
      (assoc m k (funcall f (get m k) x y z)))
  ((m k f x y z &rest more)
      (assoc m k (apply f (get m k) x y z more))))

;; "'Updates' a value in a nested associative structure, where ks is a
;;   sequence of keys and f is a function that will take the old value
;;   and any supplied args and return the new value, and returns a new
;;   nested structure.  If any levels do not exist, hash-maps will be
;;   created."
(defn update-in
  (m ks f & args)
  (let (up (fn up (m ks f args)
               (let (k (first ks)
                     ks (rest ks))
                 (if ks
                     (assoc m k (up (get m k) ks f args))
                     (assoc m k (apply f (get m k) args))))))
    (up m ks f args)))

;;this screws up a lot of stuff.
;;we could do some goofiness and actually give it a var ...
;;then symbol-function/symbol-value it.  ugh, I don't
;;want to mess with it.
(defn partial (f  &rest args)
  (lambda (&rest more)
    (apply f (concatenate 'list args more))))

;;"Returns a lazy seq of the first item in each coll, then the second etc."
(defn interleave
  (() nil)
  ((c1) (lazy-seq c1))
  ((c1 c2)
       (lazy-seq
        (let (s1 (seq c1) s2 (seq c2))
          (when (and s1 s2)
            (cons (first s1) (cons (first s2)
                                   (interleave (rest s1) (rest s2))))))))
  ((c1 c2 &rest colls)
       (lazy-seq
        (let (ss (map seq (conj colls c2 c1)))
          (when (every? identity ss)
            (concat (map first ss) (apply interleave (seq->list  (map rest ss)))))))))

;;we'll do a hack job for now.
(defn repeat
  ((v) (sequences:iterate identity v))
  ((n v) (take n (sequences:iterate identity v))))

;; "Returns a lazy seq of the elements of coll separated by sep.
;;   Returns a stateful transducer when no collection is provided."
(defn interpose
    ((sep coll)
        (drop 1 (interleave (repeat sep) coll))))

;;cheap stand-in, copy-on-write hashset built on hashmaps.
;;good enough for bootstrapping and implementation can be replaced trivially.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clojure-deftype cowset (entries _meta _hasheq)
                   ISeq
                   (-first (coll) (first (-seq coll)))
                   (-rest (coll)  (rest (-seq coll)))
                   INext
                   (-next (coll) (next (-seq coll)))
                   ICollection
                   (-conj (coll o)
                          (let (res (-assoc entries o o))
                            (if (identical? res entries)
                                coll
                                (cowset. res _meta -1))))
                   ILookup
                   (-lookup (this k)
                            (-lookup entries k))
                   (-lookup (this k not-found)
                            (-lookup entries k not-found))
                   IMeta
                   (-meta (this)  _meta)
                   IWithMeta
                   (-with-meta  (this newmeta)
                                (cowset. entries newmeta -1))
                   ISeqable
                   (-seq (this)
                         (->> (-seq entries)
                              (map first)))
                   ICounted
                   (-count (this)
                           (-count entries))
                   ISet
                   (-disjoin (coll v)
                             (let (res (dissoc entries v))
                               (if (identical? res entries)
                                   coll
                                   (cowset. res _meta -1))))
                   IString
                   (-to-string (this)
                               (str "#{" (apply #'str (interpose " " (-seq this))) "}")))

  (defmethod print-object ((obj cowset) stream)
    (format stream "#{~A}" (apply #'str (interpose " " (-seq obj))))))

(def +empty-set+ (cowset. (hash-map) (hash-map) -1))

;;temporary lame placeholder until we get better implementation.
(defn hash-set (&rest args)
  (cowset. (apply #'hash-map (mapcan (lambda (x) (list x x)) args))
           (hash-map) -1))

;;adapted from cljs core.
(clojure-deftype
 TransientHashSet
 (transient-map)
  ITransientCollection
  (-conj! (tcoll o)
          (set! transient-map (-assoc! transient-map o nil))
          tcoll)

  (-persistent! (tcoll)
                #-sbcl(PersistentHashSet. nil (-persistent! transient-map) nil)
                (cowset. (-persistent! transient-map) nil -1))

  ITransientSet
  (-disjoin! (tcoll v)
             (set! transient-map (-dissoc! transient-map v))
             tcoll)

  ICounted
  (-count (tcoll) (-count transient-map))

  ILookup
  (-lookup (tcoll v)
           (-lookup tcoll v nil))

  (-lookup (tcoll v not-found)
           (if (identical? (-lookup transient-map v lookup-sentinel) lookup-sentinel)
               not-found
               v))

  IFn
  (-invoke (tcoll k)
           (if (identical? (-lookup transient-map k lookup-sentinel) lookup-sentinel)
               nil
               k))

  (-invoke (tcoll k not-found)
           (if (identical? (-lookup transient-map k lookup-sentinel) lookup-sentinel)
               not-found
               k)))
;;can change this later.  clojure doesn't have transients
;;readable on purpose.  we flake out a bit for now for testing.
(defmethod print-object ((obj TransientHashSet) stream)
  (->>  (slot-value obj 'transient-map)
        (keys)
        (interpose " ")
        (apply #'str )
        (format stream "#TransientHashSet{~A}")))

;;"Returns a set of the distinct elements of coll."
(defn set
  (coll)
  (if (set? coll)
      (with-meta coll nil)
      (persistent! (reduce -conj! (transient +empty-set+) coll))))

(defn frequencies (xs)
  (reduce (fn (acc x)
              (if-let (n (get acc x))
                (update acc x inc)
                (assoc acc x 1)))
          (hash-map) xs))

(defmacro if-not (test then &optional else)
  `(if (not ,test) ,then ,(or  else nil)))

(defmacro when-first  (bindings &rest body)
  ;; (assert-args
  ;;  (vector? bindings) "a vector for its binding"
  ;;  (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let ((x xs) bindings)
    (with-gensyms (xs#)
      `(when-let (,xs# (seq ,xs))
         (let (,x (first ,xs#))
           ,@body)))))

;; "Evaluates x then calls all of the methods and functions with the
;;   value of x supplied at the front of the given arguments.  The forms
;;   are evaluated in order.  Returns x.

;;   (doto (new java.util.HashMap) (.put \"a\" 1) (.put \"b\" 2))"
;;   we can't do meta on forms here, since we have to have persistent lists
;;   in defmacro, and we aren't there "yet".  Persistent Lists can't be eval'd
;;   in sbcl right now (don't feel like hacking the evaluator either),
;;   but metadata won't typically matter for eval, really for special forms
;;   and macros.  We can probably ditch it for now.
#-sbcl
(defmacro doto (x &rest forms)
  (let (gx (gensym))
    `(let (,gx ,x)
       ,@(seq->list (map (fn (f)
                             (with-meta
                                  (if (seq? f)
                                      `(,(first f) ,gx ,@(next f))
                                      `(,f ,gx))
                                (meta f)))
                          forms))
       ,gx)))
(defmacro doto (x &rest forms)
  (let (gx (gensym))
    `(let (,gx ,x)
       ,@(seq->list
                (map (fn (f)
                             (if (seq? f)
                                 `(,(first f) ,gx ,@(next f))
                                 `(,f ,gx)))
                         forms))
       ,gx)))

;;this is a loose hack for now, but it works as a
;;placeholder.
;;we have to implement the gamut of readers in
;;clojure.java.io. not hard, but tedious for rn.
(defn slurp (f & opts)
  (uiop/stream:read-file-string f))

;;need readers for line-seq.
;;we have an adaptation in clojure.tools.reader, but not
;;quite enough for line-seq yet.

;;transient placeholders. we'll just have mutable
;;junk for now.

;;NOTE: right now extend-protocol and extend-typ
;;will crap out
;;on classes that have type specializers like
;;simple-vector for reasons, but extend-type works.
;;works fine with single-function protocols though, huh..

;;compatibility with generic CL sequences.
(defn sequencep (coll)
  (isa? (type-of coll) 'sequence))

;;this isn't in clojure proper, but it's helpful
;;for working with mutable adjustable arrays.
(defn array-list (size-or-seq)
  (cond (number? size-or-seq)
        (let (size size-or-seq)
          (make-array size  :adjustable t :fill-pointer 0))
        (sequencep size-or-seq)
        (let (size (length size-or-seq))
          (make-array size :adjustable t :fill-pointer size :initial-contents size-or-seq))
        ;;should check if it's an array...
        (seq size-or-seq)
        (let (size (count size-or-seq))
          (make-array size :adjustable t :fill-pointer size :initial-contents (seq->list size-or-seq)))
        :else (throw (ex-info "expected a size or seq!" (hash-map :in size-or-seq)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (extend-protocol IEditableCollection
     clclojure.pvector::pvec
     (-as-transient (coll) (clclojure.pvector:vector-to-array coll))
     clclojure.pvector::subvector
     (-as-transient (coll) (clclojure.pvector:vector-to-array coll))
     clclojure.cowmap::cowmap
     (-as-transient (coll)
        (common-utils::copy-hash-table
         (slot-value coll 'clclojure.cowmap::table)))
     clclojure.base::cowset
     (-as-transient (coll)
                    (TransientHashSet. (-as-transient  (slot-value coll 'entries)))) )
  (extend-type 
   common-lisp:vector
   ITransientCollection
   (-conj! (tcoll val)
          (vector-push val tcoll) tcoll)
   (-persistent! (tcoll)
                 (into (vector) (seq  tcoll))) ;;LAME, but meh.
   ITransientAssociative
   (-assoc! (tcoll key val)
           (if (cl:= key (length tcoll))
               (cl:vector-push val tcoll)
               (setf (elt tcoll key) val))
           tcoll)
   ITransientVector
   (-assoc-n! (tcoll n val)
              (if (cl:= n (length tcoll))
                  (cl:vector-push val tcoll)
                  (setf (elt tcoll n) val))
              tcoll)
   (-pop! (tcoll) (cl:vector-pop tcoll) tcoll))
  
  (extend-type 
   hash-table ;;hash-map
   ITransientCollection
   (-conj!       (tcoll val)
                 (if (=  (count val) 2)
                     (do (setf (gethash (nth val 1) tcoll) (nth val 0))
                         tcoll)
                     (throw (ex-info "expected a vector or list or map entry!" (hash-map :in val)))))
   (-persistent! (tcoll)
                 (into (hash-map)   (common-utils:hash-table->entries tcoll)))
   ITransientAssociative
   (-assoc! (tcoll key val)
            (progn  ;;odd that we need this, should be implicit.
              (setf (gethash key tcoll) val)
              tcoll))
   ITransientMap
   (-dissoc! (tcoll key)
             (progn 
               (remhash key tcoll)
               tcoll))))

(defn transient (coll)
  (-as-transient coll))

(defn persistent! (coll)
  (-persistent! coll))

;;fwiw, it looks like the binding form from clojure is already
;;handled in let, since CL does dynamic binds / special variables
;;natively.  I think the only area we might want to mess with this
;;is if/when we have per-thread bindings, which is where we need
;;to investigate a little more.  I know clojure has a set of bindings
;;per thread that are copied around; I think CL does something similarly,
;;but I'm weak on the semantics.  It doesn't matter for bootstrapping
;;right now, but we'll account for it with a placeholder.
;;https://lispcookbook.github.io/cl-cookbook/process.html

;; (defmacro binding
;;   {:added "1.0"}
;;   [bindings & body]
;;   (assert-args
;;    (vector? bindings) "a vector for its binding"
;;    (even? (count bindings)) "an even number of forms in binding vector")
;;   (let [var-ize (fn [var-vals]
;;                     (loop [ret [] vvs (seq var-vals)]
;;                           (if vvs
;;                               (recur  (conj (conj ret `(var ~(first vvs))) (second vvs))
;;                                       (next (next vvs)))
;;                               (seq ret))))]
;;     `(let []
;;        (push-thread-bindings (hash-map ~@(var-ize bindings)))
;;        (try
;;         ~@body
;;         (finally
;;          (pop-thread-bindings))))))

;;   "binding => var-symbol init-expr

;;   Creates new bindings for the (already-existing) vars, with the
;;   supplied initial values, executes the exprs in an implicit do, then
;;   re-establishes the bindings that existed before.  The new bindings
;;   are made in parallel (unlike let); all init-exprs are evaluated
;;   before the vars are bound to their new values."
;;Need to [eventually] implement push-thread-bindings, pop-thread-bindings

#-sbcl
(defmacro push-thread-bindings (binds &rest body))
#-sbcl
(defmacro pop-thread-bindings ())

;;we just substitute let for now, which should work fine with
;;special variables (dynamic vars).
(defmacro binding (binds &rest body)
  `(clclojure.base:let ,binds ,@body))

(defn counted? (coll)
  (implements? ICounted coll))
;; "Returns true if coll has no items. To check the emptiness of a seq,
;;   please use the idiom (seq x) rather than (not (empty? x))"
;; {:added "1.0"
;; :static true}
(defn empty? (coll) 
  (if (counted? coll)
      (zero? (count coll))
      (not (seq coll))))


;; (defprotocol ITransientSet
;;     (-disjoin! (tcoll v)))

;;destructuring junk.  not important yet.
;; (defn ds-pvec (bvec b val)
;;   (let (gvec (gensym "vec__")
;;     gseq (gensym "seq__")
;;     gfirst (gensym "first__")
;;     has-rest (some rest-arg? b))
;;     (loop (ret (let (ret (conj bvec gvec val))
;;                  (if has-rest
;;                      (conj ret gseq (list `seq gvec))
;;                      ret))
;;           n 0
;;           bs b
;;           seen-rest? false)
;;           (if (seq bs)
;;               (let (firstb (first bs))
;;                 (cond
;;                   (= firstb '&) (recur (ds-pvec ret (common-lisp:second bs) gseq)
;;                                        n
;;                                        (nnext bs)
;;                                        true)
;;                   (= firstb :as) (ds-pvec ret (common-lisp:second bs) gvec)
;;                   :else (if seen-rest?
;;                             (throw (ex-info "Unsupported binding form, only :as can follow & parameter" nil))
;;                             (recur (ds-pvec (if has-rest
;;                                                 (conj ret
;;                                                       gfirst `(first ~gseq)
;;                                                       gseq `(next ~gseq))
;;                                                 ret)
;;                                             firstb
;;                                             (if has-rest
;;                                                 gfirst
;;                                                 (list `nth gvec n nil)))
;;                                    (inc n)
;;                                    (next bs)
;;                                    seen-rest?))))
;;               ret))))

;; (defn ds-pmap  (bvec b v)
;;   (let (gmap (gensym "map__")
;;     gmapseq (with-meta gmap {:tag 'clojure.lang.ISeq})
;;     defaults (get b :or))
;;     (loop (ret (-> bvec (conj gmap) (conj v)
;;                    (conj gmap) (conj `(if (seq? ~gmap) (clojure.lang.PersistentHashMap/create (seq ~gmapseq)) ~gmap))
;;                    ((fn (ret)
;;                         (if (get b :as)
;;                             (conj ret (get b :as) gmap)
;;                             ret))))
;;           bes (let (transforms
;;                 (reduce1
;;                  (fn (transforms mk)
;;                      (if (keyword? mk)
;;                          (let (mkns (namespace mk)
;;                            mkn (name mk))
;;                            (cond (= mkn "keys") (assoc transforms mk #(keyword (or mkns (namespace %)) (name %)))
;;                                  (= mkn "syms") (assoc transforms mk #(list `quote (symbol (or mkns (namespace %)) (name %))))
;;                                  (= mkn "strs") (assoc transforms mk str)
;;                                  :else transforms))
;;                          transforms))
;;                  {}
;;                  (keys b)))
;;                 (reduce
;;                  (fn (bes entry)
;;                      (reduce  #(assoc %1 %2 ((val entry) %2))
;;                               (dissoc bes (key entry))
;;                               ((key entry) bes)))
;;                  (dissoc b :as :or)
;;                  transforms)))
;;           (if (seq bes)
;;               (let (bb (key (first bes))
;;                 bk (val (first bes))
;;                 local (if (instance? clojure.lang.Named bb) (with-meta (symbol nil (name bb)) (meta bb)) bb)
;;                 bv (if (contains? defaults local)
;;                        (list `get gmap bk (defaults local))
;;                        (list `get gmap bk)))
;;                 (recur (if (ident? bb)
;;                            (-> ret (conj local bv))
;;                            (pb ret bb bv))
;;                        (next bes)))
;;               ret))))

;; "List comprehension. Takes a vector of one or more
;;    binding-form/collection-expr pairs, each followed by zero or more
;;    modifiers, and yields a lazy sequence of evaluations of expr.
;;    Collections are iterated in a nested fashion, rightmost fastest,
;;    and nested coll-exprs can refer to bindings created in prior
;;    binding-forms.  Supported modifiers are: :let [binding-form expr ...],
;;    :while test, :when test.

;;   (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))"

;;might port for over, but we need destructuring.  looking at something
;;like metabang-bind that is seq aware.
;; (for ((x y) '(1 (2 3 4))
;;       (hd & rst) y)
;;      (list x y))

;; "bindings => x xs

;;   Roughly the same as (when (seq xs) (let [x (first xs)] body)) but xs is evaluated only once"
;;  {:added "1.0"}

;;WORK IN PROGRESS.
;;implementing for is a good skill check due to all the destructuring, plus
;;it's probably useful in the implementation side.
;; (fn emit-bind (((bind expr &rest mod-pairs) &rest next-groups))
;;     (let ((_ next-expr)  next-groups
;;           giter (gensym "iter__"))
;;       (let (gxs (gensym "s__")
;;             do-mod (fn do-mod (pair & etc)
;;                        (let ((k v) pair)
;;                          (cond
;;                            (= k :let) `(let ,v ,(do-mod etc))
;;                            (= k :while) `(when ,v ,(do-mod etc))
;;                            (= k :when) `(if ,v
;;                                             ,(do-mod etc)
;;                                             (recur (rest ,gxs)))
;;                            (keyword? k) (err "Invalid 'for' keyword " k)
;;                            next-groups
;;                            (with-gensyms (iterys# fs#)
;;                              `(let (,iterys ,(emit-bind next-groups)
;;                                     ,fs# (seq (,iterys# ,next-expr)))
;;                                 (if ,fs#
;;                                     (concat ,fs# (,giter (rest ,gxs)))
;;                                     (recur (rest ,gxs)))))
;;                            :else `(cons ,body-expr
;;                                         (,giter (rest ,gxs)))))))
;;         (if next-groups
;;             ;;"not the inner-most loop"
;;             `(fn ,giter (,gxs)
;;                  (lazy-seq
;;                   (loop (,gxs ,gxs)
;;                     (when-first (,bind ,gxs)
;;                                ,(do-mod mod-pairs)))))
;;             ;;"inner-most loop"
;;             (let (gi (gensym "i__")
;;                   gb (gensym "b__")
;;                   do-cmod (fn do-cmod (pair & etc)
;;                               (let ((k v) pair)
;;                                 (cond
;;                                   (= k :let) `(let ,v ,(do-cmod etc))
;;                                   (= k :while) `(when ,v ,(do-cmod etc))
;;                                   (= k :when) `(if ,v
;;                                                    ,(do-cmod etc)
;;                                                    (recur
;;                                                     (unchecked-inc ,gi)))
;;                                   (keyword? k)
;;                                   (err "Invalid 'for' keyword " k)
;;                                   :else
;;                                   `(do (chunk-append ,gb ,body-expr)
;;                                        (recur (unchecked-inc ,gi)))))))
;;               `(fn ,giter [,gxs]
;;                    (lazy-seq
;;                     (loop [,gxs ,gxs]
;;                           (when-let [,gxs (seq ,gxs)]
;;                             (if (chunked-seq? ,gxs)
;;                                 (let [c# (chunk-first ,gxs)
;;                                   size# (int (count c#))
;;                                   ,gb (chunk-buffer size#)]
;;                                   (if (loop [,gi (int 0)]
;;                                             (if (< ,gi size#)
;;                                                 (let [,bind (.nth c# ,gi)]
;;                                                   ,(do-cmod mod-pairs))
;;                                                 true))
;;                                       (chunk-cons
;;                                        (chunk ,gb)
;;                                        (,giter (chunk-rest ,gxs)))
;;                                       (chunk-cons (chunk ,gb) nil)))
;;                                 (let [,bind (first ,gxs)]
;;                                   ,(do-mod mod-pairs))))))))))))
;; (defmacro for (seq-exprs body-expr)
;;   ;; (assert-args
;;   ;;  (vector? seq-exprs) "a vector for its binding"
;;   ;;  (even? (count seq-exprs)) "an even number of forms in binding vector")
;;   (let (to-groups (fn (seq-exprs)
;;                       (reduce (fn (groups (k v))
;;                                    (if (keyword? k)
;;                                        (conj (pop groups)
;;                                              (conj (peek groups) (vector  k v)))
;;                                        (conj groups (vector  k v))))
;;                               (vector)  (partition 2 seq-exprs)))
;;         err (fn (& msg) (throw (ex-info (apply str msg) (hashs-map))))
;;         )
;;     emit-bind (fn emit-bind [[[bind expr & mod-pairs] & [[_ next-expr] :as next-groups]]]
;;                   (let [giter (gensym "iter__")
;;                     gxs (gensym "s__")
;;                     do-mod (fn do-mod [[[k v :as pair] & etc]]
;;                                (cond
;;                                  (= k :let) `(let ~v ~(do-mod etc))
;;                                  (= k :while) `(when ~v ~(do-mod etc))
;;                                  (= k :when) `(if ~v
;;                                                   ~(do-mod etc)
;;                                                   (recur (rest ~gxs)))
;;                                  (keyword? k) (err "Invalid 'for' keyword " k)
;;                                  next-groups
;;                                  `(let [iterys# ~(emit-bind next-groups)
;;                                     fs# (seq (iterys# ~next-expr))]
;;                                     (if fs#
;;                                         (concat fs# (~giter (rest ~gxs)))
;;                                         (recur (rest ~gxs))))
;;                                  :else `(cons ~body-expr
;;                                               (~giter (rest ~gxs)))))]
;;                     (if next-groups
;;                         #_"not the inner-most loop"
;;                         `(fn ~giter [~gxs]
;;                              (lazy-seq
;;                               (loop [~gxs ~gxs]
;;                                     (when-first [~bind ~gxs]
;;                                                 ~(do-mod mod-pairs)))))
;;                         #_"inner-most loop"
;;                         (let [gi (gensym "i__")
;;                           gb (gensym "b__")
;;                           do-cmod (fn do-cmod [[[k v :as pair] & etc]]
;;                                       (cond
;;                                         (= k :let) `(let ~v ~(do-cmod etc))
;;                                         (= k :while) `(when ~v ~(do-cmod etc))
;;                                         (= k :when) `(if ~v
;;                                                          ~(do-cmod etc)
;;                                                          (recur
;;                                                           (unchecked-inc ~gi)))
;;                                         (keyword? k)
;;                                         (err "Invalid 'for' keyword " k)
;;                                         :else
;;                                         `(do (chunk-append ~gb ~body-expr)
;;                                              (recur (unchecked-inc ~gi)))))]
;;                           `(fn ~giter [~gxs]
;;                                (lazy-seq
;;                                 (loop [~gxs ~gxs]
;;                                       (when-let [~gxs (seq ~gxs)]
;;                                         (if (chunked-seq? ~gxs)
;;                                             (let [c# (chunk-first ~gxs)
;;                                               size# (int (count c#))
;;                                               ~gb (chunk-buffer size#)]
;;                                               (if (loop [~gi (int 0)]
;;                                                         (if (< ~gi size#)
;;                                                             (let [~bind (.nth c# ~gi)]
;;                                                               ~(do-cmod mod-pairs))
;;                                                             true))
;;                                                   (chunk-cons
;;                                                    (chunk ~gb)
;;                                                    (~giter (chunk-rest ~gxs)))
;;                                                   (chunk-cons (chunk ~gb) nil)))
;;                                             (let [~bind (first ~gxs)]
;;                                               ~(do-mod mod-pairs)))))))))))]
;;     `(let [iter# ~(emit-bind (to-groups seq-exprs))]
;;        (iter# ~(second seq-exprs)))))

;; (comment 
;;  (defn destructure (bindings)
;;    (let (bents (partition 2 bindings)
;;      pb (fn pb (bvec b v)
;;             (let (pvec (fn (bvec b val) (ds-pvec pvec b val))
;;               pmap
;;               )
;;               (cond
;;                 (symbol? b) (-> bvec (conj b) (conj v))
;;                 (vector? b) (pvec bvec b v)
;;                 (map? b) (pmap bvec b v)
;;                 :else (throw (new Exception (str "Unsupported binding form: " b))))))
;;      process-entry (fn (bvec b) (pb bvec (common-lisp:first b) (common-lisp:second b))))
;;      (if (every? symbol? (map common-lisp:first bents))
;;          bindings
;;          (reduce1 process-entry +empty-pvec+ bents))))

;;  )


;;:cljs.tools.reader.impl.inspect
;;brings this in.  We already have a proxy
;;for it as a lazyseq from sequences, and it
;;extends to all known indexed common lisp types.
;;we may bring it in formally though for completeness.
;; (deftype IndexedSeq [arr i meta]
;;   Object
;;   (toString [coll]
;;             (pr-str* coll))
;;   (equiv [this other]
;;          (-equiv this other))
;;   (indexOf [coll x]
;;            (-indexOf coll x 0))
;;   (indexOf [coll x start]
;;            (-indexOf coll x start))
;;   (lastIndexOf [coll x]
;;                (-lastIndexOf coll x (count coll)))
;;   (lastIndexOf [coll x start]
;;                (-lastIndexOf coll x start))

;;   ICloneable
;;   (-clone [_] (IndexedSeq. arr i meta))

;;   ISeqable
;;   (-seq [this]
;;         (when (< i (alength arr))
;;           this))

;;   IMeta
;;   (-meta [coll] meta)
;;   IWithMeta
;;   (-with-meta [coll new-meta]
;;               (if (identical? new-meta meta)
;;                   coll
;;                   (IndexedSeq. arr i new-meta)))

;;   ASeq
;;   ISeq
;;   (-first [_] (aget arr i))
;;   (-rest [_] (if (< (inc i) (alength arr))
;;                  (IndexedSeq. arr (inc i) nil)
;;                  ()))

;;   INext
;;   (-next [_] (if (< (inc i) (alength arr))
;;                  (IndexedSeq. arr (inc i) nil)
;;                  nil))

;;   IDrop
;;   (-drop [coll n]
;;          (if (pos? n)
;;              (if (< (+ i n) (alength arr))
;;                  (IndexedSeq. arr (+ i n) nil)
;;                  nil)
;;              coll))

;;   ICounted
;;   (-count [_]
;;           (max 0 (- (alength arr) i)))

;;   IIndexed
;;   (-nth [coll n]
;;         (let [i (+ n i)]
;;           (if (and (<= 0 i) (< i (alength arr)))
;;               (aget arr i)
;;               (throw (js/Error. "Index out of bounds")))))
;;   (-nth [coll n not-found]
;;         (let [i (+ n i)]
;;           (if (and (<= 0 i) (< i (alength arr)))
;;               (aget arr i)
;;               not-found)))

;;   ISequential
;;   IEquiv
;;   (-equiv [coll other] (equiv-sequential coll other))

;;   IIterable
;;   (-iterator [coll]
;;              (IndexedSeqIterator. arr i))

;;   ICollection
;;   (-conj [coll o] (cons o coll))

;;   IEmptyableCollection
;;   (-empty [coll] (.-EMPTY List))

;;   IReduce
;;   (-reduce [coll f]
;;            (array-reduce arr f (aget arr i) (inc i)))
;;   (-reduce [coll f start]
;;            (array-reduce arr f start i))

;;   IHash
;;   (-hash [coll] (hash-ordered-coll coll))

;;   IReversible
;;   (-rseq [coll]
;;          (let [c (-count coll)]
;;            (if (pos? c)
;;                (RSeq. coll (dec c) nil)))))
