;;This is the basis for bootstrapping clojure onto common lisp.
;;I figure if I can define the primitive forms that clojure requires, 
;;there's already a ton of clojure written in clojure.  The clojurescript
;;runtime actually has a significant portion of clojure defined via protocols,
;;which given limited forms, provides a pretty slick way to bootstrap an 
;;implementation.
;;A couple of big hurdles: 
;;1) Lisp1 vs Lisp2.  I'll hack the evaluator for this.
;;2) Persistent structures.  Already built Pvector and 1/2 done with Pmap.
;;3) Protocols.  Already implemented as generic functions. 
;;4) Multimethods.  Need to find a way to implement multiple dispatch.
;;5) Reader.  CL macros use , and ,@ in place of ~ and ~@ in Clojure.
;;            We'll need to either cook the common lisp reader, or 
;;            build a separate clojure reader that will perform the
;;            appropriate replacements.
;;            @ is a literal for #'deref in clojure
;;            , is whitespace in clojure.
;;            [] denote vectors -> already have a reader macro in pvector.lisp
;;            {} denote maps    -> already have a reader macro in pmap.lisp
;;           #{} denote sets
;;6) Destructuring.  This may be a bit tricky, although there are a limited number of 
;;                   clojure forms.  Since we have reader
;;7)Seq library.  This shouldn't be too hard.  I already have a lazy list lib prototype
;;                as well as generic functions for the basic ops.  I think I'll try to 
;;                use the protocols defined in the clojurescript version as much possible,
;;                rather than baking in a lot of the seq abstraction in the host language
;;                like clojure does.                

(defpackage :clclojure.base
  (:use :common-lisp :common-utils
        :clclojure.keywordfunc :clclojure.lexical
        :clclojure.pvector :clclojure.cowmap :clclojure.protocols :clclojure.eval)
  (:shadow :let :deftype :defmacro :map :reduce :first :rest :second :dotimes :nth :cons :count :do :get :assoc :when-let :vector
   :odd? :even? :zero? :identity :filter :loop :if-let :throw :list* :cond
   :=) 
  (:export :def :defn :fn :meta :with-meta :str :symbol? :first :rest :second :next
   :deftype :defprotocol :reify :extend-type :nil? :identical?
   :extend-protocol :let :into :take :drop :filter :seq :vec :empty :conj :concat :map :reduce :dotimes :nth :cons :count :do :get :assoc :when-let
   :if-let :ns :even? :pos? :zero? :odd? :vector :hash-map :inc :dec :identity :loop  :chunk-first
   :doall  :chunk-buffer :every? :chunk-rest :interleave :ffirst :partition :seq->list :fnext :chunk-cons :nthrest
   :dorun  :chunked-seq? :->iterator :chunk-append :throw :ex-info :ex-cause :ex-message :ex-data :list* :cond :try := :true :false)              ; :defmacro
  )
(in-package clclojure.base)


;;move this later...
(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  ;;temporary hacks...
  (define-symbol-macro true 't)
  (define-symbol-macro false nil)

  ;;convenient placeholder
  (defun ns (name &rest opts)    
    (eval `(progn (defpackage ,name
                    (:use :clclojure.base :common-lisp)
                    (:shadowing-import-from :clclojure.base :let :deftype :defmacro :map :reduce :first :rest :second :dotimes :nth :cons :count :do :get :assoc :when-let :vector))
                  (in-package ,name))))
  
  (common-lisp:defmacro defmacro (name args &rest body)
    ;`(clclojure.eval:defmacro/literal-walker ,name ,args ,@body)
    `(common-lisp:defmacro ,name ,args ,@body)
    )
  
  (defun vector? (x) (typep x 'clclojure.pvector::pvec))

  ;;Let's hack let to allow us to infer vector-binds
  ;;as a clojure let definition...
  (defmacro let (bindings &body body)
    (if   ;(eq (common-lisp:first bindings) 'persistent-vector)
         (vector? bindings)
         `(unified-let* (,@(partition! 2 (vector-to-list  bindings))) ,@body)
         `(cl:let  ,bindings ,@body)))

  (defun macro?    (s) (when (macro-function s) 't))
  (defun function? (s) (fboundp s))
  ;;weak hack around lack of read-time vector creation.
  (defun vector-form? (expr) (or (vector? expr) (eq (common-lisp:first expr) 'persistent-vector))))

(define-condition not-implemented (error) ())
(define-condition uneven-arguments (error) ())

(defgeneric destructure (bindings))

;;a single function definition
(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (defstruct fn-def  name args body)
  ;;a macro definition -- later
  (defstruct macro-def name args body))

;;At compile-time, [x y] -> (persistent-vector x y).
;;This is upsetting us...
(defmacro quoted-vec (v)
  (if (vector? v)
      `(quote ,v);;`(persistent-vector ,@(mapcar #'quote-sym  (vector-to-list v))) 
      `(persistent-vector ,@(mapcar #'quote-sym  (rest v)))))

(defun variadic (v) (member '& (vector-to-list v)))

;;Todo: move this out to CLOS?
;;parse a clojure style function definition.
;; (defmacro read-fn (arg-vec body)
;;   `(make-fn-def :args   (quoted-vec ,arg-vec) 
;; 		:body   (quote ,body)))

(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (defun read-fn (arg-vec body &optional name)
    (make-fn-def :name   name 
                 :args   arg-vec
                 :body   body)))

(defgeneric arity (fd))
(defmethod  arity ((fd clclojure.pvector::pvec))  
  (values (vector-count  fd) (variadic fd)))
(defmethod  arity ((fd fn-def)) (arity (slot-value fd 'args)))

;;since clojure allows multiple bodies, with fixed arity for each body, we 
;;compose multiple function (arg body) pairs into a list of function definitions.
;;We should then be able to dispatch on the count of args, simply invoking 
;;the appropriate function matched to arity.
;; (defmacro fn* (&rest specs)	  
;;   `(list ,@(mapcar (lambda (vb) `(read-fn ,(common-lisp:first vb) ,(second vb))) specs)))

(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  ;; (defun fn* (&rest specs)
  ;;   (let* ((fst (car specs))
  ;;          (named? nil)
  ;;          (name (if (symbolp (common-lisp:first fst))                     
  ;;                    (progn (setf named? t)
  ;;                           (common-lisp:first fst) )
  ;;                    (symb (symbol-name (gensym "fn_")))))
  ;;          (specs (if named? (common-lisp:rest (common-lisp:first  specs)) specs)))
  ;;     (pprint (list fst named? name specs))
  ;;     `(,@(mapcar (lambda (vb)
  ;;                   (pprint vb)
  ;;                   (read-fn (common-lisp:first vb)
  ;;                            (if (cadr vb)                                          
  ;;                                (common-lisp:cons 'progn (common-lisp:rest vb))
  ;;                                (second vb))
  ;;                            name))
  ;;                 specs
  ;;                 ))))

    (defun fn* (name &rest specs)
      `(,@(mapcar (lambda (vb) (read-fn (common-lisp:first vb)
                                        (if (cadr vb)                            
                                            (common-lisp:cons 'progn (common-lisp:rest vb))
                                            (common-lisp:second vb)) name)) specs)))
    

  ;; (defparameter test-fn
  ;;   '(fn* ([x] x)
  ;;         ([x y]        (+ x y))
  ;;         ([x y & xs]   (common-lisp:reduce #'+ xs :initial-value (+ x y)))))

  (defstruct arg-parse lambda-list outer-let)

  (define-condition no-matching-function         (error) ())
  (define-condition multiple-variadic-functions  (error) ())

  ;;this is going to be somewhat tricky, since we'll probably have a little state 
  ;;machine that parses the args, possibly destructuring recursively.  Don't know all 
  ;;the cases yet, but we'll need to be able to destructure vectors and maps into 
  ;;corresponding lambda lists.
  (defun parse-args (args)
    (make-arg-parse :lambda-list (mapcar (lambda (x)
                                           (if (and (symbolp x)
                                                    (string-equal (symbol-name x) "&"))
                                               '&rest
                                               x)) (vector-to-list args)))))
;;Compile a clojure fn special form into a common lisp lambda
(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (defgeneric fndef->sexp (fd))
  ;; (defmethod  fndef->sexp ((fd fn-def))
  ;;   (with-slots (args body) fd
  ;;     (with-slots (lambda-list outer-let) (parse-args args)           
  ;;       (let ((interior (if outer-let `(let* ,outer-let ,body)
  ;;                           body)))
  ;;         `(lambda ,lambda-list ,interior)))))

  (defmethod  fndef->sexp ((fd fn-def))
    (with-slots (args body name) fd
      (with-slots (lambda-list outer-let) (parse-args args)           
        (let ((interior (if outer-let `(let* ,outer-let ,body)
                            body)))
          `(named-fn ,name ,lambda-list ,interior)))))

  ;;parse a list of function definitions into an n-lambda dispatching function.
  ;; (defmethod fndef->sexp ((fd common-lisp:cons))
  ;;   (if (= (length fd) 1)  (fndef->sexp (common-lisp:first fd)) ;simple case
  ;;       ;;case with multiple function definitions.
  ;;       (progn  (pprint fd)
  ;;               `(common-utils:lambda* ,@(mapcar (lambda (body)
  ;;                                                  (common-lisp:rest (fndef->sexp body))) fd)))))

  ;; (defmethod fndef->sexp ((fd common-lisp:cons))
  ;;   (if (= (length fd) 1)  (fndef->sexp (common-lisp:first fd)) ;simple case
  ;;       ;;case with multiple function definitions.
  ;;       (let ((name (fn-def-name (common-lisp:first fd))))
  ;;         `(common-utils:lambda* ,@(mapcar (lambda (body)
  ;;                                            (common-lisp:rest (common-lisp:rest (fndef->sexp body)))) fd)))))

  (defmethod fndef->sexp ((fd common-lisp:cons))
    (if (common-lisp:= (length fd) 1)  (fndef->sexp (common-lisp:first fd)) ;simple case
        ;;case with multiple function definitions.
        (let ((name (fn-def-name (common-lisp:first fd))))
          `(common-utils:named-fn* ,name
             ,@(mapcar (lambda (body)
                         (common-lisp:rest (common-lisp:rest (fndef->sexp body)))) fd)))))


  )
  
;;Clojure's anonymous function special form.
;;Todo: support destructuring in the args.
;; (defmacro fn (&rest specs)
;;   (pprint specs)
;;   (let* ((res 
;;            (cond  ((symbolp (common-lisp:first specs))
;;                    (fndef->sexp (fn*  (cons  (first specs) (list  (rest  specs))))))
;;                   ((vector-form? (common-lisp:first specs))
;;                   (fndef->sexp (fn*  specs)))
                
;;                  ;;TODO get rid of this eval....
;;                  (t
;;                   (fndef->sexp (apply #'fn* specs))))))    
;;     `(,@(clclojure.eval::custom-eval-bindings (sb-cltl2::macroexpand-all res) nil))))

(defmacro fn (&rest specs)
  (let* ((hd    (common-lisp:first specs))
         (name  (if (symbolp hd) hd (symb (symbol-name (gensym "fn_")))))
         (specs (if (symbolp hd) (common-lisp:rest specs) specs))
         (res   (if (vector-form? (common-lisp:first specs)) 
                    (fndef->sexp (fn* name specs))
                    ;;TODO get rid of this eval....
                    (let ((bodies (apply #'fn*  (common-lisp:cons name specs))))
                      (fndef->sexp bodies)))))
    `(,@(clclojure.eval::custom-eval-bindings (sb-cltl2::macroexpand-all res) nil))))

;;def 
;;===

;;Experimental.  Not sure of how to approach this guy.
;;for now, default to everything being public / exported.
;;that should be toggled via metadata in real implementation.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def (var &rest init-form)
    `(progn (defparameter ,var ,@init-form)
            (with-meta (quote ,var) '((SYMBOL .  T) (DOC . "none")))
            (when (functionp (symbol-value (quote  ,var)))
              (setf (symbol-function (quote ,var)) (symbol-value (quote  ,var))))
            (export ',var)
            (quote ,var)
            )))

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
;;[x]                   -> (pvector x)    ;;more or less implemented
;;{x y}                 -> (hash-map x y) 
;;(Blah. x)             -> (make-Blah x) ;;CLOS constructor
;;(. obj method args)   -> ((slot-value obj method) args)
;;(. obj (method args)) -> ((slot-value obj method) args)
;;(. obj method)        -> ((slot-value obj method))
;;(.method obj args)    -> (slot-value obj method) ;;CLOS accessor
;;(def x val)           -> (defparameter x val)
;;(let [x y] expr)      -> (let* ((x y)) expr)
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
  (let [classname (gentemp "REIFY")
    ctor (gensym "CONSTRUCTOR")]
    `(let ((,ctor (clojure-deftype ,classname ,'[] ,@implementations)))
       (funcall ,ctor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; core protocols ;;;;;;;;;;;;;

;;Need to get back to this guy...multiple arity is not yet implemented...
(eval-when (:compile-toplevel :load-toplevel :execute) 

  (defprotocol IFn
      (-invoke
       [this]
       [this a]
       [this a b]
       [this a b c]
       [this a b c d]
       [this a b c d e]
       [this a b c d e f]
       [this a b c d e f g]
       [this a b c d e f g h]
       [this a b c d e f g h i]
       [this a b c d e f g h i j]
       [this a b c d e f g h i j k]
       [this a b c d e f g h i j k l]
       [this a b c d e f g h i j k l m]
       [this a b c d e f g h i j k l m n]
       [this a b c d e f g h i j k l m n o]
       [this a b c d e f g h i j k l m n o p]
       [this a b c d e f g h i j k l m n o p q]
       [this a b c d e f g h i j k l m n o p q s]
       [this a b c d e f g h i j k l m n o p q s t]
       [this a b c d e f g h i j k l m n o p q s t rest]))


  ;;These work
  (defprotocol ICounted
      (-count [coll] "constant time count"))

  (defprotocol IEmptyableCollection
      (-empty [coll]))

  (defprotocol ICollection
      (-conj [coll o]))

  (defprotocol IOrdinal
      (-index [coll]))

  ;;this will break.  current implementation of defprotocol doesn't allow for 
  ;;multiple arity functions like this.  Need to handle variadic functions...
  (defprotocol IIndexed
      (-nth [coll n] [coll n not-found]))

  (defprotocol ASeq)

  (defprotocol ISeq
      (-first [coll])
    (-rest [coll]))

  (defprotocol INext
      (-next [coll]))

  (defprotocol ILookup
      (-lookup [o k] [o k not-found]))

  (defprotocol IAssociative
      (-contains-key? [coll k])
    (-entry-at [coll k])
    (-assoc [coll k v]))

  (defprotocol IMap
      (-assoc-ex [coll k v])
    (-dissoc [coll k]))

  (defprotocol IMapEntry
      (-key [coll])
    (-val [coll]))

  (defprotocol ISet
      (-disjoin [coll v]))

  (defprotocol IStack
      (-peek [coll])
    (-pop [coll]))

  (defprotocol IVector
      (-assoc-n [coll n val]))

  (defprotocol IDeref
      (-deref [o]))

  (defprotocol IDerefWithTimeout
      (-deref-with-timeout [o msec timeout-val]))

  (defprotocol IMeta
      (-meta [o]))

  (defprotocol IWithMeta
      (-with-meta [o meta]))

  (defprotocol IReduce
      (-reduce [coll f]
               [coll f start]))

  (defprotocol IKVReduce
      (-kv-reduce [coll f init]))

  (defprotocol IEquiv
      (-equiv [o other]))

  (defprotocol IHash
      (-hash [o]))
  (defprotocol ISeqable
      (-seq [o]))
  


  (defprotocol ISequential
    "Marker interface indicating a persistent collection of sequential items")

  (defprotocol IList
    "Marker interface indicating a persistent list")

  (defprotocol IRecord
    "Marker interface indicating a record object")

  (defprotocol IReversible
      (-rseq [coll]))

  (defprotocol ISorted
      (-sorted-seq [coll ascending?])
    (-sorted-seq-from [coll k ascending?])
    (-entry-key [coll entry])
    (-comparator [coll]))

  ;; (defprotocol ^:deprecated IPrintable
  ;;   "Do not use this.  It is kept for backwards compatibility with existing
  ;;    user code that depends on it, but it has been superceded by IPrintWithWriter
  ;;    User code that depends on this should be changed to use -pr-writer instead."
  ;;   (-pr-seq [o opts]))

  (defprotocol IWriter
      (-write [writer s])
    (-flush [writer]))

  (defprotocol IPrintWithWriter
    "The old IPrintable protocol's implementation consisted of building a giant
   list of strings to concatenate.  This involved lots of concat calls,
   intermediate vectors, and lazy-seqs, and was very slow in some older JS
   engines.  IPrintWithWriter implements printing via the IWriter protocol, so it
   be implemented efficiently in terms of e.g. a StringBuffer append."
    (-pr-writer [o writer opts]))

  (defprotocol IPending
      (-realized? [d]))

  (defprotocol IWatchable
      (-notify-watches [this oldval newval])
    (-add-watch [this key f])
    (-remove-watch [this key]))

  (defprotocol IEditableCollection
      (-as-transient [coll]))


  (defprotocol ITransientCollection
      (-conj! [tcoll val])
    (-persistent! [tcoll]))

  (defprotocol ITransientAssociative
      (-assoc! [tcoll key val]))

  (defprotocol ITransientMap
      (-dissoc! [tcoll key]))

  (defprotocol ITransientVector
      (-assoc-n! [tcoll n val])
    (-pop! [tcoll]))

  (defprotocol ITransientSet
      (-disjoin! [tcoll v]))

  (defprotocol IComparable
      (-compare [x y]))

  (defprotocol IChunk
      (-drop-first [coll]))

  (defprotocol IChunkedSeq
      (-chunked-first [coll])
    (-chunked-rest [coll]))

  (defprotocol IChunkedNext
      (-chunked-next [coll]))

  (defprotocol INamed
      (-name [thing]))

  )
   


;;Extending types to native structures and clojure literals:
;;==========================================================
(eval-when (:compile-toplevel :load-toplevel :execute)
  (extend-type
   null
   ICounted
   (-count [c] 0)
   IEmptyableCollection
   (-empty [c] nil)
   ICollection
   (-conj [coll itm] (common-lisp:cons itm nil))
   IStack
   (-peek [coll] nil)
   (-pop  [coll] nil)
   ISeqable
   (-seq [coll] nil)
   IHash
   (-hash [o] (sxhash nil))
   IEquiv
   (-equiv [o other] (error 'not-implemented))
   ISeq
   (-first [o] nil)
   (-rest  [o] nil)
   IReversible
   (-rseq [coll] nil))

  ;;We got a ton of goodies from
  ;;sb-sequences namespace to leverage here.
  ;;good opportunity for iterator-seq...
  (extend-type
   sequence
   ICounted
   (-count [c] (common-lisp:length c))
   
   ;; IEmptyableCollection
   (-empty [c] (sb-sequence:make-sequence-like c 0))
   ;; ICollection
   ;; (-conj [coll itm] (cons itm nil))
   IStack
   (-peek [coll] (elt coll 0))
   (-pop  [coll] (error 'not-implemented))
   ISeqable
   (-seq [coll] (error 'not-implemented))
   IHash
   (-hash [o] (sxhash o))
   
   ;; IEquiv
   ;; (-equiv [o other] (error 'not-implemented))

   ISeq
   (-first [o] (elt o 0))
   ;;TODO pull this over...
   ;;Probably identical to array-seqs

   (-rest  [o] (error 'not-implemented))
   IReversible
   (-rseq [coll] (reverse coll)))

  (extend-type
   clclojure.pvector::pvec
   
   ICounted
   (-count [c] (vector-count c))
   IIndexed
   (-nth  [coll n] (nth-vec coll n))
   (-nth  [coll n not-found] (nth-vec coll n))
   
   IEmptyableCollection
   (-empty [c] [])
   ICollection
   (-conj [coll itm] (vector-conj coll itm))
   IVector
   (-assoc-n [coll n val] (vector-assoc coll n val))
   IStack
   (-peek [coll]
          (when (not (zerop (-count coll) )) (nth-vec coll 0)))
   (-pop  [coll]  (subvec coll 1))
   ISeqable
   (-seq [coll] (vector-to-list coll ))
   IHash
   (-hash [o]   (error 'not-implemented))
   IMapEntry
   (-key [coll] (-nth coll 0))
   (-val [coll] (-nth coll 1))
   IEquiv
   (-equiv [o other] (error 'not-implemented))

   IKVReduce
   (-kv-reduce [coll f init] (error 'not-implemented))
   
   IReversible
   (-rseq [coll] (error 'not-implemented))
   IChunk
   (-drop-first [coll] (error 'not-implemented))
   IChunkedSeq
   (-chunked-first [coll] (error 'not-implemented))
   (-chunked-rest [coll] (error 'not-implemented))
   IChunkedNext
   (-chunked-next [coll] (error 'not-implemented))))
 


(eval-when (:compile-toplevel :load-toplevel :execute)
  (extend-type  symbol 
                IMeta
                (-meta [obj] (symbol-meta obj))
                IWithMeta
                (-with-meta [obj m] (with-symbol-meta obj m) obj)
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
                (-equiv [l r]
                        (or (eq l r)
                            (when  (not (or (keywordp l) (keywordp r)))
                              (common-lisp:= (sxhash l) (sxhash r)))
                            ))
                INamed
                (-name [this] (symbol-name this))
                )

  ;;not applicable.
  ;; (extend-type keyword
  ;;              IEquiv
  ;;              ;;dirty implementation....
  ;;              ;;We need to unify qualified and unqualified symbols..
  ;;              ;;in clojure, symbol equality is a bit more complex
  ;;              ;;since they're equiv iff unqualified.
  ;;              ;;unless we hack the reader to reader qualified
  ;;              ;;symbols as unqual, the preponderance of clojure
  ;;              ;;symbol comparisons will not be strict, so
  ;;              ;;we end up with a lot of unqualified symbols.
  ;;              ;;This is just to paper over the bootstrapping
  ;;              ;;process....              
  ;;              (-equiv [l r]
  ;;                      (or (eq l r)
  ;;                          ;;I don't even know if this is possible...
  ;;                          ;;I think keywords are always interned
  ;;                          ;;in the keyword package.
  ;;                          ;; (when (keywordp r)
  ;;                          ;;   (common-lisp:= (sxhash l) (sxhash r)))
  ;;                          ))
  ;;              IHash
  ;;              (-hash [k] (hash-code k))
  ;;              )

  ;;subvector impls...
  (extend-type
   clclojure.pvector::subvector
   
   ICounted
   (-count [c] (vector-count c))

   IEmptyableCollection
   (-empty [c] [])
   ICollection
   (-conj [coll itm] (vector-conj coll itm))
   IVector
   (-assoc-n [coll n val] (vector-assoc coll n val))
   IStack
   (-peek [coll]
          (when (not (zerop (-count coll) )) (nth-vec coll 0)))
   (-pop  [coll]  (subvec coll 1))
   ISeqable
   (-seq [coll] (vector-to-list coll)) ;poorly implemented.  should be arrayseq
   IHash
   (-hash [o]   (error 'not-implemented))
   IMapEntry
   (-key [coll] (-nth coll 0))
   (-val [coll] (-nth coll 1))
   IEquiv
   (-equiv [o other] (error 'not-implemented))
   IKVReduce
   (-kv-reduce [coll f init] (error 'not-implemented))

   IReversible
   (-rseq [coll] (error 'not-implemented))
   IChunk
   (-drop-first [coll] (error 'not-implemented))
   IChunkedSeq
   (-chunked-first [coll] (error 'not-implemented))
   (-chunked-rest [coll] (error 'not-implemented))
   IChunkedNext
   (-chunked-next [coll] (error 'not-implemented))
   ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;list operations.
  (extend-type
   common-lisp:cons
   ICounted
   (-count [c] (length c))

   IEmptyableCollection
   (-empty [c] '())
   ICollection
   (-conj [coll itm] (common-lisp:cons itm coll))
   IStack
   (-peek [coll]  (common-lisp:first coll))
   (-pop  [coll]  (common-lisp:rest coll))
   ISeqable
   (-seq [coll] coll)
   IHash
   (-hash [o]   (sxhash o))
   IEquiv
   (-equiv [o other] (error 'not-implemented))
   IMapEntry
   (-key [coll] (common-lisp:first coll))
   (-val [coll] (common-lisp:second coll))
   ISeq
   (-first [coll]  (common-lisp:first coll))
   (-rest  [coll]  (cdr coll))
   )

  (extend-type
   sequences::lazyseq
   ICounted
   (-count [c] (sequences:seq-count c))

   IEmptyableCollection
   (-empty [c] '())
   ICollection
   (-conj [coll itm] (sequences::cons itm coll))
   IStack
   (-peek [coll]  (sequences::first coll))
   (-pop  [coll]  (sequences::rest coll))
   ISeqable
   (-seq [coll] (sequences::seq coll))
   IHash
   (-hash [o]   (sxhash o)) ;;poorly implemented...
   IEquiv
   (-equiv [o other] (error 'not-implemented))
   IMapEntry
   (-key [coll] (sequences::first coll))
   (-val [coll] (sequences::rest coll))
   ISeq
   (-first [coll]  (sequences::first coll))
   (-rest  [coll]  (sequences::rest coll))
   )

  (extend-type
   sequences::funcseq
   ICounted
   (-count [c] (sequences:seq-count c))

   IEmptyableCollection
   (-empty [c] '())
   ICollection
   (-conj [coll itm] (sequences::cons itm coll))
   IStack
   (-peek [coll]  (sequences::first coll))
   (-pop  [coll]  (sequences::rest coll))
   ISeqable
   (-seq [coll] (sequences::seq coll))
   IHash
   (-hash [o]   (sxhash o)) ;;poorly implemented...
   IEquiv
   (-equiv [o other] (error 'not-implemented))
   IMapEntry
   (-key [coll] (sequences::first coll))
   (-val [coll] (sequences::rest coll))
   ISeq
   (-first [coll]  (sequences::first coll))
   (-rest  [coll]  (sequences::rest coll))
   )

  (extend-type
   clclojure.cowmap::cowmap

   ICounted
   (-count [c] (map-count c))

   IEmptyableCollection
   (-empty [c] {})

   ICollection
   (-conj [coll itm] (map-assoc coll (common-lisp:first itm) (second itm)))

   ISeqable
   (-seq [coll] (map-seq coll))
   
   ILookup
   (-lookup [o k] (map-get o k))
   (-lookup [o k not-found]
            (or (map-get o k) not-found))  

   IAssociative
   (-contains-key? [coll k] (map-contains? coll k))
   (-entry-at [coll k]      (map-entry-at coll k))
   (-assoc [coll k v]       (map-assoc coll k v))

   IMap
   (-assoc-ex [coll k v]  (error 'not-implemented)) ;;apparently vestigial
   (-dissoc   [coll k]    (map-dissoc coll k))
   
   IHash
   (-hash [o]   (error 'not-implemented))
   IEquiv
   (-equiv [o other] (error 'not-implemented))
   IKVReduce
   (-kv-reduce [coll f init] (error 'not-implemented)))

  (extend-type  number
   IEquiv
   (-equiv [l r]  (when (numberp r) (common-lisp:= l r)))
   IHash
   (-hash [n] (hash-code n)))

  (extend-type string
    INamed (-name [x] x))
  )
 ;; IChunk
 ;; (-drop-first [coll] (error 'not-implemented))
 ;; IChunkedSeq
 ;; (-chunked-first [coll] (error 'not-implemented))
 ;; (-chunked-rest [coll] (error 'not-implemented))
 ;; IChunkedNext
 ;; (-chunked-next [coll] (error 'not-implemented))
 
;;friendly map printing
(defmethod print-object ((obj hash-table) stream)
  (common-utils::print-map  obj stream))

;;map printing compatibility
(defmethod print-object ((obj clclojure.cowmap::cowmap) stream)
  (common-utils::print-map (cowmap-table obj) stream))

;;Core Lib
;;========


(eval-when (:compile-toplevel :load-toplevel :execute)  
  (defn seq [coll] (-seq coll))
  (defn vec [coll]
    (if (vector? coll) coll
        (sequences:apply #'persistent-vector (seq coll))))
  
  (defn vector [& xs]
    (sequences:apply #'persistent-vector (seq xs)))
  
  (defn hash-map [& xs]
    (sequences:apply #'persistent-map (seq xs))
    )
  (defn identical? [l r]
    (common-lisp:eq l r))

  (defn nil? [x]
    (identical? x nil))
  ;;need to implement arrayseq...
  ;;These are lame but easy, we really want to
  ;;get chunked-first and friends up and running.
  ;;Also want to bake in typecases for quick
  ;;dispatch...

  (defn first  [coll]  (-first (seq coll)))
  (defn rest   [coll]  (-rest  (seq coll)))
  ;;TBD fix this for an actual -next implementation.
  ;; "Returns a seq of the items after the first. Calls seq on its
  ;; argument.  If there are no more items, returns nil"

  ;;this isn't great....
  (defn implements? [p obj]
    (satisfies? p obj))

  ;;tbd : get metadata reader working...
  ;^seq
  (defn next
    [coll]
    (when-not (nil? coll)
              (if (implements? INext coll)
                  (-next coll)
                  (seq (rest coll)))))
  (defn nnext
    [coll]
    (next (next coll)))
  
  (defn second [coll]  (first (rest coll)))
  (defn ffirst [coll]  (first (first coll)))
  ;;Inaccurate...

  (defn fnext  [coll] (first (rest coll)))

  (defn get
      ([m k] (-lookup m k))
    ([m k not-found] (-lookup m k not-found)))

  (def odd? #'common-utils:odd?)
  (def even? #'common-utils:even?)
  (def zero? #'common-utils:zero?)
  (defn inc [x] (1+ x))
  (defn dec [x] (1- x))
  (defn =
    ([x]   true)
    ([x y]
        (if (and (numberp x) (numberp y))
            (common-lisp:= x y)
            (-equiv x y)))
      ([x y & more]
          (if (-equiv x y)
              (if (next more)
                  (recur y (first more) (next more))
                  (- y (first more)))
              nil)))

  (defn key [e] (-key e))
  (defn val [e] (-val e))
  (defn name [x] (-name x))
  )


(defmacro when-let (binding &rest body)
  (let [binding (seq binding)        
    arg     (-first binding)
    expr    (-first (-rest  binding))
    tst     (gensym "tst")] 
    (clclojure.eval::recover-literals
     `(let ,(vector tst  (second binding))
        (when ,tst
          (let ,(vector arg tst)
            ,@body))))))

(defmacro if-let (binding body &rest false-body)
  (let [binding (seq binding)        
    arg     (-first binding)
    expr    (-first (-rest  binding))
    tst     (gensym "tst")] 
    (clclojure.eval::recover-literals
     `(let ,(vector tst  (second binding))
        (if ,tst
          (let ,(vector arg  tst)
            ,body)
          ,@false-body)))))

(defn throw [e]
  (error e))

;;try-catch-finally...

(defn ex-info
    ([msg map]
          (make-instance 'exception-info :data map :cause msg  :message msg))
  ([msg map cause]
        (make-instance 'exception-info  :data map :cause cause  :message msg)))

(defn ex-data [e]
  (common-utils::exception-info-data e))
(defn ex-cause [e]
  (common-utils::exception-info-cause e))
(defn ex-message [e]
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

(defn assoc
    ([m k v]       (-assoc m k v))
  ([m k v & kvs]
      (reduce (fn [acc kv]
                  (-assoc acc (first kv) (second kv)))
              (-assoc m k v) kvs)))

(defn dissoc
    ([m k]       (-dissoc m k))
    ([m k & ks]
        (reduce (fn [acc k]
                    (-dissoc acc k))
                (-dissoc m k) ks)))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defn count [coll]
    (-count coll))) 

(defn nth
    ([coll index]
           (-nth coll index))
  ([coll index not-found]
         (-nth coll index not-found)))

(defn take [n coll]
  (sequences:take n (seq  coll)))

(defn drop [n coll]
  (sequences:drop n (seq coll)))

(defn conj
  ([] [])
  ([coll] coll)
  ([coll x] (-conj coll x))
  ([coll x & xs]
         (if (seq xs)
             (recur  (-conj  coll x) (first xs) (rest xs))
             (conj coll x))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defn chunked-seq? [x] nil)
  (defn chunk-first [coll] (-chunked-first coll))
  (defn chunk-rest [coll]  (-chunked-rest coll))
  (defn chunk-buffer [coll])
  (defn seq->list [xs] (sequences::seq->list (seq xs)))

  (defn cons [x coll]
    (-conj coll x))

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
                (second clauses)
                (throw (ex-info "cond requires an even number of forms" {})))
            (cons 'clclojure.base:cond (next (next clauses)))))))

(defn chunk-cons [chunk rest]
  (error 'not-implemented))

(defn chunk-append [b x]
  (error 'not-implemented))

(defn empty [coll] (-empty coll))

;; {:private true
;; :static true}
(defn spread
  [arglist]
  (cond
    (nil? arglist) nil
    (nil? (next arglist)) (seq (first arglist))
    :else (cons (first arglist) (spread (next arglist)))))

;; "Creates a new seq containing the items prepended to the rest, the
;;   last of which will be treated as a sequence."
;; {:added "1.0"
;; :static true}
(defn list*
  ([args] (seq args))
  ([a args] (cons a args))
  ([a b args] (cons a (cons b args)))
  ([a b c args] (cons a (cons b (cons c args))))
  ([a b c d & more]
      (cons a (cons b (cons c (cons d (spread more)))))))

(defmacro loop* (bindings &rest body)
  (assert (vector? bindings))
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
;;   {:added "1.0", :special-form true, :forms '[(loop [bindings*] exprs*)]}
;;   [bindings & body]
;;   (assert-args
;;    (vector? bindings) "a vector for its binding"
;;    (even? (count bindings)) "an even number of forms in binding vector")
;;   (let [db (destructure bindings)]
;;     (if (= db bindings)
;;         `(loop* ~bindings ~@body)
;;         (let [vs (take-nth 2 (drop 1 bindings))
;;           bs (take-nth 2 bindings)
;;           gs (map (fn [b] (if (symbol? b) b (gensym))) bs)
;;           bfs (reduce1 (fn [ret [b v g]]
;;                            (if (symbol? b)
;;                                (conj ret g v)
;;                                (conj ret g v b g)))
;;                        [] (map vector bs vs gs))]
;;           `(let ~bfs
;;              (loop* ~(vec (interleave gs gs))
;;                     (let ~(vec (interleave bs gs))
;;                       ~@body)))))))

(defmacro loop (bindings &rest body)
  `(loop* ,bindings ,@body))


;; "When lazy sequences are produced via functions that have side
;;   effects, any effects other than those needed to produce the first
;;   element in the seq do not occur until the seq is consumed. dorun can
;;   be used to force any effects. Walks through the successive nexts of
;;   the seq, does not retain the head and returns nil."
;; {:added "1.0"
;; :static true}

;;TCO version is tripping here...
;; (defn dorun
;;     ([coll]
;;      (when-let [s (seq coll)]
;;        (recur (next s))))
;;   ([n coll]
;;       (when (and (seq coll) (pos? n))
;;         (recur (dec n) (next coll)))))

;;temporary work around while I patch tail cail detection
;;so we stop getting false positives.
(defn dorun
    ([coll]
     (let [s (seq coll)]
       (when s
         (recur (next s)))))
  ([n coll]
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

(defn doall
    ([coll]
     (dorun coll)
     coll)
  ([n coll]
      (dorun n coll)
      coll))

;; "Returns the nth rest of coll, coll when n is 0."
;; {:added "1.3"
;; :static true}
(defn nthrest
  [coll n]
  (loop [n n
         xs coll]
        (if-let [xs (and (pos? n) (seq xs))]
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
  ([n coll]
      (partition n n coll))
  ([n step coll]
      (lazy-seq
       (when-let [s (seq coll)]
         (let [p (doall (take n s))]
           (when (= n (count p))
             (cons p (partition n step (nthrest s step))))))))
  ([n step pad coll]
      (lazy-seq
       (when-let [s (seq coll)]
         (let [p (doall (take n s))]
           (if (= n (count p))
               (cons p (partition n step pad (nthrest s step)))
               (list (take n (concat p pad)))))))))

  ;; "Returns a lazy sequence of lists like partition, but may include
  ;;   partitions with fewer than n items at the end.  Returns a stateful
  ;;   transducer when no collection is provided."
  ;; {:added "1.2"
  ;; :static true}
  ;; ([^long n]
  ;;         (fn [rf]
  ;;             (let [a (java.util.ArrayList. n)]
  ;;               (fn
  ;;                ([] (rf))
  ;;                ([result]
  ;;                 (let [result (if (.isEmpty a)
  ;;                                  result
  ;;                                  (let [v (vec (.toArray a))]
  ;;                                    ;;clear first!
  ;;                                    (.clear a)
  ;;                                    (unreduced (rf result v))))]
  ;;                   (rf result)))
  ;;                ([result input]
  ;;                         (.add a input)
  ;;                         (if (= n (.size a))
  ;;                             (let [v (vec (.toArray a))]
  ;;                               (.clear a)
  ;;                               (rf result v))
  ;;                             result))))))



;;We need to inline or otherwise capture the
;;partition-all function symbol.  Current named-fn*
;;isn't  compatible with lazy seq
  ;; (defn partition-all
  ;;   ([n coll]
  ;;       (partition-all n n coll))
  ;;   ([n step coll]
  ;;       (lazy-seq
  ;;        (when-let [s (seq coll)]
  ;;          (let [seg (doall (take n s))]
  ;;            (cons seg (partition-all n step (nthrest s step))))))))


;; An iteration state value.

;; A value describing the limit of iteration, if any.

;; The from-end value.

;; A step function of three arguments: the sequence, the state value, the from-end value. The function should return the new state value.

;; An end predicate of four arguments: the sequence, the state value, the limit value, the from-end value. The function should return a generalised boolean describing whether the iteration has reached the end of the sequence.

;; An element read function of two arguments: the sequence, the state value.

;; An element write function of three arguments: the new value to store, the sequence, the state value.

;; An index function of two arguments: the sequence, the state value. The function should return the current iteration index, starting from zero.

;; An iterator copy function of two arguments: the sequence, the state value. The function should return a "fresh" iteration value.

(defn ->iterator [s]
  (multiple-value-bind
        (state from-end step end? read-elt write-elt index copy)
      (sb-sequence:make-sequence-iterator s)
    {:state state
     :from-end from-end
     :step step
     :end? end?
     :read-elt  read-elt
     :write-elt write-elt
     :index     index
     :copy      copy}))

;;We're doing a lot of runtime checks that
;;may be suboptimal time-wise.  Could cache
;;the implements? function, or move to protocol...
(defn reduce
    ([f coll]
        (if (sequences:internal-reduce? coll)
            (sequences:reduce f coll)
            (sequences:reduce f (seq coll))))
  ([f init coll]
      (if (sequences:init-reduce? coll)
          (sequences:reduce f init coll)
          (sequences:reduce f (seq coll)))))

;; "Returns a new coll consisting of to-coll with all of the items of
;;   from-coll conjoined. A transducer may be supplied."
;; {:added "1.0"
;; :static true}
(defn into
  ([] [])
  ([to] to)
  ([to from]
       (if nil ;(instance? clojure.lang.IEditableCollection to)
           (with-meta (persistent! (reduce conj! (transient to) from)) (meta to))
           (reduce conj to from)))
  ;; ([to xform from]
  ;;      (if nil ;(instance? clojure.lang.IEditableCollection to)
  ;;          (with-meta (persistent! (transduce xform conj! (transient to) from)) (meta to))
  ;;          (transduce xform conj to from)))
  )
;; "Returns a lazy sequence consisting of the result of applying f to
;;   the set of first items of each coll, followed by applying f to the
;;   set of second items in each coll, until any one of the colls is
;;   exhausted.  Any remaining items in other colls are ignored. Function
;;   f should accept number-of-colls arguments. Returns a transducer when
;;   no collection is provided."
;; {:added "1.0"
;; :static true}

(defmacro lazy-seq (&rest body)
  `(sequences::lazy-seq ,@body))

(defn map
  ;;temporarily on hold while we fix tail recur detection.
  ;; ([f]
  ;;  (fn [rf]
  ;;      (fn
  ;;       ([] (rf))
  ;;       ([result] (rf result))
  ;;       ([result input]
  ;;                (rf result (f input)))
  ;;       ([result input & inputs]
  ;;                (rf result (apply f input inputs))))))
  ([f coll]
      (sequences:map f (seq coll)))
  ([f c1 c2]
      (sequences:map f (seq c1) (seq c2)))
  ([f c1 c2 c3]
      (sequences:map f (seq c1) (seq c2) (seq c3)))
  ([f c1 c2 c3 & colls]
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
  ;; ([pred]
  ;;  (fn [rf]
  ;;      (fn
  ;;       ([] (rf))
  ;;       ([result] (rf result))
  ;;       ([result input]
  ;;                (if (pred input)
  ;;                    (rf result input)
  ;;                    result)))))
  ;; [pred coll]
  ;; (lazy-seq
  ;;  (when-let [s (seq coll)]
  ;;    (if (chunked-seq? s)
  ;;        (let [c (chunk-first s)
  ;;          size (count c)
  ;;          b (chunk-buffer size)]
  ;;          (dotimes [i size]
  ;;            (let [v (nth c i)]
  ;;              (when (pred v)
  ;;                (chunk-append b v))))
  ;;          (chunk-cons (chunk b) (filter pred (chunk-rest s))))
  ;;        (let [f (first s)
  ;;              r (rest s)]
  ;;          (if (pred f)
  ;;              (cons f (filter pred r))
  ;;              (filter pred r)))))))

    ;;lame filter for now.

(defn filter [predicate coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [f  (first s)
            r (rest s)]
       (if (funcall predicate f)
           (cons f (filter predicate r))
           (filter predicate r))))))

(defn concat
    ([] nil)
    ([x] x)
    ([x y] (sequences:concat (seq x) (seq y)))
  ([x y & zs]
      (sequences:apply #'sequences:concat
        (sequences:map #'seq
            (common-lisp:list* x y zs)))))

(defn every? [pred xs]
  (sequences::every? pred xs))


(def identity #'common-lisp:identity)

;; "Returns a lazy seq of the first item in each coll, then the second etc."
;; {:added "1.0"
;; :static true}
(defn interleave
  ([] ())
  ([c1] (lazy-seq (seq  c1)))
  ([c1 c2]
       (lazy-seq
        (let [s1 (seq c1) s2 (seq c2)]
          (when (and s1 s2)
            (cons (first s1) (cons (first s2) 
                                   (interleave (rest s1) (rest s2))))))))
  ([c1 c2 & colls]        
        (let [ss (map seq (conj colls c2 c1))]
          (when (every? identity ss)
            (concat (map first ss) (lazy-seq  (apply interleave (map rest ss))))))))

;;need destructure...

(def symbol? #'symbolp)
(def keyword? #'keywordp)

;;for now, we don't have qualified keywords...
;;we "could" encode that information in the
;;keyword name somewhere (like a central db),
;;but it seems better to implement the actual
;;clojure qualified keywords...
;;wonder if we can inherit from symbol..(nope!).
(defn namespace [s]
  (when (symbol? s)
    (when-let [p (symbol-package s)]
      (package-name p))
    ))


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
;;           psig (fn* [sig]
;;                  ;; Ensure correct type before destructuring sig
;;                  (when (not (seq? sig))
;;                    (throw (IllegalArgumentException.
;;                             (str "Invalid signature " sig
;;                                  " should be a list"))))
;;                  (let [[params & body] sig
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
;;                               `((let [~'% ~(if (< 1 (count body)) 
;;                                             `(do ~@body) 
;;                                             (first body))]
;;                                  ~@(map (fn* [c] `(assert ~c)) post)
;;                                  ~'%))
;;                               body)
;;                        body (if pre
;;                               (concat (map (fn* [c] `(assert ~c)) pre) 
;;                                       body)
;;                               body)]
;;                    (maybe-destructured params body)))
;;           new-sigs (map psig sigs)]
;;       (with-meta
;;         (if name
;;           (list* 'fn* name new-sigs)
;;           (cons 'fn* new-sigs))
;;         (meta &form))))
;; )

;;conflicts with :common-lisp

(defn rest-arg? [x]
  (seql x '&))

;;need to implement "new" eventually
;;it's a low-level interop construct.

(defmacro new (klass &rest args)
  `(make-instance (quote ~klass) ~@args))


(defn ds-pvec [bvec b val]
  (let [gvec (gensym "vec__")
    gseq (gensym "seq__")
    gfirst (gensym "first__")
    has-rest (some rest-arg? b)]
    (loop [ret (let [ret (conj bvec gvec val)]
                 (if has-rest
                     (conj ret gseq (list `seq gvec))
                     ret))
          n 0
          bs b
          seen-rest? false]
          (if (seq bs)
              (let [firstb (first bs)]
                (cond
                  (= firstb '&) (recur (ds-pvec ret (second bs) gseq)
                                       n
                                       (nnext bs)
                                       true)
                  (= firstb :as) (ds-pvec ret (second bs) gvec)
                  :else (if seen-rest?
                            (throw (ex-info "Unsupported binding form, only :as can follow & parameter" nil))
                            (recur (ds-pvec (if has-rest
                                                (conj ret
                                                 gfirst `(first ~gseq)
                                                 gseq `(next ~gseq))
                                                ret)
                                            firstb
                                            (if has-rest
                                                gfirst
                                                (list `nth gvec n nil)))
                                   (inc n)
                                   (next bs)
                                   seen-rest?))))
              ret))))

(defn ds-pmap  [bvec b v]
  (let [gmap (gensym "map__")
        gmapseq (with-meta gmap {:tag 'clojure.lang.ISeq})
        defaults (get b :or)]
    (loop [ret (-> bvec (conj gmap) (conj v)
                   (conj gmap) (conj `(if (seq? ~gmap) (clojure.lang.PersistentHashMap/create (seq ~gmapseq)) ~gmap))
                   ((fn [ret]
                        (if (get b :as)
                            (conj ret (get b :as) gmap)
                            ret))))
          bes (let [transforms
                (reduce1
                 (fn [transforms mk]
                     (if (keyword? mk)
                         (let [mkns (namespace mk)
                           mkn (name mk)]
                           (cond (= mkn "keys") (assoc transforms mk #(keyword (or mkns (namespace %)) (name %)))
                                 (= mkn "syms") (assoc transforms mk #(list `quote (symbol (or mkns (namespace %)) (name %))))
                                 (= mkn "strs") (assoc transforms mk str)
                                 :else transforms))
                         transforms))
                 {}
                 (keys b))]
                (reduce
                 (fn [bes entry]
                     (reduce  #(assoc %1 %2 ((val entry) %2))
                              (dissoc bes (key entry))
                              ((key entry) bes)))
                 (dissoc b :as :or)
                 transforms))]
          (if (seq bes)
              (let [bb (key (first bes))
                bk (val (first bes))
                local (if (instance? clojure.lang.Named bb) (with-meta (symbol nil (name bb)) (meta bb)) bb)
                bv (if (contains? defaults local)
                       (list `get gmap bk (defaults local))
                       (list `get gmap bk))]
                (recur (if (ident? bb)
                           (-> ret (conj local bv))
                           (pb ret bb bv))
                       (next bes)))
              ret))))

(comment 
 (defn destructure [bindings]
   (let [bents (partition 2 bindings)
     pb (fn pb [bvec b v]
            (let [pvec (fn [bvec b val] (ds-pvec pvec b val))
              pmap
             ]
              (cond
                (symbol? b) (-> bvec (conj b) (conj v))
                (vector? b) (pvec bvec b v)
                (map? b) (pmap bvec b v)
                :else (throw (new Exception (str "Unsupported binding form: " b))))))
     process-entry (fn [bvec b] (pb bvec (first b) (second b)))]
     (if (every? symbol? (map first bents))
         bindings
         (reduce1 process-entry [] bents))))

 )
