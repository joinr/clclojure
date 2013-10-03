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
;;4) Multimethds.  Need to find a way to implement multiple dispatch.
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
;;more?

;;loading handled by asdf now.
;;(load "common-utils.lisp") ;useful stuff like comment, keyword creation, and more.
;;(load "protocols.lisp")
;;(load "pvector.lisp") ;imports reader macros for vector literals i.e. [1 2 3] 
;(load "pmap.lisp")  ;once pmap is finished, it'll be really useful for  
;(load "seq.lisp") ;might be nice to go ahead and clone the seq library to make this simpler.
(defpackage :clclojure.base
  (:use :common-lisp :common-utils :clclojure.pvector :clclojure.protocols))
(in-package clclojure.base)




;;Some basic stuff to facilitate clojure forms.

;;I think we want to use persistent maps for meta data, as clojure does.
;;I want to get the stubs in place, and am using property lists with a 'meta 
;;entry pointing at an assoc list for now.
(defmacro meta (symb) `(get (quote ,symb) 'meta))
(defmacro with-meta (symb m) `(setf (get (quote ,symb) 'meta) ,m))

(defprotocol IMeta 
    (meta (obj)))

;;move this later...
(defun vector? (x) (typep x 'clclojure.pvector::pvec))

;;(defun meta (obj) 
;;  (cond (symbolp obj) (eval `(get (quote ,obj) 'meta))

;;One thing about metadata, and how it differs from property lists: 
;;You can call meta on datastructures, or objects, and get a map back.
;;Symbols can have meta called on them with (meta #'the-symbol), which 
;;uses sharp-quote to get the symbol, vs the symbol-name.

;;Clojure is a lisp-1, so we need to ensure that everything, even 
;;functions, gets bound into the a single namespace. 

;;another way to do this is to have clojure-specific symbols be actual 
;;clos objects, which have meta data fields automatically.  Then we 
;;lose out on all the built in goodies from common lisp though.

;;Experimental.  Not sure of how to approach this guy.
(defmacro def (var &rest init-form)
  `(progn (defparameter ,var ,@init-form)
          (with-meta ,var '((symbol . t) (doc . "none")))
	  (quote ,var)))

;;We have a lisp1, sorta! 
;;I need to add in some more evaluation semantics, but this might be the 
;;way to go.  For now, it allows us to have clojure semantics for functions 
;;and macros.  There's still some delegation to the common lisp evaluator - which is 
;;not a bad thing at all!
(defun eval-clojure (expr)
  (cond ((atom expr)  (eval expr))
	((listp expr) (let* ((f (car expr)) ;must be a function...
			    (res (eval f))) ;resolve the function			
			(if (functionp res) ;apply the function
			    (apply res (mapcar #'eval-clojure (rest expr)))
			    (eval expr))))
	))	    


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

;;functions (specifically lambda lists) are going to be complicated...
;;need a way to define variadic functions...
;;in clojure, functions dispatch on the arity of args...

;; (defn the-function
;;     ([x y & rest] (reduce the-function x (conj rest y)))      
;;     ([x y] (do-stuff x y))
;;     ([x]   (the-function x 10)))

;;That's just dealing with arity.
;;-> 
;; (def the-function 
;;   (aux fn [& args] 
;;        (case (count args)
;; 	 1 (aux (first args) 10)
;; 	 2 (do-stuff (first args) (second args))
;; 	 3 (reduce aux (first args) (conj (ffirst args) (rest args))))))

;; (defun the-function     
;;     (labels 
;; 	((func1 (lambda (&args args) (the-function (first args) 10)))
;; 	 (func2 (lambda (&args args) (do-stuff (first args) (second args))))
;; 	 (func3 (lambda (&args args) (reduce the-function (first args) (conj (ffirst args) (rest args))))))
;;       (case (count args)
;; 	1 (func1 args)
;; 	2 (func2 args)
;; 	3 (func3 args))))      

;;using the convention that all binding forms are vectors.
;;we detect multiple-arity functions by seeing if the args is a list of vectors.

;;(fn ([args1] body1) ([args2] body2)) 
;;we can say..
;;(fn & args) 

(defun destructure-bind (args body)
  (list args body))
 

;;go from [x y z] (+ x y z) -> ( (3 (x y z))  (+ x y z))
(defun process-function-body (args body) nil)

;;and check out what the args looks like...
;;args could be [], [...], or ([] [...] [...])
;;(defun args->spec (args)
;;  (cond ((vector? args) (list (vector-count args) args)
;;	 (listp args)  (process-function-bodies args)))) ;actually function bodies. 
			  

(defun multiple-arity? (args) (listp args))

;;we'll deal with loop/recur later...

(defun var-args (args) (member '& args))
(defun variadic? (args) (not (null (var-args args))))

(defun spec->lambda (args body)
  (let ((n (length args)))
    `((,n ,(var-args args))  (lambda ,args ,body))))


;;(build-function (specs)
;;   (dolist ((args body) 

;;only one function can be variadic in a fn form 


;;testing function building.
(defun do-stuff (x y) (format nil "~A,~A" x y))

;;our specs for the-function.
(def specs 
    '(( (x) (do-stuff x 10))
      ( (x y) (do-stuff x y))
      ( (x y & rest) (reduce do-stuff x (conj y rest)))))

;;this works as well.
(def vec-specs 
    '(([x] (do-stuff x 10))
      ([x y] (do-stuff x y))
      ([x y & rest] (reduce do-stuff x (conj y rest)))))
  
;;In reality, we could have an assload of arities to deal with, since clojure 
;;allows it (up to some arbitrary number like 20?) 


;;Aside from that, for each specific function spec, we need to parse the args 
;;and introduce an appropriate function spec....

;;That's where destructuring comes in. 
;;destructuring takes a binding form of a vector 

;;The following two forms really boil down to 
;;(fn [map-arg] ...)
;;(fn [&map-arg] ...)

;;map destructuring...
;;(fn [&{:keys [x y] :or {x v1 y v2} :as my-map}] ~body) ->  

;;(lambda (&keys (x v1) (y v2))
;;   (let* ((my-map (hash-map x v1 y v2)))
;;        ,@body

;;(fn [{:keys [x y] :or {x v1 y v2} :as my-map}] ~body) -> 
;;(lambda (my-map))
;;   (let* ((x (or (get my-map x) v1))
;;          (y (or (get my-map y) v2)))
;;        ,@body)

;;The following forms boil down to 
;;(fn [vector-arg arg] body)
;;(fn [& vector-arg]  body)

;;(fn [[x y] z] body) ->
;; (lambda (coll z)
;;   (let* ((x (first coll))
;;           y (second coll)))
;;       ,@body)

;;(fn [& xs] ~body) -> (lambda (&args xs) body)

;;destructuring, in general, is the process of transforming a sequence of 
;;args into an appropriate binding.  
;;since args are unified to always be in vectors, it's tranforming a vector 
;;into something.

;;a binding form may include primitive symbols, with the symbol & delineating 
;;list arguments.  everything after & is to be bound to a list.
;;note -> further destructuring may occur here...
;;[x y & z] 
;;So, we recursively destructure the binding form.  
;;We can bifurcate the process more if we split the destructuring into special cases.
;;The first case is that we haven't hit & yet, so we are collecting simple structures. 
;;The second case is after we've hit &. 

;;If we are in case 1, we traverse the vector of symbols and see if they're compound. 
;;The only cases to handle are 
;;Symbol   -> Symbol 
;;Vector   -> (destructure vector)  ;;recursive call 
;;hash-map -> (destructure hash-map) ;;more complex destructuring.



(comment  ;testing
  (def the-val 2)
  (def symbol? #'symbolp)
  (def add-two (lambda (x) (+ 2 x)))
  (eval-clojure '(symbol? the-val)) ;=> nil
  (eval-clojure '(add-two the-val)) ;=> 4
)


(defmacro doc (v) `(pprint (rest (assoc 'DOC (meta ,v)))))   
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

;;Most of these will currently break, since the protocols use a clojure vector spec
;;and my defprotocol uses lists.  Need to bridge the gap...Still, getting them 
;;implemented will be hugely good, as it's the backbone of just about everything 
;;else.

;;;;;;;;;;;;;;;;;;;;;;;;;;; core protocols ;;;;;;;;;;;;;

;;Need to get back to this guy...multiple arity is not yet implemented...
;; (defprotocol IFn
;;   (-invoke
;;     [this]
;;     [this a]
;;     [this a b]
;;     [this a b c]
;;     [this a b c d]
;;     [this a b c d e]
;;     [this a b c d e f]
;;     [this a b c d e f g]
;;     [this a b c d e f g h]
;;     [this a b c d e f g h i]
;;     [this a b c d e f g h i j]
;;     [this a b c d e f g h i j k]
;;     [this a b c d e f g h i j k l]
;;     [this a b c d e f g h i j k l m]
;;     [this a b c d e f g h i j k l m n]
;;     [this a b c d e f g h i j k l m n o]
;;     [this a b c d e f g h i j k l m n o p]
;;     [this a b c d e f g h i j k l m n o p q]
;;     [this a b c d e f g h i j k l m n o p q s]
;;     [this a b c d e f g h i j k l m n o p q s t]
;;     [this a b c d e f g h i j k l m n o p q s t rest]))


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
  (-reduce [coll f] [coll f start]))

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

(defprotocol ^:deprecated IPrintable
  "Do not use this.  It is kept for backwards compatibility with existing
   user code that depends on it, but it has been superceded by IPrintWithWriter
   User code that depends on this should be changed to use -pr-writer instead."
  (-pr-seq [o opts]))

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