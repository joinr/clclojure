;;playing with parser combinators
;;for lambda arg parsing.
;;we'll monkey patch smug for now....
(defpackage :clj-parse
  (:use :common-lisp :smug)
  (:export
   :.cat
   :.func
   :.item
   :parse!
   :run!
   :show!
   :.empty-list
   :.list-of
   :flatten
   :.error
   :.label
   :.&
   :.if
   :.when
   :.unless
   :.rest
   :.only
   :.symbol-args
   :.dbind
   :.arg
   :.body
   :.args
   :.normal
   :.variadic
   :.fn
   :.defn-args
   :.defn-expr
   :.fn-expr))
(in-package :clj-parse)

(defun .sym (sym)
  (.&  (.is #'symbolp)
       (.func (x)  (string=  (string-downcase  (symbol-name x)) sym))))

(defun .key (sym)
  (let ((keyname (string-downcase (symbol-name sym))))
    (.&  (.is #'keywordp)
         (.func (x)  (string=  (string-downcase  (symbol-name x)) keyname)))))
;;we have a grammar for function bodies.
;; destructuring-bind-form :: 
;; dbind      :: destructuring-bind-form
;; arg        :: symbol | dbind
;; body       :: atom | list
;; normal     :: ((list* arg) body)
;; variadic   :: normal+

;;a destructuring arg is either
;;an atom, a finite list of atoms,
;;or a list of destructuring args.
;;.map is too permissive.  if want to parse
;;only successes
(defun .dbind ()
  (.let*  ((nxt (.item)))
    (if (symbolp nxt)
        (.identity nxt)
        (if (listp nxt)
            (let ((res (parse!  (.only  (.list-of (.dbind))) (list nxt))))
              (if res
                  (.identity  res)
                  (.fail)))
            (.fail)))))

;;we can probably start defining labels for our parse
;;results like spec does.
(defun .arg ()
  (.or (.is #'atom)
       (.is #'listp)))

#-sbcl
(defun .body ()
  (.or (.is #'atom)
       (.is #'listp)))

(defun .args ()
  (.let* ((args (.or (.empty-list)
                     (.list-of (.arg)))))
    (.identity (list :args args))))

;;would be nice to have this on board.
;; lambdalist :: (list* arg rest? options? keys?)

;;can't have symbol functions in args.
#-sbcl
(defun .normal ()
  (.let* ((arg-body (.item)))
    (if (and (listp arg-body)
             #-sbcl (= (length arg-body) 2))
        (let ((args (first  arg-body))
              (body (rest arg-body)))
          (if (and 
               (parse! (.args) (list  args))
               (parse! (.body) body))
              (.identity (list :normal (list  (list  :args args)  (list :body body))))
              (.fail)))
        (.fail))))
#-sbcl
(defun .normal2 ()
  (.label :normal2
          (.tuple  (.args)  (.label :body (.body)))))
#-sbcl
(defun .simplified-body ()
  (.let* ((xs (.rest)))
    (if (= (length xs) 1)
        (.identity (first xs))
        (.identity `(progn ,@xs)))))

;;corresponds to a single function body,
;;with (args &rest body) form.
(defun .normaln ()
  (.label :normal
          (.tuple (.args)
                  (.label :body (.rest)))))

;;a variadic fn is just a list of normal fns.
;;so instead of mapping over the rest of the input,
;;we map new subparsings for each item in the list.

;;given ((args1 body1 body1a) (args2 body2)) as specs
;;we know - at least - each spec is a list.
;;so we have a list of lists, which we want to sub-parse
;;as normalns.

;;maybe abstracted.
#-sbcl
(defun .one-of (p)
  (.let* ((itm (.item)))
    (let* ((res (parse! p itm)))
      (if res
          (.identity res)
          (.fail)))))

;;something like
;;this is a pattern that could show up more,
;;where we want to parse the item as if it's the
;;parse context.
(defun .funcspec ()
  (.& (.is #'listp)
      (.let* ((itm (.item)))
        (let* ((res (parse! (.normaln) itm)))
          (if res
              (.identity res)
              (.fail))))))

;;one or more normals,
;;where the arg lengths are distinct.
;;problem - .rest will consume the parse tree.
;;we need to structure it better, so that we're comparing multiple
;;lists, and parsing them individually with normaln.
(defun .variadic ()
  (.let* ((defs  (.only  (.map 'list (.funcspec)))))
    (let* ((args (mapcar (lambda (spec)
                           (destructuring-bind (tg ((ag xs) (b body))) spec
                             xs))
                         defs))
           (counts (remove-duplicates  (mapcar (lambda (arglist)
                                                 (if (keywordp (first arglist)) ;;:EMPTY-LIST
                                                     0
                                                     (length arglist))) args))))
      (if (= (length  counts) (length args))
          (.identity (list :variadic defs))
          (.fail)))))

;;we allow a lame escape hatch where if caller
;;supplies :multi after a fn definition, we
;;branch into variadic pattern.
;;We can throw in a warning about ambiguous parse too,
;;if we parse a normal, we see if it could parse as
;;multi body, and if so, warn user (or throw).
(defun .explicit-multi ()
  (.let* ((k  (.key :multi))
          (body (.variadic)))
    (if body
        (.identity body)
        (.fail))))

;;mildly janky....
(defun .fn ()
  (.or  (.explicit-multi)
       ; (.only  (.normal))
        (.variadic)
        (.only (.normaln))))

#-sbcl
(defun .fn-all ()
  (.or (.variadic)
         (.only  (.normal))         
        (.only (.normal2))))


;; '(fn (x y) (+ x y))
;; '((x y) (+ x y))
;; '(fn ((x y) z) (+ x y z))
;; '(((x y) z) (+ x y z))

;;https://github.com/clojure/core.specs.alpha/blob/master/src/main/clojure/clojure/core/specs/alpha.clj
;;defines the grammar.

;; (s/def ::defn-args
;;        (s/cat :fn-name simple-symbol?
;;               :docstring (s/? string?)
;;               :meta (s/? map?)
;;               :fn-tail (s/alt :arity-1 ::params+body
;;                               :arity-n (s/cat :bodies (s/+ (s/spec ::params+body))
;;                                               :attr-map (s/? map?)))))

;; (defun .defn-args ()
;;   (.cat :fn-name   (.is #'symbolp)
;;         :docstring (.optional (.is #'stringp))
;;         :meta      (.optional (.is #'hash-table-p)) ;;will change to map? later...
;;         :fn-tail   (.rest)))

;; (defun .defn-expr ()
;;   (.let* ((_  (.func (x)
;;                      (string= (string-downcase  (symbol-name x)) "defn")))
;;           (spec (.defn-args)))
;;     (let* ((tail          (assoc :fn-tail spec))
;;            (params-bodies (parse! (.fn) (second tail))))
;;       (if params-bodies
;;           (progn  (rplacd tail (list  params-bodies))
;;                   (.identity spec))
;;           (.fail)))))

(defun .defn-args ()
  (.cat :fn-name   (.is #'symbolp)
        :docstring (.optional (.is #'stringp))
        :meta      (.optional (.is #'hash-table-p)) ;;will change to map? later...
        :fn-tail   (.fn)))

;; (defun .defn-expr ()
;;   (.let* ((_  (.func (x)
;;                      (string= (string-downcase  (symbol-name x)) "defn")))
;;           (spec (.defn-args)))
;;     (if spec (.identity spec) (.fail))))

(defun .defn-expr ()
  (.and  (.sym "defn")
         (.defn-args)))

(defun .fn-expr ()
  (.and  (.sym "fn")
         (.cat :fn-name  (.optional  (.is #'symbolp))
    ;;             :docstring (.optional (.is #'stringp))
   ;;              :meta      (.optional (.is #'hash-table-p)) ;;will change to map? later...
               :fn-tail   (.fn))))

(defparameter tst
  '(defn interleave
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
          (concat (map first ss) (apply interleave (seq->list  (map rest ss))))))))))

(defparameter tst2
  (concatenate 'list
    '(defn interleave)
    (list "This is a docstring bro." (make-hash-table))
    '((() nil)
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
            (concat (map first ss) (apply interleave (seq->list  (map rest ss)))))))))))

;;getting our asses kicked on this one.
;;we can't parse it right now.
;;we have a variadic function,
;;which has more than 2 args in each spec.
;;args are
;;(coll) 
;;so the body is
;;( (dorun coll) coll)
;;then
;;(n coll)
;;((dorun n coll) coll) 

;;This freaks out our parsing...
;;and we conform the result to a normal
;;2-element function spec, where
;;args
(defparameter ambig
  '(FN DOALL
    ((COLL)     (DORUN COLL) COLL)
    ((N COLL)  (DORUN N COLL) COLL)))

;;we could side-step this ambiguity on the runtime side
;;by signaling variadic at compile time in meta.

;;since it will only matter for implementation and for ambiguous cases...
;;maybe we can detect ambiguity and cry out?

;;that seems cheaper than having to muck with the cl reader for now.
;;e.g. it can provide a forced safety hatch to ensure parsing is correct
;;when we need it.

;;since this only matters for bootstrapping, user level code won't
;;hit this ever (unless maybe doing interop where it's cl->clj invoking
;;bootstrapped stuff?)

(defparameter unambig
  '(FN DOALL
    :multi
    ((COLL)     (DORUN COLL) COLL)
    ((N COLL)  (DORUN N COLL) COLL)))

;;more complex implementation
;; ;;;; destructure

;; (s/def ::local-name (s/and simple-symbol? #(not= '& %)))

;; (s/def ::binding-form
;;        (s/or :local-symbol ::local-name
;;              :seq-destructure ::seq-binding-form
;;              :map-destructure ::map-binding-form))

;; ;; sequential destructuring

;; (s/def ::seq-binding-form
;;        (s/and vector?
;;               (s/cat :forms (s/* ::binding-form)
;;                      :rest-forms (s/? (s/cat :ampersand #{'&} :form ::binding-form))
;;                      :as-form (s/? (s/cat :as #{:as} :as-sym ::local-name)))))

;; ;; map destructuring

;; (s/def ::keys (s/coll-of ident? :kind vector?))
;; (s/def ::syms (s/coll-of symbol? :kind vector?))
;; (s/def ::strs (s/coll-of simple-symbol? :kind vector?))
;; (s/def ::or (s/map-of simple-symbol? any?))
;; (s/def ::as ::local-name)

;; (s/def ::map-special-binding
;;        (s/keys :opt-un [::as ::or ::keys ::syms ::strs]))

;; (s/def ::map-binding (s/tuple ::binding-form any?))

;; (s/def ::ns-keys
;;        (s/tuple
;;         (s/and qualified-keyword? #(-> % name #{"keys" "syms"}))
;;         (s/coll-of simple-symbol? :kind vector?)))

;; (s/def ::map-bindings
;;        (s/every (s/or :map-binding ::map-binding
;;                       :qualified-keys-or-syms ::ns-keys
;;                       :special-binding (s/tuple #{:as :or :keys :syms :strs} any?)) :kind map?))

;; (s/def ::map-binding-form (s/merge ::map-bindings ::map-special-binding))

;; ;; bindings

;; (defn even-number-of-forms?
;;   "Returns true if there are an even number of forms in a binding vector"
;;   [forms]
;;   (even? (count forms)))

;; (s/def ::binding (s/cat :form ::binding-form :init-expr any?))
;; (s/def ::bindings (s/and vector? even-number-of-forms? (s/* ::binding)))

;; ;; let, if-let, when-let

;; (s/fdef clojure.core/let
;;         :args (s/cat :bindings ::bindings
;;                      :body (s/* any?)))

;; (s/fdef clojure.core/if-let
;;         :args (s/cat :bindings (s/and vector? ::binding)
;;                      :then any?
;;                      :else (s/? any?)))

;; (s/fdef clojure.core/when-let
;;         :args (s/cat :bindings (s/and vector? ::binding)
;;                      :body (s/* any?)))

;; ;; defn, defn-, fn

;; (s/def ::param-list
;;        (s/and
;;         vector?
;;         (s/cat :params (s/* ::binding-form)
;;                :var-params (s/? (s/cat :ampersand #{'&} :var-form ::binding-form)))))

;; (s/def ::params+body
;;        (s/cat :params ::param-list
;;               :body (s/alt :prepost+body (s/cat :prepost map?
;;                                                 :body (s/+ any?))
;;                            :body (s/* any?))))

;; (s/def ::defn-args
;;        (s/cat :fn-name simple-symbol?
;;               :docstring (s/? string?)
;;               :meta (s/? map?)
;;               :fn-tail (s/alt :arity-1 ::params+body
;;                               :arity-n (s/cat :bodies (s/+ (s/spec ::params+body))
;;                                               :attr-map (s/? map?)))))

;; (s/fdef clojure.core/defn
;;         :args ::defn-args
;;         :ret any?)

;; (s/fdef clojure.core/defn-
;;         :args ::defn-args
;;         :ret any?)

;; (s/fdef clojure.core/fn
;;         :args (s/cat :fn-name (s/? simple-symbol?)
;;                      :fn-tail (s/alt :arity-1 ::params+body
;;                                      :arity-n (s/+ (s/spec ::params+body))))
;;         :ret any?)
