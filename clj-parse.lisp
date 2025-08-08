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
   :.defn-expr))
(in-package :clj-parse)
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
(defun .normal ()
  (.let* ((arg-body (.item)))
    (if (and (listp arg-body)
             (= (length arg-body) 2))
        (let ((args (first  arg-body))
              (body (second arg-body)))
          (if (and 
               (parse! (.args) (list  args))
               (parse! (.body) body))
              (.identity (list :normal args :body body))
              (.fail)))
        (.fail))))

(defun .normal ()
  (.label :normal 
          (.tuple  (.args) (.label :body (.body)))))

;;one or more normals,
;;where the arg lengths are distinct.

(defun .variadic ()
  (.let* ((defs  (.only  (.map 'list (.normal)))))
    (let* ((args (mapcar #'second defs))
           (counts (remove-duplicates  (mapcar #'length args))))
      (if (= (length  counts) (length args))
          (.identity (list :variadic defs))
          (.fail)))))

(defun .fn ()
  (.or   (.only  (.normal))
         (.variadic)))

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

(defun .defn-expr ()
  (.let* ((_  (.func (x)
                     (string= (string-downcase  (symbol-name x)) "defn")))
          (spec (.defn-args)))
    (if spec (.identity spec) (.fail))))

(defparameter tst
  '(defn interleave
      (() '())
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

;; (defparameter tst2
;;   (concatenate 'list
;;     '(defn interleave)
;;     (list "This is a docstring bro." (make-hash-table))
;;     '((() '())
;;       ((c1) (lazy-seq c1))
;;       ((c1 c2)
;;        (lazy-seq
;;         (let (s1 (seq c1) s2 (seq c2))
;;           (when (and s1 s2)
;;             (cons (first s1) (cons (first s2)
;;                                    (interleave (rest s1) (rest s2))))))))
;;       ((c1 c2 &rest colls)
;;        (lazy-seq
;;         (let (ss (map seq (conj colls c2 c1)))
;;           (when (every? identity ss)
;;             (concat (map first ss) (apply interleave (seq->list  (map rest ss)))))))))))


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
