;;Original license from tools.reader:

;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(defpackage cljs.tools.reader.impl.utils
  (:use :cl :clclojure.pvector :clclojure.cowmap :clclojure.protocols)
  (:shadow :char)
  (:import-from :clclojure.base
   :def :defn :ex-info :instance? :defrecord :true :false :identical? :nil? :when-not
   :hash-map :string? :keyword? :vector? :symbol?)
  (:local-nicknames (:base :clclojure.base)
                    (:re :cl-ppcre)))
(in-package :cljs.tools.reader.impl.utils)
(named-readtables:in-readtable clj-re:readtable)

(defn char (x)
  (base::when-not (base:nil? x)
            (base:char x)))

(defn ex-info? (ex)
  (instance? (find-class 'COMMON-UTILS:EXCEPTION-INFO) ex))

(defrecord ReaderConditional (splicing? form))

;;"Return true if the value is the data representation of a reader conditional"
(defn reader-conditional?  (value)
  (instance? ReaderConditional value))

;; "Construct a data representation of a reader conditional.
;;   If true, splicing? indicates read-cond-splicing."
(defn reader-conditional
    (form splicing?)
  (->ReaderConditional splicing? form))

(extend-protocol
 base::IPrintWithWriter
 ReaderConditional
 (base::-pr-writer (coll writer opts)
             (base::-write writer (str "#?" (when (base:get coll :splicing?) "@")))
             (base:pr-writer (base:get coll :form) writer opts)))

(def ws-rx #"[\s]")

;;in cljs all chars are strings due to how js stores them.
;;we have actual char primitives, so prefer to use them instead.

;;"Checks whether a given character is whitespace"
(defn whitespace?
  (ch)
  (when-not (nil? ch)
            (if (identical? ch #\,)
                true
                ;;(.test ws-rx ch)
                (re::whitespacep ch) ;;use ppcre helpers.
                )))

;;"Checks whether a given character is numeric"
(defn numeric? (ch)
  (when-not (nil? ch)
            (digit-char-p ch)))
;;"Checks whether the character is a newline"
(defn newline? (c)
  (or (identical? #\newline c)
      ;;(identical? "\n" c) ;;does this track?
      (nil? c)))

;;"Resolves syntactical sugar in metadata" ;; could be combined with some other desugar?
(defn desugar-meta
    (f)
  (base:cond
    (keyword? f) (hash-map  f true)
    (symbol? f)  (hash-map  :tag f)
    (base::string? f)  (hash-map  :tag f)
    (base::vector? f)  (hash-map  :param-tags f)
    :else        f))

(def last-id (base:atom 0))


(defn next-id ()
  (base:swap! last-id base:inc))

;;replace for with map for now.
(defn namespace-keys [ns keys]
  (for [key keys]
       (if (or (symbol? key)
               (keyword? key))
           (let [[key-ns key-name] ((juxt namespace name) key)
             ->key (if (symbol? key) symbol keyword)]
             (cond
               (nil? key-ns)
               (->key ns key-name)

               (= "_" key-ns)
               (->key key-name)

               :else
               key))
           key)))

(defn key-maker (k)
  (if (symbol? k)
      #'base::clj-symbol
      #'base::keyword))

(defn namespace-keys (ns keys)
  (base::->>
   keys
   (base:map
        (base:fn (k)
            (if (or (symbol? k)
                    (keyword? k))
                (base:let (ns-name (funcall (base:juxt base:namespace base:name) k)
                           key-ns   (base:first ns-name)
                           key-name (base:second ns-name)
                           ->key    (key-maker k))
                  (base:cond
                    (nil? key-ns)       (->key ns key-name)
                    (base:= "_" key-ns) (->key key-name)
                    :else k))
                k)
            ))))

;;formerly second'
(defn second> (a-b)
  (destructuring-bind (a b) a-b
    (when-not a b)))

;;do we need this?
;;I think we have this built-in already...
;;formerly char-code
(defn char-code> (ch base)
  (base:let (code (parse-integer (base:str ch) :radix 10 :junk-allowed t))
    (if (not code)
        -1
        code)))
