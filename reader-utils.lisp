;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.
;; (ns cljs.tools.reader.impl.utils
;;     (:refer-clojure :exclude [char])
;;     (:require
;;      [clojure.string :as string]
;;      [goog.string :as gstring]))

(defpackage cljs.tools.reader.impl.utils
  (:use :cl :clclojure.pvector :clclojure.cowmap :clclojure.protocols)
  (:shadow :char)
  (:import-from :clclojure.base :def :defn :ex-info :instance? :defrecord)
  (:local-nicknames (:base :clclojure.base)
                    (:re :cl-ppcre)))
(in-package :cljs.tools.reader.impl.utils)


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

;;high water mark
(extend-protocol
 base::IPrintWithWriter
 ReaderConditional
 (-pr-writer (coll writer opts)
             (-write writer (str "#?" (when (base:get coll :splicing?) "@")))
             (pr-writer (base:get coll :form) writer opts)))

(def ws-rx #"[\s]")

(defn ^boolean whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (when-not (nil? ch)
            (if (identical? ch \,)
                true
                (.test ws-rx ch))))

(defn ^boolean numeric?
  "Checks whether a given character is numeric"
  [ch]
  (when-not (nil? ch)
            (gstring/isNumeric ch)))

(defn ^boolean newline?
  "Checks whether the character is a newline"
  [c]
  (or (identical? \newline c)
      (identical? "\n" c)
      (nil? c)))

(defn desugar-meta
  "Resolves syntactical sugar in metadata" ;; could be combined with some other desugar?
  [f]
  (cond
    (keyword? f) {f true}
    (symbol? f)  {:tag f}
    (string? f)  {:tag f}
    (vector? f)  {:param-tags f}
    :else        f))

(def last-id (atom 0))

(defn next-id
  []
  (swap! last-id inc))

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

(defn second' [[a b]]
  (when-not a b))

(defn char-code [ch base]
  (let [code (js/parseInt ch base)]
    (if (js/isNaN code)
        -1
        code)))
