;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; (ns cljs.tools.reader.impl.commons
;;     (:refer-clojure :exclude [char])
;;     (:require
;;      [cljs.tools.reader.impl.errors :refer [reader-error]]
;;      [cljs.tools.reader.reader-types :refer [peek-char read-char]]
;;      [cljs.tools.reader.impl.utils :refer [numeric? newline? char]]))

(defpackage cljs.tools.reader.impl.commons
  (:use :cl :clclojure.pvector :clclojure.cowmap :clclojure.protocols)
  (:shadowing-import-from :clclojure.base
   :def :fn :defn :ex-info :instance? :defrecord :true :false :identical? :nil? :when-not
   :hash-map :string? :keyword? :vector? :symbol? :nth :vec :vector :let :cond :re-find
   :re-matches :get :subs
   :-> :parse-float :if-not :when-let :if-let := :== :count :char? :pos? :inc)
  (:shadowing-import-from :cljs.tools.reader.impl.errors :reader-error)
  (:shadowing-import-from :cljs.tools.reader.impl.reader-types :peek-char :read-char)
  (:shadowing-import-from :cljs.tools.reader.impl.utils   :numeric? :newline? :char)
  (:local-nicknames (:base :clclojure.base)
                    (:re :cl-ppcre)))
(in-package :cljs.tools.reader.impl.commons)
(named-readtables:in-readtable clj-re:readtable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;^boolean
;;"Checks whether the reader is at the start of a number literal"
(defn number-literal?
  (reader initch)
  (or (numeric? initch)
      (and (or (identical? #\+ initch) (identical?  #\- initch))
           (numeric? (peek-char reader)))))

;; "Read until first character that doesn't match pred, returning
;;  char."
(defn read-past
  (pred  rdr)
  (base:loop (ch (read-char rdr))
        (if (pred ch) ;;^boolean
            (recur (read-char rdr))
            ch)))

;;  "Advances the reader to the end of a line. Returns the reader"
(defn skip-line
  (reader)
  (base:loop ()
        (when-not (newline? (read-char reader))
                  (recur)))
  reader)

(def int-pattern #"^([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?$")
(def ratio-pattern #"([-+]?[0-9]+)/([0-9]+)")
(def float-pattern #"([-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")

(defn match-int
  (s)
  (let (m (vec (re-find int-pattern s))) ;;maybe slowish...
    (if-not (nil? (get m 2))
            0
            (let (negate? (base:= "-" (get m 1)) ;;^boolean, strings aren't identical in cl.
                  a       (cond
                            (not (nil? (get m 3))) (vector  (get m 3) 10)
                            (not (nil? (get m 4))) (vector  (get m 4) 16)
                            (not (nil? (get m 5))) (vector  (get m 5) 8)
                            (not (nil? (get m 7))) (vector  (get m 7) (parse-integer (get m 6)))
                            :else              (vector nil nil))
                  n (get a 0))
              (when-not (nil? n)
                        (let (bn (parse-integer n :radix (get a 1))
                              bn (if negate? (* -1 bn) bn))
                          #-sbcl
                          (when-not (js/isNaN bn)
                                    bn)
                          bn
                          ))))))

;;replacing numerator and denominator since CL has these bound.
(defn match-ratio
  (s)
  (let (m (vec (re-find ratio-pattern s))
        numer (get m 1)
        denom (get m 2)
        numer (if (re-find #"^\+" numer)
                      (subs numer 1)
                      numer))
    (/ (-> numer   parse-integer) ;;; No ratio type in cljs
       (-> denom parse-integer)))); So will convert to js/Number

;;haven't thought about BigDecimals yet....we parse everything to CL numbers.
(defn match-float
  (s)
  (let (m (vec (re-find float-pattern s)))
    (if-not (nil? (get m 4)) ;; for BigDecimal "10.03M", as all parsed to js/Number
            (parse-float (get m 1))
            (parse-float s))))

;;^boolean
;;need to munge dbind to convert & to &rest....
(defn  matches? (pattern s)
  (when-let (res (re-find pattern s))
    (base:= (base:first res) s)))

(defn match-number (s)
  (if (matches? int-pattern s)
      (match-int s)
      (if (matches? float-pattern s)
          (match-float s)
          (when (matches? ratio-pattern s)
            (match-ratio s)))))

;;migrate these to string ns.
(defn ends-with? (x suffix)
  (and (<= (count suffix) (count x))
       (string= x suffix :start1  (- (length x) (length suffix)))))

(defn starts-with? (x prefix)
  (and (<= (count prefix) (count x))
       (string= x prefix :end1 (length prefix))))

;;a little compatibility function to work with .indexOf replacement.s
(defn idx-of (x s)
  (let (c (cond (char? s) s
                (string? s) (nth s 0)
                :else (base:throw (ex-info "invalid char|string" (hash-map :in s :x x)))))
    (or (position c x)
        -1)))

;;clj version is closer.;
;;"Parses a string into a vector of the namespace and symbol"
(defn parse-symbol
  (token)
  (when-not (or (= "" token)
                (starts-with? token ":")
                (starts-with? token "::"))
            (let (ns-idx (idx-of token "/")) ;;equiv to index-of for /
              (if-let (ns (and (pos? ns-idx)
                               (subs token 0 ns-idx)))
                (let (ns-idx (inc ns-idx))
                  (when-not (== ns-idx (count token))
                            (let (sym (subs token ns-idx))
                              (cond
                                (re-matches #"[1-9]" sym)
                                (vector  ns sym)
                                (and (not (numeric? (nth sym 0)))
                                     (not (= "" sym))
                                     (not (ends-with? ns ":"))
                                     (or (= sym "/")
                                         (== -1 (idx-of sym "/"))))
                                (vector ns sym)))))
                (when (or (= token "/")
                          (== -1 (idx-of token "/")))
                  (vector  nil token))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;need to test these bro.
(defn read-comment
    (rdr & _)
  (skip-line rdr))

(defn throwing-reader
  (msg)
  (fn (rdr & _)
      (reader-error rdr msg)))
