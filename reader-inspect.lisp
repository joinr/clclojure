;;Original license from tools.reader

;;   Copyright (c) Russ Olsen, Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;(ns cljs.tools.reader.impl.inspect)
(defpackage cljs.tools.reader.impl.inspect
  (:use :cl :clclojure.pvector :clclojure.cowmap :clclojure.protocols)
  ;;(:shadow :char)
  (:shadowing-import-from :clclojure.base
   :do :def :defn :let :ex-info :instance? :defrecord :true :false :identical? :nil? :when-not
   :hash-map :string? :keyword? :vector? :symbol? :list? :set? :map? :number? :set! :inc :dec :str :throw :aget :aset
   :zero? :when-let :assoc :dissoc :conj :disj :merge :with-meta :meta :subs :declare-clj :count
   :partial :map :take :str :interpose :concat := :cond :defmulti :defmethod-clj :apply)
  (:shadow :truncate :inspect)
  (:local-nicknames (:base :clclojure.base))
  (:export :inspect))
(in-package :cljs.tools.reader.impl.inspect)
(named-readtables:in-readtable clj-re:readtable)

;(setf clclojure.base::*clj-verbose* t) ;;noisy warnings for now.

(declare-clj inspect*)

(defn inspect*-col (truncate col start end)
  (let (n (count col)
        l (if truncate 0 (min 10 n))
        elements  (map (partial #'inspect* true) (take l col))
        content   (apply #'str (interpose " " elements))
        suffix (if (< l n) "..." ""))
    (str start content suffix end)))

(defn dispatch-inspect
    (arg x)
  (cond
    (nil? x) :nil
    (string? x) :string
    (keyword? x) :strable
    (number? x) :strable
    (symbol? x) :strable
    (vector? x) :vector
    (list? x)  :list
    (map? x) :map
    (set? x) :set
    (identical? x true) :strable ;;we don't have equivalence in yet.
    (identical? x false) :strable
    :default (type-of x)))

(defmulti inspect* dispatch-inspect)

(defmethod-clj inspect* :string
  (truncate  x)
  (let (n (if truncate 5 20)
        suffix (if (> (count x) n) "...\"" "\""))
      (str
       #\"
       (subs x 0 (min n (count x)))
       suffix)))

(defmethod-clj inspect* :strable (truncate x) (str x))

;;these are all specific concrete types that cljs.core defines.
;;we don't have them right now, but we can bring them in if we
;;need to.  I don't think we do at the moment....

;; (defmethod-clj inspect* cljs.core/IndexedSeq [truncate x]
;;   "<indexed seq>")

;; (defmethod-clj inspect* cljs.core/PersistentArrayMapSeq [truncate x]
;;   "<map seq>")

;; (defmethod-clj inspect* cljs.core/NodeSeq [truncate x]
;;   "<map seq>")

;;(defmethod-clj inspect* cljs.core/Cons [truncate x] "<cons>")

;;(defmethod-clj inspect* cljs.core/LazySeq [truncate x] "<lazy seq>")

(defmethod-clj inspect* 'SEQUENCES::LAZYSEQ (truncate x) "<lazy seq>")
(defmethod-clj inspect* 'SEQUENCES::FUNCSEQ (truncate x) "<lazy seq>")

(defmethod-clj inspect* :nil (truncate x) "nil")

(defmethod-clj inspect* :list (truncate col)
    (inspect*-col truncate col #\( #\)))

;;high water mark.
(defmethod-clj inspect* :map (truncate m)
  (let (len (count m)
        n-shown (if truncate 0 len)
        contents (apply concat (take n-shown m))
        suffix (if (> len n-shown) "...}" #\}))
      (inspect*-col truncate contents #\{ suffix)))

(defmethod-clj inspect* :set (truncate col)
    (inspect*-col truncate col "#{" #\}))

(defmethod-clj inspect* :vector (truncate col)
    (inspect*-col truncate col #\[ #\]))

;;don't have pr-str defined in base yet.
;;pending.
;;default method not working, sAD!
(defmethod-clj inspect* :default (truncate x)
  (;pr-str
   princ-to-string
   (type-of x)))

;; "Return a string description of the value supplied.
;;    May be the a string version of the value itself (e.g. \"true\")
;;    or it may be a description (e.g. \"an instance of Foo\").
;;    If truncate is true then return a very terse version of
;;    the inspection."
(defn inspect
  ((x) (inspect* false x))
  ((truncate x) (inspect* truncate x)))
