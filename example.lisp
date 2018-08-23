(ql:quickload :clclojure)

;;we'll eventually morph this
;;into an ns call somehow....
(defpackage :clclojure.example
  (:use  :common-lisp :clclojure.base)
  (:shadowing-import-from :clclojure.base
   :deftype :let))
(in-package :clclojure.example)

;;we have persistent vectors, which will
;;be replaced by bootstrapped variants from
;;clclojure.base...

;;some clojure-0 expressions demonstrating
;;fundamentals of the language primitives...

;;def and defn export by default,
;;also unify the function and symbol
;;namespaces.  Working on metadata and
;;reader support...

(def v [1 2 3])

;;some core protocol stuff...
(clclojure.base::-conj v 4)
;;=> [1 2 3 4]

(clclojure.base::-count v)
;;=> 3

;;fn - single-arity, no meta, no destructuring
(def f (fn [x] (+ x 2)))

;;naive defn (no meta, no destructuring)
(defn plus [x y] (+ x y))

(plus 1 2)
;;=> 3

(defprotocol IBlah
    (blah [obj]))

(defprotocol IBlee
    (blee [obj msg]))

(deftype blather [name x]
  IBlah
  (blah [this]  (str :blaH! name x)))

(def the-blather (->blather :joinr "blech!!!"))

(blah the-blather)
;;gives us ":BLAH!:JOINRblech!!!"

;;reify works....under current single arity limitations
;;we generate effectively an anonoymous, throwaway
;;CLOS class via deftype, letting deftype do the work..

;;Note: we get warnings about being unable to find
;;the specializer class for the reified class,
;;need to check that out, may be missing a quote.
;;It works tho!
(def the-blither
  (let [msg "HOHOHO, MEEERRRRYY REIFY"]
    (reify
     IBlah
     (blah [this] msg)
     IBlee
     (blee [this custom-msg] (str "custom! " custom-msg) ))))

(blah the-blither)
;;gives us "HOHOHO, MEEERRRRYY REIFY"
(blee the-blither "Honk!")
;;gives us "custom! Honk!"

;;protocols are just structs....
IBlah
;; #S(CLCLOJURE.PROTOCOLS::PROTOCOL
;;    :NAME IBLAH
;;    :FUNCTIONS (BLAH)
;;    :SATISFIER #<CLOSURE (LAMBDA (CLCLOJURE.PROTOCOLS::NEWSPEC)
;;                           :IN
;;                           CLCLOJURE.PROTOCOLS::MAKE-SATISFIER) {10074575AB}>
;;    :MEMBERS (REIFY1 BLATHER))

;;On the cusp of greatness, but still
;;debuggin multiple arities! So close....
;; (defn idx [v n]
;;   (-nth v n))

;;coming soon...
;;meta, destrutcturing, core clojure functions
;;per cljs, and more...

;;loop/recur (maybe not necessary since we can compile
;;on most implementations and get TCO)


;;Working on variadic protocol implementations,
;;will be addressed in clclojure.variadic
(defprotocol IMany
    (many [obj] [obj msg]))

(deftype manytest []
  IMany
  (many [this] :one!)
  (many [this item] item) )




