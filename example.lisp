(ql:quickload :clclojure)

(defpackage :clclojure.example
  (:use  :common-lisp :clclojure.base))
(in-package :clclojure.example)

;;we have persistent vectors, which will
;;be replaced by bootstrapped variants from
;;clclojure.base...

;;some clojure-0 expressions demonstrating
;;fundamentals of the language primitives...

;;def 
(def v [1 2 3])

;;fn - single-arity, no meta, no destructuring
(def f (fn [x] (+ x 2)))

;;naive defn (no meta, no destructuring
(defn plus [x y] (+ x y))

(plus 1 2)

(defn idx [v n]
  (clclojure.pvector:nth-vec v n))

(defprotocol IBlah
    (blah [obj]))

(deftype blather [name x]
  IBlah
  (blah [this] name))

