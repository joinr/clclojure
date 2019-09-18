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


;;quasiquoting of literals now works...
(defparameter quasi-form
  `[,@(list 1 2 ) ,x ,@(list :literal  x :hah)
    [,x ,x ,x [x]
    {:a 2
     :b {:unquote ,x}
     :c {:quoted x}}]])

;;[1 2 2 :LITERAL 2 :HAH [2 2 2 [X] {:C {:QUOTED X} :B {:UNQUOTE 2} :A 2}]]

;;coming soon...
;;meta, destrutcturing, core clojure functions
;;per cljs, and more...

;;loop/recur (maybe not necessary since we can compile
;;on most implementations and get TCO)


;;Working on variadic protocol implementations,
;;will be addressed in clclojure.variadic
(defprotocol IMany
    (many [obj] [obj msg]))

;;currently broken, close to fixing...
(deftype manytest []
  IMany
  (many [this] :one!)
  (many [this item] item))


;;error in vector, vector args aren't being evaluated.
(defn test-my-scope []
  (let
      [hello :hello
       world :world
       k 2
       inc (fn [x] (+ x 1))
       add (fn [x y] (+ x y))
       tbl (let [tbl (make-hash-table)]
              (setf  (gethash :hello tbl) "World")
              (setf  (gethash :world tbl) "Hello")
              (setf  (gethash :k  tbl)    k)
             tbl)]
    (list  (hello tbl)
           (world tbl)
           (add (inc 39) k)
           (gethash :k tbl) ;;(:k tbl);;WIP
           )))

;;EXAMPLE> (test-my-scope)
;;("World" "Hello" 42 2)

;;named functions don't currently parse!
;;This fails too, we have some jank with the
;;reader when we're inside a macro...
;;Need to fix the quasi quoter, should
;;be in backtick.lisp.
(defn test-arities []
  (let [sum (fn ([x] x)
                ([x y] (+ x y))
                ([x y &rest zs] (reduce #'+ zs :initial-value (+ x y))))]
    (clclojure.pvector:persistent-vector
     (sum 1)
     (sum 1 2)
     (sum 1 2 3 4 5 6))))
