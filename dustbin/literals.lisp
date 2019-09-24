(defpackage clclojure.literals
  (:use :common-lisp
        :clclojure.eval
        :clclojure.pvector
        :clclojure.cowmap))
(in-package :clclojure.literals)
;;Data Literal Eval Semantics

(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (clclojure.eval:enable-custom-eval)
  ;;(eval [x y z]) => (vector (eval x) (eval y) (eval z))
  ;;this is somewhat inefficient since we're not exploiting
  ;;chunks, but good enough for proof of concept.  We do
  ;;have chunks, fyi.
  (defmethod custom-eval ((obj pvec))
    (vector-map (lambda (x) (eval x)) obj))

  (defmethod custom-eval ((obj subvector))
    (vector-map (lambda (x) (eval x)) obj))

  ;;(map {x y j k} => (persistent-map (eval x) (eval y) (eval j) (eval k))
  (defmethod custom-eval ((obj cowmap))
    (reduce (lambda (acc kv)
              (destructuring-bind (k v) kv
                (map-assoc acc (eval k) (eval v)))) (map-seq obj) :initial-value (empty-map)))
)
