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
  (defmethod clclojure.eval::literal? ((obj pvec)) t)
  (defmethod custom-eval ((obj pvec) &optional env)
    (vector-map (lambda (x) (clclojure.eval:custom-eval-in-lexenv  x env)) obj))

  ;; (defmethod let-expr   ((obj pvec))
  ;;   `(clclojure.eval:literal
  ;;     (clclojure.pvector:persistent-vector ,@(clclojure.pvector:vector-to-list obj))))
  
  (defmethod let-expr   ((obj pvec))
    obj)

  (defmethod clclojure.eval::literal? ((obj subvector)) t)
  (defmethod custom-eval ((obj subvector) &optional env)
    (vector-map (lambda (x) (clclojure.eval:custom-eval-in-lexenv  x env)) obj))
  
  (defmethod let-expr   ((obj subvector))
    `(clclojure.eval:literal
      (clclojure.pvector:persistent-vector ,@(clclojure.pvector:vector-to-list obj))))

  (defmethod clclojure.eval::literal? ((obj cowmap)) t)
  ;;(map {x y j k} => (persistent-map
  ;;(clclojure.eval:custom-eval-in-lexenv x)
  ;;(clclojure.eval:custom-eval-in-lexenv y)
  ;;(clclojure.eval:custom-eval-in-lexenv j)
  ;;(clclojure.eval:custom-eval-in-lexenv k))
  
  (defmethod custom-eval ((obj cowmap) &optional env)
    (reduce (lambda (acc kv)
              (destructuring-bind (k v) kv
                (map-assoc acc (clclojure.eval:custom-eval-in-lexenv  k env)
                           (clclojure.eval:custom-eval-in-lexenv  v env))))
            (map-seq obj) :initial-value (empty-map)))

  (defmethod let-expr   ((obj cowmap))
    `(clclojure.eval:literal
      (clclojure.cowmap:persistent-map
       ,@(reduce (lambda (acc kv)
                   (cons (first kv) (cons (second kv) acc)))
                 (clclojure.cowmap:map-seq obj) :initial-value '()))))

  
)
