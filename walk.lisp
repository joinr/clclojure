;;a simple code walking package based off clojure.walk, with
;;severe limitations.
(defpackage :clclojure.walk
  (:use :common-lisp :common-utils)
  (:export :walk :prewalk :postwalk :prewalk-replace :postwalk-replace) ;:loop :defmacro
  )
(in-package :clclojure.walk)

;; "Traverses form, an arbitrary data structure.  inner and outer are
;;   functions.  Applies inner to each element of form, building up a
;;   data structure of the same type, then applies outer to the result.
;;   Recognizes all Clojure data structures. Consumes seqs as with doall."

;; {:added "1.1"}
(defun walk (inner outer form)
  (cond  ((listp form)
          (funcall outer (apply #'list (mapcar (lambda (x) (funcall inner x)) form))))
         (t (funcall outer form))))

;; "Like postwalk, but does pre-order traversal."
;; {:added "1.1"}
(defun prewalk (f form)
  (walk (lambda (x)
          (prewalk f x))  #'identity (funcall f form)))

(defun postwalk (f form)
  (walk (lambda (x)
          (postwalk f x))  f form))

;; "Recursively transforms form by replacing keys in smap with their
;;   values.  Like clojure/replace but works on any data structure.  Does
;;   replacement at the root of the tree first."
;; {:added "1.1"}
(defun prewalk-replace (f form)
  (prewalk (lambda (x) (let ((res (funcall f x)))
                         (if res res x))) form))

(defun postwalk-replace (f form)
  (postwalk (lambda (x) (let ((res (funcall f x)))
                          (if res res x))) form))

