(ql:quickload :clclojure)

(defpackage :clclojure.example
  (:use  :common-lisp :clclojure.base))
(in-package :clclojure.example)

;;we have persistent vectors, which will
;;be replaced by bootstrapped variants from
;;clclojur.base...

(def v [:a :b :c ])

