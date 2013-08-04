;;A package for useful library utilities that come up during the course
;;of implementing clclojure.
(defpackage :common-utils
  (:export  :comment :make-keyword :stringify :symb))
(in-package :common-utils)

;;same as clojure's comment macro.
(defmacro comment (&rest xs))

;;Converts thing to a keyword representation.  Used in building library funcs.
(defun make-keyword (thing) 
  (values (intern (string-upcase thing) :keyword)))

;;Turn x into a string.  
(defun stringify (x) (format nil "~a" x))

;;Turn xs, assumably strings, into a symbol as if typed at the repl.
(defun symb (&rest xs)
  (eval (read-from-string (concatenate 'string xs))))