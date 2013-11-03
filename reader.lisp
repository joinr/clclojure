;;A package for defining read table extensions 
;;for clojure data structures.  

;;Pending..................

(defpackage :clclojure.reader
  (:use :common-lisp)
  (:export :nth-vec))
(in-package :clclojure.reader)

;;Have to make this available to the compiler at compile time!
;;Maybe move this into a clojure-readers.lisp or something.

(defparameter *quoted-chars* '(#\[ #\{))

(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (defun |bracket-reader| (stream char)
    "A reader macro that allows us to define persistent vectors
    inline, just like Clojure."
    (declare (ignore char))
    `(persistent-vector ,@(read-delimited-list #\] stream t)))
  (set-macro-character #\[ #'|bracket-reader|)
  (set-syntax-from-char #\] #\))

  (defun |brace-reader| (stream char)
    "A reader macro that allows us to define persistent vectors
    inline, just like Clojure."
    (declare (ignore char))
    `(persistent-vector ,@(read-delimited-list #\] stream t)))
  (set-macro-character #\[ #'|brace-reader|)
  (set-syntax-from-char #\] #\))

;;standard quote dispatch
  (set-macro-character #\'  #'(lambda (stream char)
				(list 'quote (read stream t nil t)))))