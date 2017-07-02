;;A package for defining read table extensions 
;;for clojure data structures.  

;;Pending..................
(defpackage :clclojure.reader
  (:use :common-lisp :common-utils)
  (:export :nth-vec :*literals*))
(in-package :clclojure.reader)

(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  ;;Have to make this available to the compiler at compile time!
  ;;Maybe move this into a clojure-readers.lisp or something.

  (defparameter *quoted-chars* '(#\[ #\{ ) )
  ;;alist of literals...
  (defparameter *literals*  '())
  ;;default quote...
  ;; (comment 
  ;;  (set-macro-character #\'
  ;;                       #'(lambda (stream char)
  ;;                           (declare (ignore char))
  ;;                           `(quote ,(read stream t nil t)))))
  (defun    quote-sym (sym) `(quote ,sym))
  (defmacro quoted-children (c)  
    `(,(first c)  ,@(mapcar #'quote-sym  (rest c))))
  ;;This should be consolidated...
  (set-macro-character #\'
    #'(lambda (stream char)
        (declare (ignore char))
        (let ((res (read stream t nil t)))
          (if (atom res)  `(quote ,res)
              (if-let ((lit-ctor (member (first res) *literals*)))
                `(quoted-children ,res)
                `(quote ,res))))))

  (defun push-reader! (literal ldelim rdelim rdr)
    (progn (setf  *literals* (union  (list literal) *literals*))
           (set-macro-character ldelim rdr)
           (set-syntax-from-char rdelim #\))))
  
  ;; (comment (defun |brace-reader| (stream char)
  ;;            "A reader macro that allows us to define persistent vectors
  ;;   inline, just like Clojure."
  ;;            (declare (ignore char))
  ;;            `(persistent-vector ,@(read-delimited-list #\] stream t)))
  ;;          (set-macro-character #\{ #'|brace-reader|)
  ;;          (set-syntax-from-char #\} #\))

  ;;          ;;standard quote dispatch
  ;;          (set-macro-character #\'  #'(lambda (stream char)
  ;;                                        (list 'quote (read stream t nil t))))
           
           
  ;;          (set-macro-character #\'  #'(lambda (stream char)
  ;;                                        (let ((res (read stream t nil t)))
  ;;                                          (case (first res)
  ;;                                            ('persistent-vector 'persistent- ))
  ;;                                          (list 'quote )))))
  )  
