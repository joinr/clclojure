;;A package for defining read table extensions 
;;for clojure data structures.  

;;Pending..................
(defpackage :clclojure.reader
  (:use :common-lisp :common-utils :named-readtables)
  (:export :nth-vec :*literals* :quoted-children :quote-sym))
(in-package :clclojure.reader)

(comment 
 (defconstant +left-bracket+ #\[)
 (defconstant +right-bracket+ #\])
 (defconstant +left-brace+ #\{)
 (defconstant +right-brace+ #\})
 (defconstant +comma+ #\,)
 (defconstant +colon+ #\:)


 (defconstant +at+ #\@)
 (defconstant +tilde+ #\~))

(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)

  ;;Problem right now is that, when we read using delimited-list,
  ;;we end up losing out on the reader literal for pvecs and the like...
  ;;When we have quoted 
  
  ;;we can use a completely custom reader...perhaps that's easiet..
  ;;Have to make this available to the compiler at compile time!
  ;;Maybe move this into a clojure-readers.lisp or something.

  ;;alist of literals...
  (defparameter *literals*  '())
  ;;default quote...
  ;; (comment 
  ;;  (set-macro-character #\'
  ;;                       #'(lambda (stream char)
  ;;                           (declare (ignore char))
  ;;                           `(quote ,(read stream t nil t)))))
  (defun    quote-sym (sym) (list 'quote sym)) ;`(quote ,sym)
  ;; (defmacro quoted-children (c)  
  ;;   `(,(first c)  ,@(mapcar #'quote-sym  (rest c))))

  (defun literal? (s) (and (listp s) (member (first s) *literals*)))
  (defmacro quoted-children (c)
    `(,(first c)
      ,@(mapcar (lambda (s)
                  (if  (literal? s) ;;we need to recursively call quoted-children..
                       `(quoted-children ,s)
                       (funcall #'quote-sym s)))  (rest c))))
  
  ;;Enforces quoting semantics for literal data structures..
  (defmacro clj-quote (expr)
    (if (literal? expr)
        `(quoted-children ,expr)
        (quote-sym expr)))

  ;; (set-macro-character #\\
  ;;    #'(lambda (stream char)
  ;;        (let ((res (read stream t nil t)))
  ;;          (if (characterp res) res
               
  ;;              )))
  ;;    )
                       
  ;;This should be consolidated...
  (set-macro-character #\'
     #'(lambda (stream char)
         (declare (ignore char))
         (let ((res (read stream t nil t)))
           (if (atom res)  `(quote ,res)
               `(clj-quote ,res)))))

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


;;https://gist.github.com/chaitanyagupta/9324402
;;https://common-lisp.net/project/named-readtables/


