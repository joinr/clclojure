;;A package for defining read table extensions 
;;for clojure data structures.  

;;Pending..................
(defpackage :clclojure.reader
  (:use :common-lisp :common-utils :named-readtables :clclojure.pvector :clclojure.cowmap)
  (:export :*literals* :*reader-context* :quoted-children :quote-sym :literal?))
(in-package :clclojure.reader)

(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)

  ;;Problem right now is that, when we read using delimited-list,
  ;;we end up losing out on the reader literal for pvecs and the like...
  ;;When we have quoted 
  
  ;;we can use a completely custom reader...perhaps that's easiet..
  ;;Have to make this available to the compiler at compile time!
  ;;Maybe move this into a clojure-readers.lisp or something.

  ;;alist of literals...
  (defparameter *literals* '(list)      ; '(list cons)
    )
  (defparameter *reader-context* :read)
  ;;default quote...o
  ;; (comment 
  ;;  (set-macro-character #\'
  ;;                       #'(lambda (stream char)
  ;;                           (declare (ignore char))
  ;;                           `(quote ,(read stream t nil t)))))
  (defun    quote-sym (sym) (list 'quote sym)) ;`(quote ,sym)
  ;; (defmacro quoted-children (c)  
  ;;   `(,(first c)  ,@(mapcar #'quote-sym  (rest c))))

  (defun dotted-pair? (xs)
    (and (listp xs)
         (not  (listp (cdr xs)))))
  
  (defun literal? (s) (or  (and (listp s)   (find (first s) *literals*))
                           (and (symbolp s) (find s *literals*))))
  
  (defmacro quoted-children (c)
    `(,(first c)
      ,@(mapcar (lambda (s)
                  (cond ((literal? s) ;;we need to recursively call quoted-children..
                         `(quoted-children ,s))
                        ((dotted-pair? s)
                         `(quote ,s))
                        ((listp s)
                         `(quoted-children ,(cons  (quote list) s)))
                        (t (funcall #'quote-sym s))))  (rest c))))
  
  ;;Enforces quoting semantics for literal data structures..
  ;;We may not need this anymore since we hacked eval.
  (defmacro clj-quote (expr)
    
    (cond ((literal? expr)     `(quoted-children ,expr))
          ((dotted-pair? expr) `(quote  ,expr))
          ((listp expr)
           `(quoted-children ,(cons  (quote list) expr)))
          (t
           (quote-sym expr))))  
  
  (defun as-char (x)
    (cond ((characterp x) x)
          ((and (stringp x)
                (= 1 (length x))) (char x 0))
          ((symbolp x) (as-char (str x)))
          (t (error (str (list "invalid-char!" x) ))))
    )
  
  ;;Gives us clj->cl reader for chars...
  (set-macro-character #\\
                       #'(lambda (stream char)
                           (declare (ignore char))
                           (let ((res (read stream t nil t)))
                             (as-char res)))
                       )

  ;;Doesn't work currently, since we can't redefine
  ;;print-method for chars...
  (defun print-clj-char (c &optional (stream t))
    "Generic char printer for clojure-style syntax."
    (format stream "\~c" c))

  (defun print-cl-char (c &optional (stream t))
    "Generic char printer for common lisp syntax."
    (format stream "#\~c" c))

  (defun quoted-read (stream char)
    (declare (ignore char))
    (let ((res (read stream t nil t)))
      (if (atom res)  `(quote ,res)
          `(clj-quote ,res))))
  
  ;;This should be consolidated...
  (set-macro-character #\'
                       #'quoted-read)


  ;;need to define quasiquote extensions...
  ;;quasiquoting has different behavior for literal datastructures..
  ;;in the case of clojure, we provide fully-qualified symbols vs.
  ;;standard CL-symbols.  We have reader support for them,
  ;;that is, blah/x vs x.

  ;;so, clojure resolves the symbol in the current ns, at read-time.

  ;; (defun resolved-symbol (s)
  ;;   (let* ((this-package (package-name *package*)))
  ;;     (multiple-value-bind (x  y)
  ;;         (find-symbol (symbol-name s))
  ;;       (if x
  ;;           ;;symbol exists
  ;;           `(,(package-name (symbol-package x)) ,(symbol-name x))
  ;;           `(,this-package ,(symbol-name s))
  ;;           ))))

  ;; (defun qualify (s)
  ;;   (apply #'common-utils::symb
  ;;          (let ((res (resolved-symbol s)))
  ;;            (list (first res) "::" (second res)))))
  
  ;; (defun quasi-quoted-read (stream char)
  ;;   (declare (ignore char))
  ;;   (let ((res (read stream t nil t)))
  ;;     (cond ((symbolp res)
  ;;            (let ((resolved )))
  ;;            `(quote ,res))
  ;;           (t              `(clj-quote ,res)))))
  
  ;;Additionally, for dataliterals, quasiquote serves as a template
  ;;for building said datastructure, as if by recursively quasiquoting
  ;;elements in the expression.

  ;;Additionally, clojure 

  ;;we can get package-qualified symbols via:
  ;;`(common-lisp-user::x)
  ;;but they print as 'x
  
  ;;s.t. `[x y]
  ;;namespace-qualified symbols are kind of out of bounds at the
  ;;moment...
    
  (defun push-reader! (literal ldelim rdelim rdr)
    (progn (setf  *literals* (union  (list literal) *literals*))
           (set-macro-character ldelim rdr)
           (set-syntax-from-char rdelim #\))))

  (defun quoting? () (> sb-impl::*backquote-depth* 0))
  
  ;;This now returns the actual pvector of items read from
  ;;the stream, versus a quoted form.  Should work nicely
  ;;with our protocol definitions now!


  ;;The issue we run into with our EDN forms is this:
  ;;(defparameter x 2)
  ;;(eval [x])
  ;;should yield [2]

  ;;we don't currently.
  ;;So,

  ;;Quasiquoting custom data literals..
  ;;===================================
  ;;THis is way janky...
  ;;I'm not afraid to say I don't know how I pulled this off.
  ;;The key is that the quasiquoting mechanism in backq.lisp
  ;;has a sb-int:comma struct to denote 3 kinds of commas:
  ;;0 -> ,x
  ;;1 -> ,.x
  ;;2 -> ,@x
  
  ;;We ignore the dot version for now, although it's probably simple
  ;;enough to get working.
  ;;So we just manually build the expression.
  ;;If it's not a comma, we quasiquote it and let the macroexpander
  ;;figure it out.
  (defun quasify (xs)
    (nreverse
     (reduce  (lambda (acc x) x
                (if (sb-int:comma-p x)
                    (let ((expr (sb-int:comma-expr x)))
                      (case (sb-int:comma-kind x) 
                        (0  (cons expr acc))
                        (2  (reduce (lambda (a b) (cons b a)) (eval  expr) :initial-value acc))
                        (1  (error "comma-dot not handled!"))))
                    (cons (list 'sb-int:quasiquote x) acc))) xs :initial-value '())))
  
  ;;Original from Stack Overflow, with some slight modifications.
  ;;Have to make this available to the compiler at compile time!
  ;;Maybe move this into a clojure-readers.lisp or something.
  ;;We need to modify this.  It implicity acts like quote for
  ;;symbols, since we're using read-delimited-list.
  (defun |bracket-reader| (stream char)
    "A reader macro that allows us to define persistent vectors
    inline, just like Clojure."
    (declare (ignore char))
    (if (not (quoting?))
        (apply #'persistent-vector (read-delimited-list #\] stream t))
        (eval `(persistent-vector ,@(quasify (read-delimited-list #\] stream t))))))

  ;;Original from Stack Overflow, with some slight modifications.
  (defun |brace-reader| (stream char)
    "A reader macro that allows us to define persistent maps
   inline, just like Clojure."
    (declare (ignore char))
    (if (not (quoting?))
        (apply #'persistent-map `(,@(read-delimited-list #\} stream t)))        
        (eval `(persistent-map ,@(quasify (read-delimited-list #\} stream t))))))
  
  (set-macro-character #\{ #'|brace-reader|)
  (set-syntax-from-char #\} #\))
  
  ;;TODO move to named-readtable
  (push-reader! 'persistent-vector  #\[ #\] #'|bracket-reader|)
  ;;TODO move to named-readtable
  (push-reader! 'clclojure.pvector:persistent-vector  #\[ #\] #'|bracket-reader|))
  
(comment
 ;;WIP, moving to more elegant solution from named-readtables....
 ;; (defreadtable clojure:syntax
 ;;   (:merge :standard)
 ;;   (:macro-char #\[ #'|bracket-reader| t)
 ;;   (:case :preserve))
 )
  
 
  ;;https://gist.github.com/chaitanyagupta/9324402
  ;;https://common-lisp.net/project/named-readtables/


  
