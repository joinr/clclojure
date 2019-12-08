(defpackage common-utils.recurtest
  (:use :common-lisp :common-utils))
(in-package :common-utils.recurtest)

;;can we implement (recur ...) ?

;; (block some-name
;;   (tagbody some-point 
;;     :dostuff
;;      (when :recur
;;        (progn  (update-vars)
;;                (go some-point))
;;        )
;;       )
;;   result)

;; (defun custom-loop (x)
;;   (let ((res))
;;     (macrolet ((recur (xnew)
;;                  `(progn (setf ,'x ,xnew)
;;                          (pprint ,'x)
;;                          (go ,'recur-from))))
;;       (tagbody recur-from
;;          (setf res
;;                (if (= x 10)
;;                    x
;;                    (recur (1+  x)))))
;;       res)))

;; (defmacro with-recur (args &rest body)
;;   (let* ((recur-sym  (intern "RECUR")) ;HAVE TO CAPITALIZE!
;;          (local-args (mapcar (lambda (x)
;;                                (intern (symbol-name x))) args))
;;          (res        (gensym "res"))
;;          (recur-from (gentemp "recur-from"))
;;          (recur-args (mapcar (lambda (x) (gensym (symbol-name x))) local-args
;;                              ))
;;          (bindings   (mapcar (lambda (xy)
;;                                `(setf ,(car xy) ,(cdr xy))) (pairlis local-args recur-args))))
;;     `(let ((,res))
;;        (tagbody ,recur-from
;;           (flet ((,recur-sym ,recur-args
;;                    (progn ,@bindings
;;                           (go ,recur-from))
;;                    ))
;;             (setf ,res ,@body)))
;;        ,res)))


;; (defmacro with-recur (args &rest body)
;;   (let* ((recur-sym  (intern "RECUR")) ;HAVE TO CAPITALIZE!
;;          (local-args (mapcar (lambda (x)
;;                                (intern (symbol-name x))) args))
;;          (res        (gensym "res"))
;;          (recur-from (gentemp "recur-from"))
;;          (recur-args (mapcar (lambda (x) (gensym (symbol-name x))) local-args
;;                              ))
;;          (update-binds (gentemp "update-binds"))
;;          (bindings   (mapcar (lambda (xy)
;;                                `(setf ,(car xy) ,(cdr xy))) (pairlis local-args recur-args))))
;;     `(let ((,res))
;;        (flet ((,update-binds ,recur-args
;;                 (progn ,@bindings)))
;;          (macrolet ((,recur-sym ,args
;;                       `(progn (,,update-binds ,,@args)
;;                               (go ,,recur-from)))))
;;          (tagbody ,recur-from
;;             (setf ,res ,@body)))
;;        ,res)))

;; (with-recur (x 2)
;;   (if (< x 4)
;;       (recur (1+ x))
;;       x))

;; (let ((continue? t)
;;       (x 2)
;;       (res)
;;       (continue? nil))
;;   (flet ((recur (x)
;;            (setf x x)
;;            (setf continue? t)))   
;;     (tagbody recur-from
;;        (progn
;;          (setf res
;;                (if (< x 4)
;;                    (recur (1+ x))
;;                    x))
;;          (when continue?
;;            (setf continue? nil)
;;            (go recur-from))))
;;     res))

(comment
 ;;we can call simmary-tails on all these and get nil,
 ;;or some combination of (t, nil), (t, some-list-of illegal callsites)
 (defparameter normal-call
   `(if (= 2 3)
        :equal
        (progn (print :otherwise)
               :inequal)))

 (defparameter good-tail
   '(if (= 2 3)
     (recur 2)
     (recur 3)))

 (defparameter bad-tail
   '(progn
     (recur 2)
     3))

 (defparameter gnarly-bad-tail
   '(lambda (x)
     (with-recur (acc x)
       (let ((blah 5)
             (blee 3))
         (if (<= acc blah)
             (recur (1+ x))
             (progn (when (< 2 3)
                      (recur 44))
                    2))))))

 (defparameter gnarly-good-tail
   '(lambda (x)
     (with-recur (acc x)
       (let ((blah 5)
             (blee 3))
         (if (<= acc blah)
             (recur (1+ x))
             (progn (when (< 2 3)
                      (print 44))
                    2))))))
 )


                                        ;(with-recur (x 2 y 3) (+ x y))
(with-recur (x 0)
  (if (< x 10)
      (recur (1+ x))
      x))

(with-recur (x 0)
  (if (> x 9)
      x
      (recur (1+ x))))

(defun good-tail ()
  (with-recur (x 2)
    (if (> x 5)
        x
        (if (= x 2)
            (recur 5)
            (recur (1+ x))))))

;;not currently checked!
(defun bad-tail ()
  (with-recur ()
    (progn
      (recur 2)
      3)))

(defun gnarly-bad-tail (x)
  (with-recur (acc x)
    (let ((blah 5)
          (blee 3))
      (if (<= acc blah)
          (recur (1+ x))
          (progn (when (< 2 3)
                   (recur 44))
                 2)))))

(defun gnarly-good-tail (x)
  (with-recur (acc x)
    (let ((blah 5)
          (blee 3))
      (if (<= acc blah)
          (recur (1+ acc))
          (progn (when (< 2 3)
                   (print 44))
                 2)))))


;;test function for sussing out the correct way to
;;handle recur forms with varargs...
(defun tst ()
  (flet ((blah (&rest args) (pprint :hobart) nil))
    (macrolet ((recur (&whole whole-form &rest args)
                 (let* ((frm (list* 'apply (list 'function 'blah) args)))
                   (progn (pprint (list :expanding whole-form :to frm))
                          frm))))
      (labels ((blah (x &rest xs)
                 (pprint (list x xs))
                 (if (null xs) x
                     (progn (pprint (macroexpand-1 `(recur (+ ,x (first ,xs)) (rest ,xs))))
                            (recur (+ x (first xs)) (rest xs))))))
        #'blah))))

;; ;;looking at using with-recur...
;; ;;possible naive way to inline using with-recur...
;; (defun blah (&rest xs)
;;   (labels ((aux (&rest xs)
;;              (macrolet ((blah (&rest args)
;;                           `(apply #'aux ,args)))
;;                (case (length xs)
;;                  (1          (with-recur (x (first xs))
;;                                (pprint x)))
;;                  (2          (with-recur (acc   (first xs)
;;                                                 bound (second xs))
;;                                (if (< acc bound)
;;                                    (blah (+ acc 3) bound)
;;                                    acc)))
;;                  (otherwise   (with-recur ((x y &rest zs) xs)
;;                                 (+ x y (apply #'+ zs))))))))
;;     (apply #'aux xs)))

;; (with-recur (x 2)
;;   (if (< x 10) (recur (1+ x)) x))

;; (LET ((#:|continuex764| T) (#:|res763|) (X 2))
;;   (FLET ((RECUR (#:X765)
;;            (PROGN (SETF X #:X765) (SETF #:|continuex764| T))))
;;     (TAGBODY
;;        |recur-from2|
;;        (PROGN
;;          (SETF #:|res763|
;;                (IF (< X 10)
;;                    (RECUR (1+ X))
;;                    X))
;;          (WHEN #:|continuex764| (SETF #:|continuex764| NIL) (GO |recur-from2|))))
;;     #:|res763|))

;; (with-recur ((x &rest xs) xs)
;;   (if (null zs)
;;       (+ x y)
;;       (recur (+ x y)
;;              (first zs)
;;              (rest zs))))

;; (destructuring-bind (x y &rest zs) xs
;;   (LET ((#:|continuex764| T)
;;         (#:|res763|)
;;         (arg-X x)
;;         (arg-y y)
;;         (rest-zs zs))
;;     (FLET ((RECUR (x y &rest zs)
;;              (PROGN (SETF arg-X    x)
;;                     (setf arg-y    y)
;;                     (setf rest-zs zs)
;;                     (SETF #:|continuex764| T))))
;;       (TAGBODY
;;          |recur-fromvar|
;;          (PROGN
;;            (SETF #:|res763|
;;                  (IF (< X 10)
;;                      (RECUR (1+ X))
;;                      X))
;;            (WHEN #:|continuex764| (SETF #:|continuex764| NIL) (GO |recur-fromvar|))))
;;       #:|res763|)))


;;let's construct one from scratch..

(comment
 (defun blah (&rest xs)
   (let* ((blah-1         (named-fn blah-1 (x)
                                    (pprint x)))
          (blah-2         (named-fn blah-2 (acc bound)
                                    (if (< acc bound)
                                        (blah (+ acc 3) bound)
                                        acc)))
          (blah-variadic  (named-fn blah-variadic (x y &rest zs)
                                    (+ x y (apply #'+ zs)))))
     (case (length xs)
       (1          (funcall blah-1 (first xs)))
       (2          (funcall blah-2 (first xs) (second xs)))
       (otherwise  (apply blah-variadic xs) ))))
 
 ;;shouldn't blow the stack..but it does unless compiled.
 (defun blah (&rest xs)
   (let* ((blah-1         (named-fn blah-1 (x)
                                    (pprint x)))
          (blah-2         (named-fn blah-2 (acc bound)
                                    (if (< acc bound)
                                        (recur (+ acc 3) bound)
                                        acc)))
          (blah-variadic  (named-fn blah-variadic (x y &rest zs)
                                    (+ x y (apply #'+ zs)))))
     (case (length xs)
       (1          (funcall blah-1 (first xs)))
       (2          (funcall blah-2 (first xs) (second xs)))
       (otherwise  (apply blah-variadic xs) ))))

 )

(comment
 ;;testing -this works.
 (defparameter f (named-fn* blah
                            ((x)           (pprint (list x)) x)
                            ((x &rest xs)  (if (null xs) (blah x) (recur (+ x (first xs)) (rest xs))))))
 )


;; (defmacro named-fn* (name &rest args-bodies)
;;   (if (= (length args-bodies) 1)
;;       (let ((args-body (first args-bodies)))
;;  	`(named-fn ,name ,(first args-body) ,(second args-body))) ;regular named-fn, no dispatch.
;;       (destructuring-bind (cases var) (parse-dispatch-specs args-bodies)
;;         (let* ((args (gensym "args"))
;;                (funcspecs (mapcar (lambda (xs)
;; 				    (destructuring-bind (n (args body)) xs
;; 				      (let* ((fname (func-name name n))
;; 					     (fbody  `(named-fn ,fname ,args ,body)))
;; 					(if (= n 0)					    
;; 					    `(,n ,fname  ,fbody)
;; 					    `(,n ,fname  ,fbody))))) cases))
;;                (varspec   (when var 
;;                             (let* ((fname (func-name name :variadic))
;;                                    (fbody  `(named-fn ,fname ,(first var) ,(second var))))
;;                               `(:variadic ,fname ,fbody))))
;;                (specs     (if var (append funcspecs (list varspec)) funcspecs))
;;                (aux       (gensym "aux"))
;;                (dummy     (gensym "stupid-var")))     
;;           `(let ((,dummy nil) )
;;              (declare (ignore ,dummy))             
;;              (macrolet ((,name (,'&rest ,'args)
;;                           (list 'apply (list 'function (quote ,aux))  (list* 'list ,'args))))
;;                (let (,@(mapcar (lambda (xs) `(,(second xs) ,(third xs)))
;;                                specs))
;;                  (labels ((,aux (,'&rest ,args)
;;                             (case (length ,args)
;;                               ,@(mapcar (lambda (xs)  (let ((n (first xs))
;;                                                             (name (second xs)))
;;                                                         (if (= n 0) 
;;                                                             `(,n (funcall ,name))
;;                                                             `(,n (apply  ,name  ,args))))) funcspecs)
;;                               (otherwise ,(if var  `(apply  ,(second varspec) ,args)
;;                                               `(error 'no-matching-args))))))
;;                    (function ,aux)))))))))

;;testing
(comment
 (defparameter e
   (named-fn* blah
              ((acc bound)   (if (< acc bound) (recur (+ acc 3) bound) acc))
              ))
 (defparameter f
   (named-fn* blah
              ((acc bound)   (if (< acc bound) (recur (+ acc 3) bound) acc))
              ((x &rest xs)  (if (null xs) x (recur (+ x (first xs)) (rest xs))))
              ))

 ;;we shouldn't need progn...
 (defparameter g
   (named-fn* blah
              ((x)
               (progn 
                 (pprint (list :blah-1 :result x))
                 x))
              ((acc bound)
               (progn (pprint (list :blah-2 :counting acc :to bound))
                      (if (< acc bound) (recur (+ acc 3) bound) acc)))
              ((x &rest xs)
               (progn (pprint (list :blah-variadic :adding x :to xs))
                      (if (null xs) x   (recur (+ x (first xs)) (rest xs)))))))

 (defparameter h
   (named-fn* blah
              ((x)
               (progn 
                 (pprint (list :blah-1 :result x))
                 x))
              ((acc bound)
               (progn (pprint (list :blah-2 :counting acc :to bound))
                      (if (< acc bound) (blah (+ acc 3) bound) acc)))
              ;; ((x &rest xs)
              ;;  (progn (pprint (list :blah-variadic :adding x :to xs))
              ;;         (if (null xs) x   (recur (+ x (first xs)) (rest xs)))))
              ))
 
 )

;; (defmacro named-fn* (name &rest args-bodies)
;;   (if (= (length args-bodies) 1)
;;       (let ((args-body (first args-bodies)))
;;  	`(named-fn ,name ,(first args-body) ,(second args-body))) ;regular named-fn, no dispatch.
;;       (destructuring-bind (cases var) (parse-dispatch-specs args-bodies)
;;         (let* ((recur-sym  (intern "RECUR"))
;;                (args (gensym "args"))
;;                (funcspecs (mapcar (lambda (xs)
;; 				    (destructuring-bind (n (args body)) xs
;; 				      (let* ((fname (func-name name n))
;; 					     (fbody  `(named-fn ,fname ,args ,body)))
;; 					(if (= n 0)					    
;; 					    `(,n ,fname  ,fbody)
;; 					    `(,n ,fname  ,fbody))))) cases))
;;                (varspec   (when var 
;;                             (let* ((fname (func-name name :variadic))
;;                                    (fbody  `(named-fn ,fname ,(first var) ,(second var))))
;;                               `(:variadic ,fname ,fbody))))
;;                (specs     (if var (append funcspecs (list varspec)) funcspecs))
;;                (aux       (gensym "aux")))
;;           `(let ((,name))
;;              (macrolet ((,name (,'&rest ,args)
;;                           (list* 'funcall ',name ,args)
;;                           ))
;;                (let* (,@(mapcar (lambda (xs) `(,(second xs) ,(third xs))) specs))
;;                  (labels ((,aux (,'&rest ,args) 
;;                             (case (length ,args)
;;                               ,@(mapcar (lambda (xs)  (let ((n (first xs))
;;                                                             (name (second xs)))
;;                                                         (if (= n 0) 
;;                                                             `(,n (funcall ,name))
;;                                                             `(,n (apply  ,name ,args))))) funcspecs)
;;                               (otherwise ,(if var  `(apply  ,(second varspec) ,args)
;;                                               `(error 'no-matching-args))))))
;;                    (setf ,name (lambda (&rest ,args) (apply ,aux ,args)))
;;                    (labels ((,name (,'&rest ,args) (apply ,name ,args)))
;;                      (function ,aux))))))))))

;;testing
(comment 
 (defparameter the-func
   (lambda* 
    (() 2)
    ((x) (+ x 1))
    ((x y) (+ x y))
    ((&rest xs) (reduce #'+ xs))))

 )
