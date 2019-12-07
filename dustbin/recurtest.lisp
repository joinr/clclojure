(defpackage common-utils.recurtest
  (:use :common-lisp :common-utils))
(in-package :common-utils.recurtest)

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
