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


;;base case works....
(defparameter test-fn2
  (named-fn test-fn (x) 
	    (if (< x 2) x 
		(progn (print `(:calling ,x))
		       (test-fn (- x 2))))))
;;recur works just fine....
(defparameter test-fn3
  (named-fn test-fn (x) 
	    (if (< x 2) x 
		(progn (print `(:calling ,x))
		       (recur (- x 2))))))


(defparameter nftest  (named-fn* test-fn 
                                 ((x)    (+ x 1))
                                 ((x y)  (+ x y))                    
                                 ((&rest xs) (reduce #'+ xs))))


(defparameter nf (named-fn* test-fn 
                            ((coll) (when-let ((x (first coll)))
                                      (progn (pprint x) (recur (rest coll)))))
                            ((c1 c2) (test-fn c2))))
