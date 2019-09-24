;;quick test to see if we get compiler support for our
;;eval hack...
(ql:quickload :cl-package-locks)
(load "eval.lisp")

(defstruct blah (x))

(defparameter b (make-blah :x 2))

(defmethod clclojure.eval:custom-eval
    ((obj blah))
  (list :this-is-custom (blah-x obj)))

(clclojure.eval:enable-custom-eval)
(defparameter custom (eval b))
;;(:THIS-IS-CUSTOM 2)

(clclojure.eval:disable-custom-eval)
(defparameter normal (eval b))
;;#S(BLAH :X 2)

