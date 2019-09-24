(defgeneric blah (obj))

(defparameter *global-msg* "global!")

(let ((local-msg "local1!")) 
  (progn 
    (defclass test1 () ())
    (defmethod blah ((obj test1)) local-msg)))

(blah (make-instance 'test1))

(let [local-msg "local2!"]
  (progn 
    (defclass test2 () ())
    (defmethod blah ((obj test2)) local-msg)))


(def x
    (let [local-msg "local3!"]
      (progn 
        (defclass test3 () ())
        (defmethod blah ((obj test3)) local-msg))))

(blah (make-instance 'test3))

(defprotocol IBlee (blee [this]))
(def result
    (let [local-msg "local4!"]
      (progn 
        (defclass test4 () ())
        (eval '(DEFMETHOD BLEE ((THIS TEST4)) LOCAL-MSG)))))


(defgeneric blah (obj))
(defclass test () ())

(let ((local-msg "local1!"))
  (DEFMETHOD BLAH ((THIS TEST)) LOCAL-MSG))

;;=>(blah (make-instance 'test))
;;"local1!"

(let ((local-msg "local2!"))
  (eval (list 'DEFMETHOD 'BLAH '((THIS TEST4)) LOCAL-MSG)))
