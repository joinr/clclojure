;;monkey patch our combinator lib to extend
;;its utility bro.
(in-package :smug) 
(defstruct parse-state remaining (errors nil))

(defmethod smug/smug:input-empty-p ((xs parse-state))
  (null (parse-state-remaining xs)))

(defmethod smug/smug:input-rest ((xs parse-state))
  (with-slots (remaining) xs
    (make-parse-state :remaining
                      (if (atom remaining)
                          nil
                          (rest remaining)))))

(defmethod smug/smug:input-first ((xs parse-state))
  (with-slots (remaining) xs
    (if (atom remaining)
        remaining
        (first remaining))))

(defun .item ()
  (lambda (input)
    (unless (input-empty-p input)
      (list (cons (input-first input)
                  (input-rest input))))))

;;use out parse-state.
(defun parse! (parser input)
  (parse parser (make-parse-state :remaining input)))

(defun run! (parser input)
  (run parser (make-parse-state :remaining input)))

(defun show! (parser input)
  (multiple-value-bind (res &rest others) (parse! parser input)
    res))

(defun .empty-list ()
  (.let* ((nxt (.item)))
    (if (null nxt)
        (.identity (list :empty-list))
        (.fail))))

(defun .list-of (parser)
  (.let* ((nxt (.is #'listp)))
    (let ((res (parse! 
                (.map 'list parser) nxt)))
      (if res
          (.identity res)
          (.fail)))))

;;we'll break down the error checking thusly:
;;just get the shape of the args correct.
;;then validate the specs.
;;it's too rough to do error handling with parser combinators
;;but it's trivial to validate staged parses...

;;borrowed from common-utils temporarily.
(defun flatten (expr)
  (labels ((aux (acc xs)
	     (if (atom xs) xs
		 (progn (dolist (x xs)
			  (if (atom x) (push x acc)
			      (let ((res (nreverse (aux (list) x))))
				(mapcar (lambda (x) (push x acc)) res))))
			acc))))
    (nreverse (aux (list) expr))))

(defstruct smug-parse-error data)

;;goofy
(defun .error (&optional msg)
  (lambda (input)
    (list  (cons  (make-smug-parse-error :data (list :parse-error msg :at input))
                  (make-parse-state :remaining nil :errors (list :parse-error msg :at input)))
           )))

(defun .label (lbl p)
  (.let* ((res  p))
    (.identity (list lbl res))))

(defmacro .func (args &rest body)
  `(.is (lambda ,args ,@body)))

(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (defun pairs (xs)
    (loop :for (a b) :on xs :by #'cddr :while b 
          :collect (list a b))))

;;needed non-consuming and.
;;default .and consumes input.
(defun .& (p1 &rest ps)
  (lambda (input)
    (let* ((no  (gensym))
           (res (funcall (.or  p1 (.identity no)) input)))
      (if (eql (caar  res) no)
          nil
          (if ps
              (funcall (apply #'.& ps) input)
              res)))))

(defun .if (test-parser then-parser 
            &optional (else-parser (.fail)))
  (.or  (.& test-parser then-parser)
        else-parser))

(defun .when (test-parser then-parser)
  "we define .when in terms of .IF, but it's really just .AND again"
  (.if test-parser then-parser))

(defun .unless (test-parser then-parser)
  "defined in term of .when, even though it's just (.AND (.NOT ...))"
  (.when (.not test-parser) then-parser))

;;cat takes one or more label 
(defmacro .cat (&rest rawbinds)
  (let* ((binds  (pairs rawbinds))
         (ks     (mapcar #'first binds))
         (parses (mapcar (lambda (kv) (list  (gensym (symbol-name (first  kv))) (second kv))) binds)))
    `(.only  (.let* ,parses
               (.identity (mapcar #'list (list ,@ks) (list ,@(mapcar #'first parses))))
               ))))

(defmacro .alt (&rest rawbinds)
  (let* ((binds (pairs rawbinds)))
    `(.or ,@(mapcar (lambda (kv)
                      (list '.label (first kv) (second kv)))
                    binds))))

(defun .rest ()
  (lambda (input)
    (list
     (cons 
      (parse-state-remaining input)
      (make-parse-state)))))

(defun .only (parser)
  (.let* ((nxt parser)
          (more (.optional (.item))))
    (if more
        (.fail)
        (.identity nxt))))

;;it occurs to me we can use typeclasses as predicates...
(defmacro .tuple (&rest preds)
  (let ((syms (loop for x in preds collect (list (gensym) x))))
    `(.let* (,@syms)
      (.identity (list ,@(mapcar #'first syms))))))

(defun .symbol-args ()
  (.or (.list-of (.is #'symbolp))
       (.empty-list)
       (.error "Expected Symbol List")))

(export '(.alt
          .cat
          .tuple
          .func
          .item
          parse!
          run!
          show!
          .empty-list
          .list-of
          .error
          .label
          .&
          .if
          .when
          .unless
          .rest
          .only
          .symbol-args))
