;;playing with parser combinators
;;for lambda arg parsing.
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

;;we have a grammar for function bodies.
;; destructuring-bind-form :: 
;; dbind      :: destructuring-bind-form
;; arg        :: symbol | dbind
;; body       :: atom | list
;; normal     :: ((list* arg) body)
;; variadic   :: normal+

;;a destructuring arg is either
;;an atom, a finite list of atoms,
;;or a list of destructuring args.
;;.map is too permissive.  if want to parse
;;only successes
(defun .dbind ()
  (.let*  ((nxt (.item)))
    (if (symbolp nxt)
        (.identity nxt)
        (if (listp nxt)
            (let ((res (parse!  (.only  (.list-of (.dbind))) (list nxt))))
              (if res
                  (.identity  res)
                  (.fail)))
            (.fail)))))

;;we can probably start defining labels for our parse
;;results like spec does.
(defun .arg ()
  (.or (.is #'atom)
       (.is #'listp)))

(defun .body ()
  (.or (.is #'atom)
       (.is #'listp)))

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

(defun .args ()
  (.let* ((args (.or (.empty-list)
                     (.list-of (.arg)))))
    (.identity (list :args args))))

;;would be nice to have this on board.
;; lambdalist :: (list* arg rest? options? keys?)

;;can't have symbol functions in args.
(defun .normal ()
  (.let* ((arg-body (.item)))
    (if (and (listp arg-body)
             (= (length arg-body) 2))
        (let ((args (first  arg-body))
              (body (second arg-body)))
          (if (and 
               (parse! (.args) (list  args))
               (parse! (.body) body))
              (.identity (list :normal args :body body))
              (.fail)))
        (.fail))))

;;one or more normals,
;;where the arg lengths are distinct.

(defun .variadic ()
  (.let* ((defs  (.only  (.map 'list (.normal)))))
    (let* ((args (mapcar #'second defs))
           (counts (remove-duplicates  (mapcar #'length args))))
      (if (= (length  counts) (length args))
          (.identity (list :variadic defs))
          (.fail)))))

(defun .only (parser)
  (.let* ((nxt parser)
          (more (.optional (.item))))
    (if more
        (.fail)
        (.identity nxt))))

(defun .fn ()
  (.or   (.only  (.normal))
         (.variadic)))

(defun show! (parser input)
  (multiple-value-bind (res &rest others) (parse! parser input)
    res))

;; '(fn (x y) (+ x y))
;; '((x y) (+ x y))
;; '(fn ((x y) z) (+ x y z))
;; '(((x y) z) (+ x y z))

(defparameter tst
  '(defn interleave
      (() '())
    ((c1) (lazy-seq c1))
    ((c1 c2)
     (lazy-seq
      (let (s1 (seq c1) s2 (seq c2))
        (when (and s1 s2)
          (cons (first s1) (cons (first s2)
                                 (interleave (rest s1) (rest s2))))))))
    ((c1 c2 &rest colls)
     (lazy-seq
      (let (ss (map seq (conj colls c2 c1)))
        (when (every? identity ss)
          (concat (map first ss) (apply interleave (seq->list  (map rest ss))))))))))

;;we'll break down the error checking thusly:
;;just get the shape of the args correct.
;;then validate the specs.
;;it's too rough to do error handling with parser combinators
;;but it's trivial to validate staged parses...

(defstruct smug-parse-error data)

;;goofy
(defun .error (&optional msg)
  (lambda (input)
    (list  (cons  (make-smug-parse-error :data (list :parse-error msg :at input))
                  (make-parse-state :remaining nil :errors (list :parse-error msg :at input)))
           )))

(defun .symbol-args ()
  (.or (.list-of (.is #'symbolp))
       (.empty-list)
       (.error "Expected Symbol List")))

(defun unpack (spec)
  (list (second spec) (last spec)))
;; (defun .clj-fn ()
;;   (.let* ((spec (.fn)))
;;     (case (first spec)
;;       :normal
;;       (.and  (.symbol-args (second spec))
;;        (.identity (unpack spec)))
;;       :variadic
;;       (let ((args (mapcar #'second (second spec))))
;;         (if  (parse! (.list-of (.symbol-args)) (list args))
;;             (mapcar #'unpack (second spec))))
;;       ())))
