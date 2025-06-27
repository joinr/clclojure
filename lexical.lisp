;;Defining lexically scoped, unified variables and
;;functions with keyword access.
(defpackage :clclojure.lexical
  (:use  :common-lisp  :clclojure.keywordfunc
   :common-utils)
  (:export :unified-let*)
  (:local-nicknames (:mbind :metabang-bind)))
(in-package :clclojure.lexical)

;;if the arg can be construed as a function,
;;the lexical symbol should be unified..

;; (defmacro unify-binding (var)
;;   `(cond ((functionp  ,var)          
;;           (setf (symbol-function (quote ,var))
;;                 ,var))
;;          ((keywordp   ,var)
;;           (if  (not (keyfn? ,var))
;;                (progn (pprint (format nil "adding keyword access for: ~a " k ))
;;                       (eval (key-accessor ,var)))))))

;;we need to use let and flet instead of this...

;; (defmacro unify-binding (var)
;;   `(cond ((functionp  ,var)          
;;           (setf (symbol-function (quote ,var))
;;                 ,var))
;;          ((keywordp   ,var)
;;           (if  (not (keyfn? ,var))
;;                (progn (pprint (format nil "adding keyword access for: ~a " ,var ))
;;                       ;;(eval (key-accessor ,var))
;;                       (setf (symbol-function (quote ,var))
;;                             (->keyaccess ,var)) 
;;                       )))))

;;a couple of notes on evaluation and symbol/function namespaces,
;;including lexical scope....
;;we have a few cases to cover...
;;if we want to cover every possible case and get a lisp1,
;;in the lexical case, we are relegated to using a combination
;;of let and flet on all the symbols
;; (let* ((g       (->keyaccess :a))
;;        (lookup  (->keyaccess :b)))
;;   (labels ((g      (arg)      (funcall (keyaccess-func g)  arg))
;;            (lookup (arg)      (funcall (keyaccess-func lookup)  arg)))
;;     ;;(mapcar f (list  keyfns keyfns))
;;     (pprint (list :obj lookup :fn (g keyfns)))))

;;this is an example of how we can play with lexical binds...
;;In the extreme case, we may not know what any types are,
;;which means they're functions or objects....
;; (defun some-fn (z)
;;   (let* ((g       (lambda (x) (+ x 5))) ;;an actual function object...
;;          (lookup  (->keyaccess :b))
;;          (z       (if (keywordp z)
;;                       (->keyaccess z)
;;                       z))) ;;keyword access function object...
;;     (labels (;;general implementation of fn
;;              (g      (&rest args)      (apply  g  args))
;;              ;;specific implementation for kw lookup..
;;              (lookup (arg)      (funcall (keyaccess-func lookup)  arg))
;;              (z      (&rest args)     (apply z args))
;;              )
;;       (pprint (list :obj g :fn (g 2) :keyaccess lookup
;;                     :z z :z-lookup (z keyfns)
;;                     (mapcar  (lambda (x) (list :type x (type-of x)))
;;                              (list  g  lookup z)))))))

;;the only things that we know...  are keywords, or fn forms bindings
;;are already in pairs...

;;Scrape the bindings to let*, and if we find keywords,
;;create an alist that associates the keyword to an
;;expression that defines a labels lexical function
;;for the keyword accessor.  We compute/construct
;;a keyaccessor at compile time, and though it's
;;funcallable, we lookup its associated function
;;for use (and efficiency).  We then provide
;;a simple function wrapper that invokes the keyword
;;fn (bear in mind, this is setfable).
(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (defun keyword-accessors (bindings)
    (let ((arg (gensym "lookup"))
          (xs  (common-utils:filter (lambda (lr) (keywordp (second lr))) bindings)))
      (when-not (null xs)
                (mapcar (lambda (lr)
                          (pprint lr)
                          (destructuring-bind (l r) lr
                            (let ((f (keyaccess-func  (->keyaccess r))))
                              (list r `(,l (,arg) (funcall (->keyaccess ,r) ,arg))))))
                        xs))))

  ;;we use a generic apply here...  collect all the args into a list and
  ;;apply.  In clojure, there's some cost to that.  Dunno what the
  ;;overhead is in CL.  Also, if we "know" anything about the function,
  ;;we may be able to do some analysis and compile a more efficient
  ;;binding form (i.e. known number of args in the lambda. or simple
  ;;funcall...

  ;;There's some question about how much we know about the parameters at
  ;;runtime (specifically for let bindings).  For certain classes of
  ;;lexical environments, we may be a-okay doing significant analysis of
  ;;what's involved in the let (case in point: if it's a lambda or a
  ;;known function we have meta on, we can derive types / args).  Thats
  ;;a future optimization...

  ;;Note: if we don't refer to the lexical vars (NOT fns) for the
  ;;keywords, we end up with a slew of style warnings, since they don't
  ;;appear to be used (they are used for the lexical keyaccessors
  ;;though).  To prevent this, we define a dummy function (never
  ;;invoked) that builds a list composed from the symbol-values.  For
  ;;now, it's convenient.  I may revisit this to see if we can detect if
  ;;the symbols aren't validly used...

  ;;we get compiler complaints with this if we don't...

  (defun functionize-bindings (bindings)
    (let* ((kwalist (keyword-accessors bindings))
           (vars    (mapcar (lambda (lr) (first (second lr))) kwalist))
           (dummy   (gensym "dummyfn")))
      (cons `(,dummy () (list :this-prevents-warnings-nothing-else
                              ,@vars))
            (mapcar (lambda (lr)
                      (destructuring-bind (l r) lr
                        (if (keywordp r)
                            (second  (assoc r kwalist))
                            `(,l (,'&rest ,'args) (apply ,l ,'args)))))
                    bindings)))))

;;so at the lexical level, we need to analyze the bindings.
;;determine if an item is a function (or an applicable object like
;;a keyword), and create matching labels for them...

;;this acts like let*, except it allows bindings that may be functions
;;or things that can act like functions -> keywords.  Everything else
;;should be covered by a funcallable object...  We unify the
;;symbol-value and symbol-function namespaces in the lexical context,
;;detecting the need to generate keyword accessors.

;;DEPRECATED - original implementation.
;; (defmacro unified-let* (bindings &rest body)
;;   `(let* (,@bindings)
;;      (labels (,@ (functionize-bindings bindings)
;;                  )
;;        ,@body)))

;;If we want to introduce lisp1 binding forms, we need to cover
;;(with-slots (...)  body)
;;(destructuring-bind (& locals) body)
;;then we can call this unified-bind or something.

;; (defmacro unified-with-slots (slots obj &rest body)
;;   `(with-slots ,slots ,obj
;;      (labels (,@ (functionize-slots bindings)
;;                  )
;;        ,@body)))

;; (defmacro unified-dbind (binding obj &rest body)
;;   `(destructuring-bind ,binding ,obj
;;      (labels (,@(functionize-slots binding)
;;                  )
;;        ,@body)))



;;a simple test function to tie everything together.
;; (defun test-my-scope ()
;;   (unified-let* ((hello :hello)  ;;we create (or lookup cached) keyaccess funcallable objects
;;                  (world :world)  ;;when we have literal keywords bound to symbols.
;;                  (k 2) 
;;                  (inc (lambda (x) (+ x 1)))
;;                  (add (lambda (x y) (+ x y)))
;;                  (tbl (unified-let* ((tbl (make-hash-table)))
;;                                     (setf  (gethash :hello tbl) "World")
;;                                     (setf  (gethash :world tbl) "Hello")
;;                                     (setf  (gethash :k  tbl)    k)
;;                                     tbl)))
;;                 (list (hello tbl)
;;                       (world tbl)
;;                       (add (inc 39) k)
;;                       ;;(:k tbl) ;;doesn't work without some extra macro magic...
;;                       (funcall (->keyaccess :k) tbl) ;;it will look like this behind the scenes.
;;                       )))

;;LEXICAL> (test-my-scope)
;;("World" "Hello" 42 2)  ;;works!


;;example of working with funcallable
;;classes.

;; (defclass fn-class ()
;;   ((data :initarg :data))
;;   (:metaclass sb-mop:funcallable-standard-class))

;; (defmethod initialize-instance :after ((obj fn-class) &rest initargs)
;;   (declare (ignore initargs))
;;   (sb-mop:set-funcallable-instance-function obj
;;     (lambda (&rest args)
;;       (apply (slot-value obj 'data) args))))

;; (defparameter *blah* (make-instance 'fn-class :data (lambda (x) (+ x 99))))

;;we can play games with macrolet here and scrape the results...
;;let's let it replace all of our calls in the function position
;;and turn them into funcalls.
;;This is another way to lisp1 stuff in the lexical environment.
;;We introduce some asspain though, since e.g. in let* we can
;;have later binds depend on earlier ones.  That means leveraging
;;earlier binds as functions.  We currently have that problem with
;;the legacy scheme though (e.g. unified-let* will mess up since we
;;don't unify a binding at a time and instead go in bulk.

;;e.g., this code currently fails: despite being valid:

;; (unified-let*
;;  ((f (lambda (x) (+ x 2)))
;;   (g (lambda (x) (f x)))
;;   (k (g 3)))
;;  (f k))

;;as it expands to
;; (LET* ((F (LAMBDA (X) (+ X 2))) (G (LAMBDA (X) (F X))) (K (G 3)))
;;   (LABELS ((#:|dummyfn1809| ()
;;              (LIST :THIS-PREVENTS-WARNINGS-NOTHING-ELSE))
;;            (F (&REST ARGS)
;;              (APPLY F ARGS))
;;            (G (&REST ARGS)
;;              (APPLY G ARGS))
;;            (K (&REST ARGS)
;;              (APPLY K ARGS)))
;;     (F H)))
;;and we have a failure to resolve (g 3) inside of K.

;;if we nest our unified bindings, we're okay though:

;; (unified-let*
;;  ((f (lambda (x) (+ x 2)))
;;   (g (lambda (x) (f x)))
;;   (k (g 3)))
;;  (f k))

(defmacro replace-funcalls (vars &rest body)
  (let* ((args (gensym "args"))
         (impls (mapcar (lambda (var)
                        `(,var (,'&rest ,args)
                               `(funcall ,',var ,@,args)))
                       vars)))
    `(macrolet (,@impls)
       ,@body)))

;;this is probably a more elegant approach going forward.
;;we might have collisions with other macrolets though....
;;like how does this work with with-slots and friends...
;; (defmacro unified-let*2 (bindings &rest body)
;;   (let ((vars (mapcar #'first bindings)))
;;     `(replace-funcalls ,vars
;;       (let* ,bindings
;;         ,@body))))

;;this works fine now.
;; (unified-let*2
;;  ((f (lambda (x) (+ x 2)))
;;   (g (lambda (x) (f x)))
;;   (k (g 3)))
;;  (f k))


;; (defstruct dummy (g))

;; (pprint
;;  (sb-cltl2:macroexpand-all
;;   '(unified-let*2
;;      ((f (lambda (x) (+ x 2)))
;;       (g (lambda (x) (f x)))
;;       (k (g 3))
;;       (the-object (make-dummy :g 99)))
;;     (with-slots (g) the-object
;;       (f k)))))

;;we can more simply introduce metabang this way.
;;we just need to scrape any LHS binding forms from
;;our let, flatten those, then feed them to
;;replace-funcalls...


;;this is probably a more elegant approach going forward.
;;we might have collisions with other macrolets though....
;;like how does this work with with-slots and friends...
;;I think we lose out on mutual recursion here maybe
;;since we aren't doing explicit labels functions.
;;do we care?
(defmacro unified-let* (bindings &rest body)
  (let ((vars (->> (mapcar #'first bindings)
                   (concatenate 'list) 
                   (flatten)
                   (remove-duplicates)
                   (filter (lambda (x) (and  (not (keywordp x))
                                             (not (seql x 'QUOTE))))))))
    `(replace-funcalls ,vars
        (mbind:bind ,bindings
          ,@body))))

;; (pprint (macroexpand-1 '(unified-let*3 (((x y) '(1 2))
;;                                         ((f g) (list (lambda (x) (+ x 2)) (lambda (y) (* y 4))))
;;                                         (h (lambda (n) (f (g n))))
;;                                         (k 10)
;;                                         (the-object (make-dummy :g 10))
;;                                         ((:slots g) the-object))
;;                          (list x y (+  (h k) g)))))
;; ;;(1 2 52)

;;defines a lambda form with a unified value/function namespace, e.g. lisp1
;;where the args can be used in function position without problems.
(defmacro unified-lambda (args &rest body)
  (let ((vars (common-utils:lambda-list->args '(x y z &optional b &rest xs &key d e f) t)))
    `(lambda ,args
       (replace-funcalls ,vars
                         ,@body))))

;;one limitation of the replace-funcalls approach is that we don't affect
;;passing values to other things that may expect functions.
;;like if we want to interop with mapcar, the function argument isn't defined.
;;where with labels it probably is.....hmmm.
;;lambdas are fine to pass around.  unclear about other cases...so far, no
;;big deal. we can always revert to the labels approach if necessary.
