;;Defining lexically scoped, unified variables and
;;functions with keyword access.
(defpackage :clclojure.lexical
  (:use  :common-lisp  :clclojure.keywordfunc
   :common-utils)
  (:export :unified-let*))
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
(defmacro unified-let* (bindings &rest body)
  `(let* (,@bindings)
     (labels (,@ (functionize-bindings bindings)
                 )
       ,@body)))

;;a simple test function to tie everything together.
(defun test-my-scope ()
  (unified-let* ((hello :hello)  ;;we create (or lookup cached) keyaccess funcallable objects
                 (world :world)  ;;when we have literal keywords bound to symbols.
                 (k 2) 
                 (inc (lambda (x) (+ x 1)))
                 (add (lambda (x y) (+ x y)))
                 (tbl (unified-let* ((tbl (make-hash-table)))
                                    (setf  (gethash :hello tbl) "World")
                                    (setf  (gethash :world tbl) "Hello")
                                    (setf  (gethash :k  tbl)    k)
                                    tbl)))
                (list (hello tbl)
                      (world tbl)
                      (add (inc 39) k)
                      ;;(:k tbl) ;;doesn't work without some extra macro magic...
                      (funcall (->keyaccess :k) tbl) ;;it will look like this behind the scenes.
                      )))

;;LEXICAL> (test-my-scope)
;;("World" "Hello" 42 2)  ;;works!
