(defpackage :clclojure.eval
  (:use :common-lisp        
        :cl-package-locks)
  (:export :custom-eval :let-expr :let-expr? :noisy-expand :custom-eval-bindings
           :custom-eval? :enable-custom-eval :disable-custom-eval
   :simple-eval-in-lexenv))

(in-package clclojure.eval)

(defgeneric let-expr     (obj))
(defmethod  let-expr     (obj) obj)

(defgeneric custom-eval  (obj))
;;We perform the same thing existing eval does for unknown
;;datums.  Just return the data as-is.  This is effectively
;;what sbcl does by default.
(defmethod custom-eval (obj) obj)

;;another option is to use find-method...
;;(find-method #'custom-eval '() '(t) nil)
(defvar +original-eval+ (symbol-function  'SB-IMPL::simple-eval-in-lexenv))


(defun custom-eval? (obj)
  (find-method #'custom-eval '() `(,(class-of obj)) nil))

(defun let-expr? (obj)
  (find-method #'let-expr '() `(,(class-of obj)) nil))

(defparameter *noisy-custom* nil)
(defparameter *noisy-depth*  0)

(defun noisy-log (msg)
  (when *noisy-custom*
    (pprint (list :noisy-log msg))))

(defmacro noisy-expand (expr)
  (let ((*noisy-custom* t))
    (pprint  (custom-eval-bindings expr nil))))

(defmacro bump-depth (&rest body)
  `(let ((,'clclojure.eval::*noisy-depth* (1+ ,'clclojure.eval::*noisy-depth*)))
     ,@body))

(defun symbol-key (s)
  (if (and (symbolp s)
           (string= (string-upcase (package-name  (symbol-package s)))
                    "CLCLOJURE.BASE")
           (string= (string-upcase (symbol-name s))
                    "LET"))
      :clj-let
      s))

;;we need to make sure this is recursive, so that the body is walked,
;;as are the arg bindings.
(defun custom-eval-bindings (expr lexenv)
  (when (> *noisy-depth* 50)
    (error "Possibly infinite recursion: ~S " expr))
  (when *noisy-custom* (pprint `(:evaluating ,expr)))
  (if (listp expr)
      (case (symbol-key  (first expr))
        (:clj-let
         (let ((nexpr (macroexpand expr)))
           (noisy-log :clj-let)
           (bump-depth
            (custom-eval-bindings nexpr lexenv))))
        ((let let*)
         (destructuring-bind (name args &rest body) expr
           ;;this is wrong for labels and flet
           (let* ((new-args 
                    (mapcar (lambda (kv)
                              (let ((lhs (first  kv))
                                    (rhs (second kv)))
                                (if (let-expr? rhs)
                                    `(,lhs ,(clclojure.eval:let-expr rhs))
                                    `(,lhs ,(macroexpand rhs))))) args))
                  (new-body (mapcar (lambda (v)
                                      (cond ((let-expr? v)
                                             (clclojure.eval:let-expr v))
                                            ((listp v)
                                             (bump-depth  (custom-eval-bindings v lexenv)))
                                            (t v))) body)))
             (noisy-log :let-let*-labels-flet-macrolet)
             `(,name ,new-args ,@new-body))))
        ((labels flet macrolet)
         (destructuring-bind (name args &rest body) expr
           ;;this is wrong for labels and flet
           (let* ((new-body (mapcar (lambda (v)
                                      (cond ((let-expr? v)
                                             (clclojure.eval:let-expr v))
                                            ((listp v)
                                             (bump-depth
                                              (custom-eval-bindings v lexenv)))
                                            (t v))) body)))
             (noisy-log :labels-flet-macrolet)
             `(,name ,args ,@new-body))))
        (otherwise
         (if (listp (cdr expr))             
             (progn (noisy-log :list-expr)                    
                    (mapcar (lambda (v)                              
                              (cond ((let-expr? v)
                                     (clclojure.eval:let-expr v))
                                    ((listp v)
                                     (bump-depth
                                      (custom-eval-bindings v lexenv)))
                                    (t v))) expr))
             (destructuring-bind (l . r) expr
               (progn (noisy-log :cons-expr)
                      (cons (cond ((let-expr? l)
                                   (clclojure.eval:let-expr l))
                                  ((listp l)
                                   (bump-depth
                                    (custom-eval-bindings l lexenv)))
                                  (t l))
                            (cond ((let-expr? r)
                                   (clclojure.eval:let-expr r))
                                  ((listp r)
                                   (bump-depth
                                    (custom-eval-bindings r lexenv)))
                                  (t r))
                            ))))))
      (if (let-expr? expr)
          (progn (noisy-log :custom-let-expr)
                 (let-expr expr))
          (progn (noisy-log :pass-through-expr)
                 expr))))

;;we have a minor problem with let/let* in that they don't
;;participate in the custom-eval hack for data literals.
;;Ergo, if we have a vector literal, like [x], we get
;;the legacy eval semantics that just passes the the thing through
;;unevaluated.

;;Tracing out the calls in sbcl/src/code/eval.lisp

;; (sb-impl::%simple-eval
;;  '(let* ((x 2)
;;          (y [x])) y)
;;  (sb-impl::make-null-lexenv))
;; yields:
;; [x]

;;So we either need to walk the code at macro expansion
;;time and unpack all the literals...
;;Or (better), hook into simple-eval
;;and allow custom eval semantics,
;;or (maybe better), hook into
;;sb-impl:%simple-eval and
;;enable custom evaluation semantics...

;;We "could" code walk this stuff
;;in our clclojure let macro too,
;;and make sure our literals expand
;;to function calls...

;;The simplest solution is just to inject #'custom-eval
;;calls into the pipeline so that %simple-eval
;;picks it up when it goes to build the expression
;;sb-impl:%simple-eval coerces the expression into
;;a lambda prior to sending it off for compilation.
;;We can leverage this to walk the bindings for
;;let, let* and inject rhs bindings that
;;have custom-eval applied if they are
;;custom-eval?
;;This should cover like 90% of our cases
;;without interfering with legacy
;;common lisp stuff.

;;This is identical to the default sbcl eval..
;;with the exception of the hook to our custom method.

(unlock-package :sb-impl)
(in-package :sb-impl)
    
(defun custom-eval-in-lexenv (original-exp lexenv)
  (declare (optimize (safety 1)))
  ;; (aver (lexenv-simple-p lexenv))
  (incf *eval-calls*)
  (sb-c:with-compiler-error-resignalling
    (let ((exp (macroexpand original-exp lexenv)))
      (handler-bind ((eval-error
                       (lambda (condition)
                         (error 'interpreted-program-error
                                :condition (encapsulated-condition condition)
                                :form exp))))
        (typecase exp
          (symbol
           (ecase (info :variable :kind exp)
             ((:special :global :constant :unknown)
              (symbol-value exp))
             ;; FIXME: This special case here is a symptom of non-ANSI
             ;; weirdness in SBCL's ALIEN implementation, which could
             ;; cause problems for e.g. code walkers. It'd probably be
             ;; good to ANSIfy it by making alien variable accessors
             ;; into ordinary forms, e.g. (SB-UNIX:ENV) and (SETF
             ;; SB-UNIX:ENV), instead of magical symbols, e.g. plain
             ;; SB-UNIX:ENV. Then if the old magical-symbol syntax is to
             ;; be retained for compatibility, it can be implemented
             ;; with DEFINE-SYMBOL-MACRO, keeping the code walkers
             ;; happy.
             (:alien
              (sb-alien-internals:alien-value exp))))
          (list
           (let ((name (first exp))
                 (n-args (1- (length exp))))
             (case name
               ((function)
                (unless (= n-args 1)
                  (error "wrong number of args to FUNCTION:~% ~S" exp))
                (let ((name (second exp)))
                  (if (and (legal-fun-name-p name)
                           (not (consp (let ((sb-c:*lexenv* lexenv))
                                         (sb-c:lexenv-find name funs)))))
                      (%coerce-name-to-fun name)
                      ;; FIXME: This is a bit wasteful: it would be nice to call
                      ;; COMPILE-IN-LEXENV with the lambda-form directly, but
                      ;; getting consistent source context and muffling compiler notes
                      ;; is easier this way.
                      (%simple-eval original-exp lexenv))))
               ((quote)
                (unless (= n-args 1)
                  (error "wrong number of args to QUOTE:~% ~S" exp))
                (second exp))
               (setq
                (unless (evenp n-args)
                  (error "odd number of args to SETQ:~% ~S" exp))
                (unless (zerop n-args)
                  (do ((name (cdr exp) (cddr name)))
                      ((null name)
                       (do ((args (cdr exp) (cddr args)))
                           ((null (cddr args))
                            ;; We duplicate the call to SET so that the
                            ;; correct value gets returned.
                            (set (first args)
                                 (simple-eval-in-lexenv (second args) lexenv)))
                         (set (first args)
                              (simple-eval-in-lexenv (second args) lexenv))))
                    (let ((symbol (first name)))
                      (case (info :variable :kind symbol)
                        (:special)
                        (t (return (%simple-eval original-exp lexenv))))
                      (unless (type= (info :variable :type symbol)
                                     *universal-type*)
                        ;; let the compiler deal with type checking
                        (return (%simple-eval original-exp lexenv)))))))
               ((progn)
                (simple-eval-progn-body (rest exp) lexenv))
               ((eval-when)
                ;; FIXME: DESTRUCTURING-BIND returns ARG-COUNT-ERROR
                ;; instead of PROGRAM-ERROR when there's something wrong
                ;; with the syntax here (e.g. missing SITUATIONS). This
                ;; could be fixed by hand-crafting clauses to catch and
                ;; report each possibility, but it would probably be
                ;; cleaner to write a new macro
                ;; DESTRUCTURING-BIND-PROGRAM-SYNTAX which does
                ;; DESTRUCTURING-BIND and promotes any mismatch to
                ;; PROGRAM-ERROR, then to use it here and in (probably
                ;; dozens of) other places where the same problem
                ;; arises.
                (destructuring-bind (eval-when situations &rest body) exp
                  (declare (ignore eval-when))
                  (multiple-value-bind (ct lt e)
                      (sb-c:parse-eval-when-situations situations)
                    ;; CLHS 3.8 - Special Operator EVAL-WHEN: The use of
                    ;; the situation :EXECUTE (or EVAL) controls whether
                    ;; evaluation occurs for other EVAL-WHEN forms; that
                    ;; is, those that are not top level forms, or those
                    ;; in code processed by EVAL or COMPILE. If the
                    ;; :EXECUTE situation is specified in such a form,
                    ;; then the body forms are processed as an implicit
                    ;; PROGN; otherwise, the EVAL-WHEN form returns NIL.
                    (declare (ignore ct lt))
                    (when e
                      (simple-eval-progn-body body lexenv)))))
               ((locally)
                (simple-eval-locally (rest exp) lexenv))
               ((macrolet)
                (destructuring-bind (definitions &rest body) (rest exp)
                  (let ((sb-c:*lexenv* lexenv))
                    (sb-c::funcall-in-macrolet-lexenv
                     definitions
                     (lambda (&optional funs)
                       (simple-eval-locally body sb-c:*lexenv*
                                            :funs funs))
                     :eval))))
               ((symbol-macrolet)
                (destructuring-bind (definitions &rest body) (rest exp)
                  (let ((sb-c:*lexenv* lexenv))
                    (sb-c::funcall-in-symbol-macrolet-lexenv
                     definitions
                     (lambda (&optional vars)
                       (simple-eval-locally body sb-c:*lexenv*
                                            :vars vars))
                     :eval))))
               ((if)
                (destructuring-bind (test then &optional else) (rest exp)
                  (eval-in-lexenv (if (eval-in-lexenv test lexenv)
                                      then
                                      else)
                                  lexenv)))
               ((let let*)
                ;;minor hack to enable custom eval semantics
                ;;We pre-process the expression to ensure that
                ;;calls to custom-eval are inserted for types with custom-eval...
                                        ;(%simple-eval exp lexenv)
                (%simple-eval (clclojure.eval:custom-eval-bindings exp lexenv) lexenv)                
                )
               (t
                (if (and (symbolp name)
                         (eq (info :function :kind name) :function))
                    (collect ((args))
                      (dolist (arg (rest exp))
                        (args (eval-in-lexenv arg lexenv)))
                      (apply (symbol-function name) (args)))
                    (%simple-eval exp lexenv))))))
          (t
           ;;Unlike the default SBCL eval, we inject our custom-eval here.
           ;;This allows types to define custom evaluation semantics, e.g.
           ;;for data literals, otherwise, it behaves exactly like original
           ;;eval and returns the type.
           (clclojure.eval:custom-eval exp)))))))       ; something dangerous

(in-package :clclojure.eval)
(lock-package :sb-impl)

(defun enable-custom-eval ()
  (with-packages-unlocked (:sb-impl :sb-int)
    (setf (symbol-function  'SB-IMPL::simple-eval-in-lexenv)
          (symbol-function  'SB-IMPL::custom-eval-in-lexenv)))       ; something dangerous
)

(defun disable-custom-eval ()
  (with-packages-unlocked (:sb-impl :sb-int)
    (setf (symbol-function  'SB-IMPL::simple-eval-in-lexenv)
           +original-eval+)       ; something dangerous
    t))
