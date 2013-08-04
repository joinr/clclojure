;;This is the basis for bootstrapping clojure onto common lisp.
;;I figure if I can define the primitive forms that clojure requires, 
;;there's already a ton of clojure written in clojure.  The clojurescript
;;runtime actually has a significant portion of clojure defined via protocols,
;;which given limited forms, provides a pretty slick way to bootstrap an 
;;implementation.
;;A couple of big hurdles: 
;;1) Lisp1 vs Lisp2.  I'll hack the evaluator for this.
;;2) Persistent structures.  Already built Pvector and 1/2 done with Pmap.
;;3) Protocols.  Already implemented as generic functions. 
;;4) Multimethds.  Need to find a way to implement multiple dispatch.
;;5) Reader.  CL macros use , and ,@ in place of ~ and ~@ in Clojure.
;;            We'll need to either cook the common lisp reader, or 
;;            build a separate clojure reader that will perform the
;;            appropriate replacements.
;;            @ is a literal for #'deref in clojure
;;            , is whitespace in clojure.
;;            [] denote vectors -> already have a reader macro in pvector.lisp
;;            {} denote maps    -> already have a reader macro in pmap.lisp
;;           #{} denote sets
;;6) Destructuring.  This may be a bit tricky, although there are a limited number of 
;;                   clojure forms.  Since we have reader
;;7)                
(load "common-utils.lisp") ;useful stuff like comment, keyword creation, and more.
(load "protocols.lisp")
(load "pvector.lisp") ;imports reader macros for vector literals i.e. [1 2 3] 
;(load "pmap.lisp")  ;once pmap is finished, it'll be really useful for  
;(load "seq.lisp") ;might be nice to go ahead and clone the seq library to make this simpler.

(defpackage :clclojure.base
  (:use :pvector :common-utils)
  (:export :comment))

(in-package clclojure.base)

;;one really useful macro is the comment form:
(defmacro comment (&rest xs ))

;;Some basic stuff to facilitate clojure forms.

;;I think we want to use persistent maps for meta data, as clojure does.
;;I want to get the stubs in place, and am using property lists with a 'meta 
;;entry pointing at an assoc list for now.
(defmacro meta (symb) `(get (quote ,symb) 'meta))
(defmacro with-meta (symb m) `(setf (get (quote ,symb) 'meta) ,m))

;;Clojure is a lisp-1, so we need to ensure that everything, even 
;;functions, gets bound into the a single namespace. 

;;another way to do this is to have clojure-specific symbols be actual 
;;clos objects, which have meta data fields automatically.  Then we 
;;lose out on all the built in goodies from common lisp though.

;;Experimental.  Not sure of how to approach this guy.
(defmacro def (var &rest init-form)
  `(progn (defparameter ,var ,@init-form)
          (with-meta ,var '((symbol . t) (doc . "none")))
	  (quote ,var)))

;;We have a lisp1, sorta! 
;;I need to add in some more evaluation semantics, but this might be the 
;;way to go.  For now, it allows us to have clojure semantics for functions 
;;and macros.  There's still some delegation to the common lisp evaluator - which is 
;;not a bad thing at all!
(defun eval-clojure (expr)
  (cond ((atom expr)  (eval expr))
	((listp expr) (let* ((f (car expr)) ;must be a function...
			    (res (eval f))) ;resolve the function			
			(if (functionp res) ;apply the function
			    (apply res (mapcar #'eval-clojure (rest expr)))
			    (eval expr))))
	))	    

(comment  ;testing
  (def the-val 2)
  (def symbol? #'symbolp)
  (def add-two (lambda (x) (+ 2 x)))
  (eval-clojure '(symbol? the-val)) ;=> nil
  (eval-clojure '(add-two the-val)) ;=> 4
)

(defmacro doc (v) `(pprint (rest (assoc 'DOC (meta ,v)))))   
(comment 

(defmacro fn  (& sigs) 
  (let* ((name (if (symbol? (first sigs)) (first sigs) nil)
         sigs (if name (next sigs) sigs)
         sigs (if (vector? (first sigs)) 
                 (list sigs) 
                 (if (seq? (first sigs))
                   sigs
                   ;; Assume single arity syntax
                   (throw (IllegalArgumentException. 
                            (if (seq sigs)
                              (str "Parameter declaration " 
                                   (first sigs)
                                   " should be a vector")
                              (str "Parameter declaration missing"))))))
          psig (fn* [sig]
                 ;; Ensure correct type before destructuring sig
                 (when (not (seq? sig))
                   (throw (IllegalArgumentException.
                            (str "Invalid signature " sig
                                 " should be a list"))))
                 (let [[params & body] sig
                       _ (when (not (vector? params))
                           (throw (IllegalArgumentException. 
                                    (if (seq? (first sigs))
                                      (str "Parameter declaration " params
                                           " should be a vector")
                                      (str "Invalid signature " sig
                                           " should be a list")))))
                       conds (when (and (next body) (map? (first body))) 
                                           (first body))
                       body (if conds (next body) body)
                       conds (or conds (meta params))
                       pre (:pre conds)
                       post (:post conds)                       
                       body (if post
                              `((let [~'% ~(if (< 1 (count body)) 
                                            `(do ~@body) 
                                            (first body))]
                                 ~@(map (fn* [c] `(assert ~c)) post)
                                 ~'%))
                              body)
                       body (if pre
                              (concat (map (fn* [c] `(assert ~c)) pre) 
                                      body)
                              body)]
                   (maybe-destructured params body)))
          new-sigs (map psig sigs)]
      (with-meta
        (if name
          (list* 'fn* name new-sigs)
          (cons 'fn* new-sigs))
        (meta &form))))
)
