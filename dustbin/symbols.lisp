;;beginnings of custom symbol tables...
;;symbols have different behavior in
;;CL and CLJ.  We need to 
(defpackage :clclojure.symbols 
  (:use  :common-lisp :clclojure.base)
  (:shadowing-import-from :clclojure.base
   :deftype :let))
(in-package :clclojure.symbols)

(defstruct cljsymbol namespace name)
(defun ->symbol (name &optional namespace)
  (make-cljsymbol :namespace namespace :name name))

;;where do symbols live in cljs?
;;I think there's a local var that defines namespaces and symbols.
;;In clj jvm, there's a map...



;;good reference here:
;;http://blogish.nomistech.com/clojure/clojure-symbols-vs-lisp-symbols/

;;this leads, naturally, to vars as well..
;;namespaces are just mappings of symbols to vars.

