(defpackage :clclojure.equivalence ;;might change this to clojure.lang at some point.
  (:use :common-lisp :common-utils :clclojure.protocols)
  (:export :IHashCode :-hashcode :IHasheq :-hasheq :IEquiv :-equiv :equiv))
(in-package :clclojure.equivalence)

;;copping some fundamental protocols for bootstrapping symbol/key/ns support.
;;putting them here allows for bootstrapping elsewhere, specifically for cowmap
;;and hashtable interop.
(defprotocol IHashcode
    (-hashcode (this)))
(defprotocol IHasheq
    (-hasheq (this)))
(defprotocol IEquiv
    (-equiv (o other)))
;;TODO look at optimizing this.
;;We are probably waaaaay slow.
;;guessing this is a Good Thing  
(defun equiv (x y)
  (if (and (numberp x) (numberp y))
      (common-lisp:= x y)
      (or (eq x y)
          (-equiv x y))))
;;lets us have interop with legacy hash tables.
;;on jvm this was accomplished through hashCode and
;;equals. we may not need to do that here.
(sb-ext:define-hash-table-test equiv -hasheq)
