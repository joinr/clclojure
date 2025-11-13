;;wherein we define some base classes and structs for our clojure
;;objects to inherit from.  This is for implementation help e.g. with
;;our legacy data structure implementations, e.g.  cowmap and
;;persistent vector structs etc.  We can treat these as superclasses
;;for our implementation level stuff and e.g. pack along metadata and
;;hasheq slots with our core datastructures.

(defpackage :clj-objects ;;might change this to clojure.lang at some point.
  (:use :common-lisp :common-utils)
  (:export :make-cljstruct))
(in-package :clj-objects)

(defstruct cljstruct (meta nil) (hash -1))
;;do we need these? hmm maybe.
(defclass  cljclass ()
  ((meta
    :initarg :meta
    :initform nil
    :accessor meta)
   (hash
    :initarg :hash
    :initform -1
    :accessor hash)))
