;;note: for quicklisp users...
;;compile and load this file,
;;or from emacs/SLIM (C-c C-k)
;;then quicklisp can load it for us
;;easy...
;;(ql:quickload :clj)
(asdf:defsystem :clclj
  :depends-on (:named-readtables :cl-package-locks :cl-murmurhash
               :clj-con :cl-ppcre :clj-re) ;copied from example. debate using :cl-hamt
  :components ((:file "common-utils")
               (:file "walk"        :depends-on ("common-utils"))
               (:file "sequences"   :depends-on  ("common-utils"))
               ;(:file "reader" :depends-on ("pvector" "cowmap" "sequences"))
               ;(:file "eval"   :depends-on ("common-utils" "walk" "reader"))
               ;(:file "literals"  :depends-on ("eval" "pvector" "cowmap"))
               (:file "keywordfunc")
               (:file "lexical"
                :depends-on ("keywordfunc"))
               (:file "pvector")
               (:file "cowmap")
               ;(:file "lexical"  :depends-on ("keywordfunc"))
               (:file "protocols"  :depends-on ("common-utils" "pvector" "cowmap"))
	       (:file "clj"        :depends-on ("common-utils" "protocols" "pvector" "cowmap" "lexical"))
               ))

(asdf:defsystem :tools.reader
  :depends-on (:clclj :named-readtables :cl-package-locks :cl-murmurhash
               :clj-con :cl-ppcre :clj-re) ;copied from example. debate using :cl-hamt
  :components ((:file "reader-utils")
               (:file "reader-types"   :depends-on ("reader-utils"))
               ;(:file "reader-errors"  :depends-on ("reader-types" "reader-inspect"))
               ;(:file "reader-commons" :depends-on ("reader-utils" "reader-errors" "reader-types"))
               ))
