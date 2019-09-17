;;note: for quicklisp users...
;;compile and load this file,
;;or from emacs/SLIM (C-c C-k)
;;then quicklisp can load it for us
;;easy...
;;(ql:quickload :clclojure)
(asdf:defsystem :clclojure
  :depends-on (:named-readtables :cl-package-locks) ;copied from example.
  :components ((:file "common-utils")
               (:file "eval")
               (:file "literals"
                :depends-on ("eval" "pvector" "cowmap"))
               (:file "reader"
                :depends-on ("pvector" "cowmap"))
               (:file "keywordfunc")
               (:file "pvector")
               (:file "cowmap")
               (:file "lexical"
                      :depends-on ("keywordfunc"))
               (:file "protocols"
                :depends-on ("common-utils" "reader" "pvector" "cowmap"))
	       (:file "bootstrap"
                :depends-on ("literals"
                             "common-utils"
                             "lexical"
                             "keywordfunc"
                             "protocols"
                             "pvector"
                             "cowmap")))
  )
