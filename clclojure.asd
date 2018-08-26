;;note: for quicklisp users...
;;compile and load this file,
;;or from emacs/SLIM (C-c C-k)
;;then quicklisp can load it for us
;;easy...
;;(ql:quickload :clclojure)
(asdf:defsystem :clclojure
  :depends-on (:named-readtables) ;copied from example.
  :components ((:file "common-utils")
               (:file "reader"
                :depends-on ("pvector"))
               (:file "keywordfunc")
               (:file "pvector")
               (:file "lexical"
                      :depends-on ("keywordfunc"))
               (:file "protocols"
                :depends-on ("common-utils" "reader" "pvector"))
	       (:file "bootstrap"
                :depends-on ("common-utils"
                             "lexical"
                             "keywordfunc"
                             "protocols"
                             "pvector")))
  )
