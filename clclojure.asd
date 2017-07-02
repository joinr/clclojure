(asdf:defsystem :clclojure
;  :depends-on (#:cl-ppcre) ;copied from example.
  :components ((:file "common-utils")
               (:file "reader")
	       (:file "pvector"
                :depends-on ("reader"))
	       (:file "protocols"
                :depends-on ("common-utils"))
	       (:file "bootstrap"
                :depends-on ("common-utils"
                             "protocols"
                             "pvector"))))
