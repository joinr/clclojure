(asdf:defsystem :clclojure
  :depends-on (:named-readtables) ;copied from example.
  :components ((:file "common-utils")
               (:file "reader"
                ;:depends-on (:named-readtables)
                )
	       (:file "pvector"
                :depends-on ("reader"))
	       (:file "protocols"
                :depends-on ("common-utils"))
	       (:file "bootstrap"
                :depends-on ("common-utils"
                             "protocols"
                             "pvector"))))
