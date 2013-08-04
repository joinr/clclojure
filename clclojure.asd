(asdf:defsystem :clclojure
;  :depends-on (#:cl-ppcre)
  :components ((:file "common-utils")
	       (:file "pvector")
	       (:file "protocols"
		      :depends-on ("common-utils"))
	       (:file "bootstrap"
		      :depends-on ("common-utils"
				   "clojure.pvector"))))