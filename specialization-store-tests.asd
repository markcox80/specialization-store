(defsystem "specialization-store-tests"
  :author "Mark Cox"
  :description "Tests for the SPECIALIZATION-STORE system."
  :depends-on ("specialization-store" "fiveam")
  :serial t
  :components ((:module "tests"
			:serial t
			:components ((:file "packages")
                                     (:file "common")
				     (:file "lambda-lists")
                                     (:file "standard-store")
                                     (:file "syntax-layer")
                                     (:file "asdf")))))
