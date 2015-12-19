(defsystem "specialization-store"
  :author "Mark Cox"
  :description "The specialization store system provides a new kind of
  function whose behavior depends on the types of objects passed to
  the function. The dispatching process required at run-time can be
  avoided provided that sufficient information is available at compile
  time."
  :depends-on ("specialization-store-features" "introspect-environment")
  :license "Simplified BSD License variant"
  :serial t
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "common")
				     (:file "lambda-lists")
				     (:file "dispatch")
				     (:file "protocols")
				     (:file "standard-store"))))
  :in-order-to ((test-op (test-op "specialization-store-tests"))))
