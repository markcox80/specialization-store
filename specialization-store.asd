(defsystem "specialization-store"
  :author "Mark Cox"
  :description "The specialization store system provides a new kind of function, called a store function, whose behavior depends on the types of objects passed to the function."
  :depends-on ("specialization-store-features" "introspect-environment" "alexandria")
  :license "Simplified BSD License variant"
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "common")
                                     (:file "lambda-lists")
                                     (:file "dispatch")
                                     (:file "dispatch-fixed-arity")
                                     (:file "dispatch-variable-arity")
                                     (:file "protocols")
                                     (:file "standard-store"))))
  :in-order-to ((test-op (test-op "specialization-store-tests"))))
