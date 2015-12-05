(defsystem "specialization-store-features"
  :author "Mark Cox"
  :description "A system which identifies features of the implementation needed for the specialization-store system."
  :depends-on ("introspect-environment")
  :serial t
  :components ((:module "features"
                        :serial t
                        :components ((:file "packages")
                                     (:file "features")))))
