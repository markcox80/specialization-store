(in-package "SPECIALIZATION-STORE.TESTS")

(defmethod asdf:perform ((op asdf:test-op) (c (eql (asdf:find-system "specialization-store-tests"))))
  (5am:run! 'all-tests)

  (format t "~&;;;; Running syntax layer tests.~%")
  (process-syntax-layer-tests)
  (format t "~&;;;; Done.~%"))
