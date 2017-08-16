;; Not the best check. Some would say that the wine has increased my
;; apathy.
(unless (eql *package* (find-package "SPECIALIZATION-STORE.TESTS.SYNTAX-LAYER"))
  (error "This file can only be processed using SPECIALIZATION-STORE.TESTS::PROCESS-SYNTAX-LAYER-TESTS"))

(glue-layer-test ensure-store/initial
  (test ensure-store
    (is-false (fboundp 'my-function))
    (is-false (find-store 'my-function nil))
    (is-false (compiler-macro-function 'my-function))

    (ensure-store 'my-function '(a))

    (is-true (typep (find-store 'my-function) 'standard-store))
    (is-true (fboundp 'my-function))
    (is-true (compiler-macro-function 'my-function))

    (signals inapplicable-arguments-error
      (funcall (fdefinition 'my-function) 1))

    (make-store-unbound 'my-function)

    (is-false (find-store 'my-function nil))
    (is-false (fboundp 'my-function))
    (is-false (compiler-macro-function 'my-function))))

(glue-layer-test ensure-store/reinitialised
  (test ensure-store
    (ensure-store 'my-function '(&optional (a 1)))
    (ensure-specialization 'my-function '((a real)) 'real #'1+)
    (is (= 2 (funcall 'my-function)))
    (ensure-store 'my-function '(&optional (a 2)))
    (is (= 3 (funcall 'my-function)))))

(glue-layer-test find-store
  (test find-store
    (signals error (find-store 'my-function))
    (is-false (find-store 'my-function nil))

    (ensure-store 'my-function '(a))

    (is-true (find-store 'my-function))
    (is-true (find-store 'my-function nil))

    (make-store-unbound 'my-function)

    (signals error (find-store 'my-function))
    (is-false (find-store 'my-function nil))))

(glue-layer-test ensure-specialization
  (test ensure-specialization
    (ensure-store 'my-function '(a))
    (ensure-specialization 'my-function '((a integer)) 'integer #'1+)

    (flet ((invoke (&rest args)
             (apply (fdefinition 'my-function) args)))
      (signals inapplicable-arguments-error (invoke "hey"))
      (is (= 2 (invoke 1))))))

(glue-layer-test ensure-specialization/name
  (test ensure-specialization
    (ensure-store 'my-function '(a))
    (ensure-specialization 'my-function '((a integer)) 'integer #'1+ :name 'my-function/integer)

    (is-true (fboundp 'my-function/integer))
    (is (equal '(my-function/integer 1) (expand-store (find-store 'my-function) '(my-function 1))))))
