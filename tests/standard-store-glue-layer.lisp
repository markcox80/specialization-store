;; Not the best check. Some would say that the wine has increased my
;; apathy. 
(unless (eql *package* (find-package "SPECIALIZATION-STORE.TESTS.SYNTAX-LAYER"))
  (error "This file can only be processed using SPECIALIZATION-STORE.TESTS::PROCESS-SYNTAX-LAYER-TESTS"))

(glue-layer-test ensure-store/initial
  (test ensure-store
    (is-false (find-store 'my-function nil))
    (ensure-store 'my-function '(a))
    (is-true (typep (find-store 'my-function) 'standard-store))

    (signals inapplicable-arguments-error
      (funcall (fdefinition 'my-function) 1))

    (make-store-unbound 'my-function)
    (is-false (find-store 'my-function nil))
    (is-false (fboundp 'my-function))))

(glue-layer-test ensure-store/reinitialised
  (test ensure-store
    (let ((fn1 #'identity)
          (fn2 #'print))
      (ensure-store 'my-function '(a) :value-completion-function fn1)
      (is (eql fn1 (store-value-completion-function (find-store 'my-function))))
      (ensure-store 'my-function '(a) :value-completion-function fn2)
      (is (eql fn2 (store-value-completion-function (find-store 'my-function)))))))
