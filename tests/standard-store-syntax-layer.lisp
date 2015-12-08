(unless (eql *package* (find-package "SPECIALIZATION-STORE.TESTS.SYNTAX-LAYER"))
  (error "This file can only be processed using PROCESS-SYNTAX-LAYER-TESTS"))

(syntax-layer-test basic
  (defstore example (a))

  (defspecialization example ((a (integer 0)))
    (1+ a))

  (defspecialization example ((a (integer * (0))))
    (1- a))

  (test basic
    (is (=  1 (example 0)))
    (is (= -2 (example -1)))))

(syntax-layer-test redefinition
  (defstore example (a))
  
  (defspecialization example ((a (integer 0)))
    (1+ a))

  (defstore example (a))

  (test redefinition
    (is (= 1 (example 0)))))

(syntax-layer-test inlining
  (defstore example (a))

  (defspecialization (example :inline t) ((a (integer 0)))
    (1+ a))

  (defun foo (x)
    (example (the (integer 0) x)))

  (compile 'foo)

  (defspecialization (example :inline t) ((a (integer 0)))
    (1- a))

  (test inlining
    (is (= -1 (example 0)))
    (is (= 1 (foo 0)))))
