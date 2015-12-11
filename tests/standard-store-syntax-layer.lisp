;; Not the best check. Some would say that the wine has increased my
;; apathy. 
(unless (eql *package* (find-package "SPECIALIZATION-STORE.TESTS.SYNTAX-LAYER"))
  (error "This file can only be processed using SPECIALIZATION-STORE.TESTS::PROCESS-SYNTAX-LAYER-TESTS"))

(syntax-layer-test basic
  (defstore example (a))
  
  (defspecialization example ((a (integer 0)))
    (1+ a))

  (defspecialization example ((a (integer * (0))))
    (1- a))

  (test basic
    (is (=  1 (example 0)))
    (is (= -2 (example -1)))
    (signals no-applicable-specialization-error (example "Hey"))))

(syntax-layer-test basic/rest
  (defstore example (a &rest args))

  (defspecialization example ((a (integer 0)))
    (1+ a))

  (defspecialization example ((a (integer 0)) (b (integer 0)))
    (+ a b))

  (test basic/rest
    (is (= 1 (example 0)))
    (is (= 4 (example 1 3)))
    (signals no-applicable-specialization-error (example -1))
    (signals no-applicable-specialization-error (example 0 -1))
    (signals no-applicable-specialization-error (example -1 0))
    (signals no-applicable-specialization-error (example 0 1 2))))

(syntax-layer-test lexical-environment/optional
  (flet ((init-a ()
           5))
    (defstore example (&optional (a (init-a)) (b a))))

  (defspecialization example ((a integer) (b integer))
    (declare (ignore a b))
    'integer-integer)  

  (defspecialization example (a b)
    (declare (ignore a b))
    't-t)

  (test lexical-environment
    (is (eql 'integer-integer (example)))
    (is (eql 't-t (example "Hey")))))

(syntax-layer-test lexical-environment/keywords
  (flet ((init-a ()
           5))
    (defstore example (&key (a (init-a)) (b a))))

  (defspecialization example (&key (a integer) (b integer))
    (declare (ignore a b))
    'integer-integer)  

  (defspecialization example (&key a b)
    (declare (ignore a b))
    't-t)

  (test lexical-environment
    (is (eql 'integer-integer (example)))
    (is (eql 't-t (example :a "Hey")))))

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

(syntax-layer-test named-specializations
  (defstore example (a))

  (defspecialization (example :name example/integer) ((a integer))
    (1+ a))

  (test named-specializations
    (is (= 1 (example 0)))
    (is (= 1 (example/integer 0)))
    (is (equal '(example/integer 0) (introspect-environment:compiler-macroexpand-1 '(example 0))))))

(syntax-layer-test define-specialization
  (defstore example (a))

  (define-specialization example ((a integer))
    (:function (lambda (a)
                 (1+ a)))
    (:expand-function (compiler-macro-lambda (a)
                        `(1- ,a))))

  (test define-specialization
    (let ((x 0))
      (is (= 1 (example x))))

    (is (equal '(1- 0) (introspect-environment:compiler-macroexpand-1 '(example 0))))))

(syntax-layer-test define-specialization/invalid-options
  (defstore example (a))

  (test invalid-options
    (signals warning (macroexpand '(define-specialization example ((a integer))
                                     (:function (lambda (a) (1+ a)))
                                     (:expand-function (compiler-macro-lambda (a) `(1- ,a)))
                                    (:inline t))))))
