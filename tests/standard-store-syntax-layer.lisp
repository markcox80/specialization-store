;; Not the best check. Some would say that the wine has increased my
;; apathy.
(unless (eql *package* (find-package "SPECIALIZATION-STORE.TESTS.SYNTAX-LAYER"))
  (error "This file can only be processed using SPECIALIZATION-STORE.TESTS::PROCESS-SYNTAX-LAYER-TESTS"))

(syntax-layer-test basic
  (defstore example (a))

  (defspecialization example ((a (integer 0))) (integer 1)
    (1+ a))

  (defspecialization example ((a (integer * (0)))) (integer * (1))
    (1- a))

  (test basic
    (is (=  1 (example 0)))
    (is (= -2 (example -1)))
    (signals inapplicable-arguments-error (example "Hey"))))

(syntax-layer-test basic/2
  (defstore example (a b c))

  (defspecialization example (a (b float) (c (integer * (0)))) (eql 1)
    (declare (ignore a b c))
    1)

  (defspecialization example (a (b float) (c (integer 10))) (eql 2)
    (declare (ignore a b c))
    2)

  (defspecialization example (a (b (integer * (0))) (c float)) (eql 3)
    (declare (ignore a b c))
    3)

  (defspecialization example (a (b (integer 10)) (c float)) (eql 4)
    (declare (ignore a b c))
    4)

  (defspecialization example (a b c) (eql 5)
    (declare (ignore a b c))
    5)

  (test basic/2
    (is (= 1 (example 10 2d0 -1)))
    (is (= 2 (example 10 2d0 10)))
    (is (= 3 (example 10 -1 2d0)))
    (is (= 4 (example 10 10 2d0)))
    (is (= 5 (example 10 2d0 1)))
    (is (= 5 (example 10 1 2d0)))))

(syntax-layer-test basic/rest
  (defstore example (a &rest args))

  (defspecialization example ((a (integer 0))) (integer 1)
    (1+ a))

  (defspecialization example ((a (integer 0)) (b (integer 0))) (integer 0)
    (+ a b))

  (test basic/rest
    (is (= 1 (example 0)))
    (is (= 4 (example 1 3)))
    (signals inapplicable-arguments-error (example -1))
    (signals inapplicable-arguments-error (example 0 -1))
    (signals inapplicable-arguments-error (example -1 0))
    (signals inapplicable-arguments-error (example 0 1 2))))

(syntax-layer-test basic/rest/2
  (defstore example (a &rest args))

  (defspecialization example (a b c d &rest args) (eql 1)
    (declare (ignore a b c d args))
    1)

  (defspecialization example (a b &optional c) (eql 2)
    (declare (ignore a b c))
    2)

  (defspecialization example ((a integer) b) (eql 3)
    (declare (ignore a b))
    3)

  (defspecialization example (a (b integer)) (eql 4)
    (declare (ignore a b))
    4)

  (defspecialization example ((a float) (b integer)) (eql 5)
    (declare (ignore a b))
    5)

  (test basic/rest/2
    (is (= 1 (example 1 2 3 4)))
    (is (= 1 (example 1 2 3 4 5)))
    (is (= 2 (example 1d0 1d0)))
    (is (= 2 (example 1d0 1d0 1d0)))
    (is (= 3 (example 1 "hey")))
    (is (= 4 (example "hey" 1)))
    (is (= 5 (example 5d0 2)))
    (is (= 2 (example 5d0 "hey")))
    (is (= 2 (example 5d0 1 "hey")))
    (signals inapplicable-arguments-error (example 1))))

(syntax-layer-test lexical-environment/optional
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (flet ((init-a ()
             5))
      (defstore example (&optional (a (init-a)) (b a)))))

  (defspecialization example ((a integer) (b integer)) t
    (declare (ignore a b))
    'integer-integer)

  (defspecialization example (a b) t
    (declare (ignore a b))
    't-t)

  (test lexical-environment
    (is (eql 'integer-integer (example)))
    (is (eql 't-t (example "Hey")))))

(syntax-layer-test lexical-environment/keywords
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (flet ((init-a ()
             5))
      (defstore example (&key (a (init-a)) (b a)))))

  (defspecialization example (&key (a integer) (b integer)) t
    (declare (ignore a b))
    'integer-integer)

  (defspecialization example (&key a b) t
    (declare (ignore a b))
    't-t)

  (test lexical-environment
    (is (eql 'integer-integer (example)))
    (is (eql 't-t (example :a "Hey")))))

(syntax-layer-test lexical-environment/keywords/init-form-function
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (flet ((init-b (a)
             (1+ a)))
      (defstore example (&key (a 1) (b (the number (init-b a)))))))

  (defspecialization (example :inline t) (&key (a number) (b number)) t
    (+ a b))

  (test lexical-environment
    (is (= 3 (print (example))))
    (is (= 5 (example :a 2 :b 3)))
    (is (= 3 (funcall (compile nil `(lambda ()
                                      ,(introspect-environment:compiler-macroexpand '(example)))))))))

(syntax-layer-test redefinition
  (defstore example (a))

  (defspecialization example ((a (integer 0))) (integer 1)
    (1+ a))

  (defstore example (a))

  (test redefinition
    (is (= 1 (example 0)))))

(syntax-layer-test inlining
  (defstore example (a))

  (defspecialization (example :inline t) ((a (integer 0))) (integer 1)
    (1+ a))

  (defun foo (x)
    (example (the (integer 0) x)))

  (compile 'foo)

  (defspecialization (example :inline t) ((a (integer 0))) (integer * (0))
    (1- a))

  (test inlining
    (is (= -1 (example 0)))
    (is (= 1 (foo 0)))))

(syntax-layer-test named-specializations
  (defstore example (a))

  (defspecialization (example :name example/integer) ((a integer)) integer
    (1+ a))

  (test named-specializations
    (is (= 1 (example 0)))
    (is (= 1 (example/integer 0)))
    (is (equal '(example/integer 0) (introspect-environment:compiler-macroexpand-1 '(example 0))))))

(syntax-layer-test define-specialization
  (defstore example (a))

  (define-specialization example ((a integer)) integer
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
    (signals warning (macroexpand '(define-specialization example ((a integer)) integer
                                     (:function (lambda (a) (1+ a)))
                                     (:expand-function (compiler-macro-lambda (a) `(1- ,a)))
                                    (:inline t))))))

(syntax-layer-test example/rest
  (defstore example (object &rest args))

  (defspecialization example ((object list) index) t
    (elt object index))

  (defspecialization example ((object simple-vector) index) number
    (1+ (elt object index)))

  (defspecialization example ((object array) &rest args) t
    (apply #'aref object args))

  (test example/rest
    (let ((v1 (make-array 6 :initial-element 1d0 :element-type 'double-float))
          (v2 (make-list 6 :initial-element 1)))
      (is (= 1d0 (example v1 0)))
      (is (= 1 (example v2 0))))))

(syntax-layer-test example/rest/optional-lexical-environment
  (defstore example (object &rest args))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (flet ((init-b (object)
             (mapcar #'1+ object)))
      (defspecialization example ((object list) &optional (next (init-b object))) list
        (append object next))))

  (test lexical-environment
    (is (equal '(1 2 3 2 3 4) (example '(1 2 3))))
    (is (equal '(1 2 3 1) (example '(1 2 3) '(1))))))

(syntax-layer-test make-store-unbound
  (defstore example (object))

  (defspecialization (example :name example/integer :inline t) ((object integer)) integer
    (1+ object))

  (test make-store-unbound
    (is-true (find-store 'example))
    (is-true (fboundp 'example))
    (is-true (compiler-macro-function 'example))
    (is-true (fboundp 'example/integer))
    (is-true (compiler-macro-function 'example/integer))

    (make-store-unbound 'example)

    (signals invalid-store-name-error (find-store 'example))
    (is-false (fboundp 'example))
    (is-false (compiler-macro-function 'example))
    (is-false (fboundp 'example/integer))
    (is-false (compiler-macro-function 'example/integer))))
