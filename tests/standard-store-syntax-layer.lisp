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

  (defspecialization example ((a integer) b) (eql 2)
    (declare (ignore a b))
    2)

  (defspecialization example (a (b integer)) (eql 3)
    (declare (ignore a b))
    3)

  (defspecialization example ((a float) (b integer)) (eql 4)
    (declare (ignore a b))
    4)

  (test basic/rest/2
    (is (= 1 (example 1 2 3 4)))
    (is (= 1 (example 1 2 3 4 5)))
    (is (= 2 (example 1 "hey")))
    (is (= 3 (example "hey" 1)))
    (is (= 4 (example 5d0 2)))
    (signals inapplicable-arguments-error (example 1d0 1d0))
    (signals inapplicable-arguments-error (example 1d0 1d0 1d0))
    (signals inapplicable-arguments-error (example 1))))

(syntax-layer-test basic/rest/3
  (defstore example (a &rest args))

  (defspecialization example ((a integer)) (eql 1)
    (declare (ignore a))
    1)

  (defspecialization example ((a integer) &rest (args integer)) (eql 2)
    (declare (ignore a args))
    2)

  (defspecialization example ((a integer) &rest (args float)) (eql 3)
    (declare (ignore a args))
    3)

  (defspecialization example ((a integer) &rest args) (eql 4)
    (declare (ignore a args))
    4)

  (defspecialization example (a (b string)) (eql 5)
    (declare (ignore a b))
    5)

  (test basic/rest/3
    (is (= 1 (example 1)))
    (is (= 2 (example 1 2)))
    (is (= 2 (example 1 2 3)))
    (is (= 3 (example 1 2.0 3d0 4.0)))
    (is (= 3 (example 1 2.0 3.0)))
    (is (= 4 (example 1 2.0 3.0 4)))
    (is (= 5 (example "hey" "there")))
    (is (= 5 (example 3.0 "hey")))
    (is-true (member (example 1 "hey") '(4 5)))))

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
    (is (= 3 (example)))
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

(syntax-layer-test inlining/required
  (defstore example (a b))

  (defspecialization (example :inline t) ((a (integer 0)) (b (integer 0))) (integer 1)
    (+ (1+ a) b))

  (defun foo (x y)
    (example (the (integer 0) x)
             (the (integer 0) y)))

  (compile 'foo)

  (fmakunbound 'example)

  (test inlining
    (is (= 1 (foo 0 0)))
    (is (= 6 (foo 2 3)))))

(syntax-layer-test inlining/optional
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (let ((x 0))
      (flet ((compute-default (a)
               (prog1 (+ a x)
                 (incf x))))
        (defstore example (a &optional (b (the real (compute-default a))) (c (the real (1+ b))))))

      (defun reset ()
        (setf x 0)
        (values))))

  (defspecialization (example :inline t) ((a real) (b real) (c real)) real
    (+ a b c))

  (defun foo (x)
    (example (the real x)))

  (defun foo2 (x y)
    (example (the real x) (the real y)))

  (compile 'foo)
  (compile 'foo2)

  (fmakunbound 'example)

  (test foo
    (reset)

    ;; x is 0
    (is (= 1 (foo 0))) ; a = 0, b = 0, c = 1
    ;; x is 1
    (is (= 6 (foo 1))) ; a = 1, b = 2, c = 3
    ;; x is 2
    (is (= 11 (foo 2)))) ; a = 2, b = 4, c = 5

  (test foo2
    (reset)

    ;; x is always 0 since b is specified.
    (is (= 3 (foo2 0 1))) ;; a = 0, b = 1, c = 2
    (is (= 6 (foo2 1 2))) ;; a = 1, b = 2, c = 3
    (is (= 9 (foo2 2 3))) ;; a = 2, b = 3, c = 4
    ))

(syntax-layer-test inlining/rest
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (flet ((init-b (a)
             (1+ a)))
      (defstore example (a &optional (b (the integer (init-b a))) &rest args))))

  (defspecialization (example :inline t) ((a integer) (b integer) &rest (args integer)) integer
    (reduce #'+ args :initial-value (+ a b)))

  (defun foo (a &optional (b 0 bp) (c 0 cp))
    (cond ((and bp cp)
           (example (the integer a) (the integer b) (the integer c)))
          (bp
           (example (the integer a) (the integer b)))
          (t
           (example (the integer a)))))

  (compile 'foo)
  (fmakunbound 'example)

  (test foo
    (is (= 1 (foo 0)))
    (is (= 3 (foo 1)))
    (is (= 2 (foo 1 1)))
    (is (= 3 (foo 2 1)))
    (is (= 5 (foo 2 1 2)))))

(syntax-layer-test inlining/keywords/sans-rest
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (flet ((init-c (a b)
             (+ a b)))
      (defstore example (&key (a 0) (b 0) (c (the integer (init-c a b)))))))

  (defspecialization (example :inline t) (&key (a integer) (b integer) (c integer)) integer
    (+ a b c))

  (defun foo (&key (a nil ap) (b nil bp) (c nil cp))
    (cond ((and ap bp cp)
           (example :a (the integer a) :b (the integer b) :c (the integer c)))
          ((and ap bp)
           (example :a (the integer a) :b (the integer b)))
          (ap
           (example :a (the integer a)))
          (t
           (example))))

  (compile 'foo)
  (fmakunbound 'example)

  (test foo
    (is (= 0 (foo :a 0)))
    (is (= 2 (foo :a 1)))
    (is (= 6 (foo :b 2 :a 1)))
    (is (= 10 (foo :c 6 :b 5 :a -1)))))

(syntax-layer-test inlining/keywords/with-rest
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (flet ((init-b (args)
             (loop
               for v in (rest args) by #'cddr
               sum v)))
      (defstore example (&rest args &key (a 0) (b (the integer (init-b args))) (c 0)))))

  (defspecialization (example :inline t) (&key (a integer) (b integer) (c integer)) integer
    (+ a b c))

  (defun foo (&key (a nil ap) (b nil bp) (c nil cp))
    (declare (ignore b bp))
    (cond ((and ap cp)
           (example :a (the integer a) :c (the integer c)))
          (ap
           (example :a (the integer a)))))

  (compile 'foo)
  (fmakunbound 'example)

  (test foo
    (is (= 6 (foo :a 1 :c 2)))
    (is (= 4 (foo :a 2)))))
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

(syntax-layer-test example/key/other-lexical-environment
  (defstore example (object &key &allow-other-keys))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (flet ((init-b (object)
             (mapcar #'1+ object)))
      (defspecialization example ((object list) &key (next (init-b object))) list
        (append object next))))

  (test lexical-environment
    (is (equal '(1 2 3 2 3 4) (example '(1 2 3))))
    (is (equal '(1 2 3 1) (example '(1 2 3) :next '(1))))))

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
