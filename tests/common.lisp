(in-package "SPECIALIZATION-STORE.TESTS")
(in-suite all-tests)

(proclaim '(ftype (function (integer) (values integer integer)) global-function))
(defun global-function (a)
  (values (+ a 1) (+ a 2)))

#+specialization-store.features:function-declarations
(test determine-form-multiple-value-type/with-function-declarations-feature
  (flet ((local-function (a)
           (concatenate 'string "Hello" a))
         (imperative (a)
           (declare (ignore a))
           (values)))
    (declare (ftype (function (string) string) local-function)
             (ftype (function (integer) (values)) imperative)
             (ignorable (function local-function) (function imperative)))
    (symbol-macrolet ((my-symbol (local-function 1)))
      ;; Macrolet has to be here in order for the environment to capture
      ;; the flet and symbol macrolet.
      (macrolet ((compute (form &environment environment)
                   `(quote ,(determine-form-multiple-value-type form environment))))
        ;; Global Function
        (let ((result (compute (global-function 1))))
          (is (or (equal '(values integer integer) result)
                  (equal '(values integer integer &rest t) result))))
        ;; Local Function
        (let ((result (compute (local-function "Hello"))))
          (is (or (equal 'string result)
                  (equal '(values string) result)
                  (equal '(values string &rest t) result)
                  (equal 'base-string result)
                  (equal '(values base-string) result)
                  (equal '(values base-string &rest t) result))))
        (let ((result (compute (imperative 1))))
          (is (or (equal '* result)
                  (equal 't result)
                  (equal '(values) result)
                  (equal '(values &rest t) result))))
        ;; Symbol macrolet
        (let ((result (compute my-symbol)))
          (is (or (equal 'string result)
                  (equal '(values string) result)
                  (equal '(values string &rest t) result)
                  (equal 'base-string result)
                  (equal '(values base-string) result)
                  (equal '(values base-string &rest t) result))))
        ;; Non existent function
        (is (equal '* (compute (non-existent-function 1 2 3))))))))

#-specialization-store.features:function-declarations
(test determine-form-multiple-value-type/without-function-declarations-feature
  (flet ((local-function (a)
           (concatenate 'string "Hello string"))
         (imperative (a)
           (values)))
    (declare (ftype (function (string) string) local-function)
             (ftype (function (integer) (values)) imperative)
             (ignorable (function local-function) (function imperative)))
    (symbol-macrolet ((my-symbol (local-function 1)))
      (macrolet ((compute (form &environment environment)
                   `(quote ,(determine-form-multiple-value-type form environment))))
        (is (equal '* (compute (global-function 1))))
        ;; Local Functions
        (is (equal '* (compute (local-function "Hello"))))
        (is (equal '* (compute (imperative 1))))
        ;; Symbol macrolet
        (is (equal '* (compute my-symbol)))
        ;; Non existent function
        (is (equal '* (compute (non-existent-function 1 2))))))))

(test determine-form-value-type
  (flet ((local-function (a)
             (concatenate 'string "Hello" a))
         (imperative (a)
           (declare (ignore a))
           (values)))
    (declare (ftype (function (string) string) local-function)
             (ftype (function (integer) (values)) imperative)
             (ignorable (function local-function) (function imperative)))
    (symbol-macrolet ((my-symbol "Blah blah"))
      ;; Macrolet has to be here in order for the environment to capture
      ;; the flet and the symbol macrolet
      (macrolet ((compute (form &environment environment)
                   `(quote ,(determine-form-value-type form environment))))
        ;; Lexical Variables
        (let ((a 5))
          (is (equal t (compute a))))

        #+specialization-store.features:variable-types
        (let ((a 5))
          (declare (type (integer 5 5) a)
                   (ignorable a))
          (is (equal '(integer 5 5) (compute a))))

        ;; Constants
        (is (equal 'null (compute nil)))
        (is (equal '(eql hey) (compute 'hey)))
        (is (equal '(eql 1) (compute 1)))
        (is (equal '(eql #\c) (compute #\c)))
        (is (not (equal '(eql "hey") (compute (copy-seq "hey")))))
        (is (subtypep (compute "hey") (type-of "hey")))
        (let ((object my-symbol))
          (is (equal `(eql ,object) (compute my-symbol))))

        ;; Constants (non eql types)


        ;; Forms like (the type form)
        (is (equal 'integer (compute (the integer a))))

        ;; Symbol macrolet
        (let ((object my-symbol))
          (is (equal `(eql ,object) (compute my-symbol))))

        ;; Functions
        #+specialization-store.features:function-declarations
        (progn
          (let ((result (compute (global-function 1))))
            (is (equal 'integer result)))

          (let ((result (compute (local-function "Hello"))))
            (is (or (equal 'string result)
                    (equal 'base-string result))))

          (let ((result (compute (imperative 1))))
            (is (or (equal 't result)
                    (equal 'null result))))

          (is (equal 't (compute (non-existent-function 1 2 3)))))

        #-specialization-store.features:function-declarations
        (progn
          (is (equal t (compute (global-function 1))))
          (is (equal t (compute (local-function "Hello"))))
          (is (equal t (compute (imperative 1))))
          (is (equal t (compute (non-existent-function 1 2 3)))))))))
