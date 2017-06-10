(in-package "SPECIALIZATION-STORE.STANDARD-STORE.TESTS")
(in-suite standard-store-tests)

(test store-specialization-congruence
  (flet ((do-add (store-lambda-list specialization-lambda-list)
           (let* ((store (make-instance 'standard-store :lambda-list store-lambda-list))
                  (specialization (make-instance 'standard-specialization :lambda-list specialization-lambda-list)))
             (add-specialization store specialization))))
    (macrolet ((good (store-lambda-list specialization-lambda-list)
                 `(finishes (do-add ',store-lambda-list ',specialization-lambda-list)))
               (bad (store-lambda-list specialization-lambda-list)
                 `(signals incongruent-specialization-error (do-add ',store-lambda-list ',specialization-lambda-list))))
      (good (a) (b))
      (bad (a) ())
      (bad (a) (b c))
      (bad (a) (c &rest args))
      (bad (a) (d &key))

      (good (a &optional b) (c d))
      (bad (a &optional b) ())
      (bad (a &optional b) (c d e))
      (bad (a &optional b) (c d &rest args))

      (good (a &optional b &key hey) (c d &key hey))
      (bad (a &optional b &key hey) (a b &key))
      (bad (a &optional b &key hey) (a b &rest args))

      (good (a &rest args) (b &rest args))
      (bad (a &rest args) ())
      (good (a &rest args) (b c &rest args)))))

(test add-and-remove-specialization
  (let* ((store (make-instance 'standard-store :lambda-list '(a b))))
    (flet ((add (specialized-lambda-list)
             (let ((s (make-instance 'standard-specialization :lambda-list specialized-lambda-list)))
               (add-specialization store s)
               s))
           (specialization-count (store)
             (length (store-specializations store))))
      (is (= 0 (specialization-count store)))
      (add '(a b))
      (is (= 1 (specialization-count store)))
      (add '(c d))
      (is (= 1 (specialization-count store)))
      (add '((c integer) d))
      (is (= 2 (specialization-count store)))
      (add '(c (d float)))
      (is (= 3 (specialization-count store)))
      (let ((s1 (add '((c integer) (d float)))))
        (is (= 4 (specialization-count store)))
        (let ((s2 (add '((d integer) (c float)))))
          (is (= 4 (specialization-count store)))
          (is-true (find s2 (store-specializations store)))
          (is-false (find s1 (store-specializations store)))

          (remove-specialization store s2)
          (is (= 3 (specialization-count store)))
          (is-false (find s2 (store-specializations store))))))))

(test add-specialization/keywords
  (let* ((store (make-instance 'standard-store :lambda-list '(&key a))))
    (flet ((add (specialized-lambda-list)
             (let ((s (make-instance 'standard-specialization :lambda-list specialized-lambda-list)))
               (add-specialization store s)
               s))
           (specialization-count (store)
             (length (store-specializations store))))
      (add '(&key a))
      (is (= 1 (specialization-count store)))
      (add '(&key (a t)))
      (is (= 1 (specialization-count store))))))

(test add-specialization/keywords/allow-other-keys
  (let* ((store (make-instance 'standard-store :lambda-list '(&key a))))
    (flet ((add (specialized-lambda-list)
             (let ((s (make-instance 'standard-specialization :lambda-list specialized-lambda-list)))
               (add-specialization store s)
               s))
           (specialization-count (store)
             (length (store-specializations store))))
      (add '(&key a))
      (is (= 1 (specialization-count store)))
      (add '(&key (a t) b))
      (is (= 1 (specialization-count store))))))

(test add-specialization/positional
  (let* ((store (make-instance 'standard-store :lambda-list '(a))))
    (flet ((add (specialized-lambda-list)
             (let ((s (make-instance 'standard-specialization :lambda-list specialized-lambda-list)))
               (add-specialization store s)
               s))
           (specialization-count (store)
             (length (store-specializations store))))
      (add '(a))
      (is (= 1 (specialization-count store)))
      (add '((a t)))
      (is (= 1 (specialization-count store))))))

(test add-specialization/rest
  (let* ((store (make-instance 'standard-store :lambda-list '(a &rest args))))
    (flet ((add (specialized-lambda-list)
             (let ((s (make-instance 'standard-specialization :lambda-list specialized-lambda-list)))
               (add-specialization store s)
               s))
           (specialization-count (store)
             (length (store-specializations store))))
      (add '(a &rest args))
      (is (= 1 (specialization-count store)))
      (add '(a &rest others))
      (is (= 1 (specialization-count store)))
      (add '(a (b integer)))
      (is (= 2 (specialization-count store))))))

(test invoking-store
  (let* ((store (make-instance 'standard-store
                               :lambda-list '(a)))
         (specialization (make-instance 'standard-specialization
                                        :lambda-list '((a integer))
                                        :function (lambda (c)
                                                    (1+ c))
                                        :expand-function (compiler-macro-lambda (a)
                                                           `(1+ ,a))))
         (specialization/no-expand-function (make-instance 'standard-specialization
                                                           :lambda-list '((a integer))
                                                           :function (lambda (c)
                                                                       (1+ c)))))
    (signals error (funcall-store store))
    (signals error (apply-store store nil))
    (signals error (expand-store store '(test)))

    (signals error (funcall-store store 1 2))
    (signals error (apply-store store (list 1 2)))
    (signals error (expand-store store '(test 1 2)))

    (signals inapplicable-arguments-error (funcall-store store 1))
    (signals inapplicable-arguments-error (apply-store store (list 1)))
    (let ((form '(test 1)))
      (is (eq form (expand-store store form))))

    (add-specialization store specialization)

    (is (= 2 (funcall-store store 1)))
    (is (= 3 (apply-store store (list 2))))
    (is (equal `(1+ 2) (expand-store store '(test 2))))

    (signals inapplicable-arguments-error (funcall-store store 1d0))
    (signals inapplicable-arguments-error (apply-store store (list 1d0)))
    (let ((form '(test 1d0)))
      (is (eq form (expand-store store form))))

    ;; Replace current specialization with one that has no expand
    ;; function.
    (add-specialization store specialization/no-expand-function)
    (is (= 2 (funcall-store store 1)))
    (is (= 2 (apply-store store (list 1))))
    (let ((form '(test 1)))
      (is (eq form (expand-store store form))))))

(test store-reinitialization
  (let* ((store (make-instance 'standard-store
                               :lambda-list '(&optional a)
                               :value-completion-function (lambda (continuation)
                                                            (lambda (&optional (a 1))
                                                              (funcall continuation a))))))
    (add-specialization store (make-instance 'standard-specialization
                                             :lambda-list '((a (integer 0)))
                                             :function (lambda (a)
                                                         (1+ a))))
    (is (= 2 (funcall-store store)))
    (finishes (reinitialize-instance store :lambda-list '(&optional b)))
    (signals store-error (reinitialize-instance store :lambda-list '(b)))

    (reinitialize-instance store :value-completion-function (lambda (continuation)
                                                              (lambda (&optional (b 2))
                                                                (funcall continuation b))))
    (is (= 3 (funcall-store store)))))

(test store-reinitialization/different-lambda-list
  ;; Changing lambda lists of a store object that does not contain
  ;; specializations is fine.
  (let* ((store (make-instance 'standard-store :lambda-list '(a))))
    (finishes (reinitialize-instance store :lambda-list '(a b))))

  ;; Changing lambda lists of a store object with specializations
  ;; should signal a store-error.
  (let* ((store (make-instance 'standard-store :lambda-list '(a)))
         (s (make-instance 'standard-specialization :lambda-list '((a integer)) :function #'1+)))
    (add-specialization store s)
    (signals store-error (reinitialize-instance store :lambda-list '(a b))))

  ;; Check that is ok to reuse the completion functions for a new
  ;; lambda list which is congruent with the old lambda list.
  (let* ((store (make-instance 'standard-store
                               :lambda-list '(&optional (a (init-a)))
                               :value-completion-function #'null
                               :type-completion-function #'null
                               :form-completion-function #'null)))
    (finishes (reinitialize-instance store :lambda-list '(&optional (b (init-b))))))

  ;; Signal an error if a new lambda list is supplied that is not
  ;; congruent with the old lambda list and no completion functions
  ;; are supplied.
  (let* ((store (make-instance 'standard-store :lambda-list '(a))))
    (signals store-error (reinitialize-instance store :lambda-list '(&optional (b (init-b))))))

  ;; Check the above case completes if completion functions are
  ;; specified.
  (let* ((store (make-instance 'standard-store :lambda-list '(a))))
    (finishes (reinitialize-instance store
                                     :lambda-list '(&optional (b (init-b)))
                                     :value-completion-function #'null
                                     :type-completion-function #'null
                                     :form-completion-function #'null))))

(test require-completion-functions
  (finishes (make-instance 'standard-store :lambda-list '(&optional a)))
  (finishes (make-instance 'standard-store :lambda-list '(&optional (a 2))))
  (finishes (make-instance 'standard-store :lambda-list '(&key a)))
  (finishes (make-instance 'standard-store :lambda-list '(&key (a 2))))

  (signals missing-completion-functions-error (make-instance 'standard-store :lambda-list '(&optional (a (hellow-world)))))
  (signals missing-completion-functions-error (make-instance 'standard-store :lambda-list '(&key (a (hello-world))))))

(test default-completion-functions/dispatch
  (let ((store (make-instance 'standard-store :lambda-list '(&optional (a 2))))
        (a (make-instance 'standard-specialization
                          :lambda-list '((a integer))
                          :function (lambda (a) (1+ a))
                          :expand-function (lambda (form env)
                                             (declare (ignore form env))
                                             :here))))
    (add-specialization store a)
    (is (= 3 (funcall-store store)))
    (is (eql :here (expand-store store '(test))))))

(test default-completion-functions/argument-forms
  (let ((store (make-instance 'standard-store :lambda-list '(&optional (a 2))))
        (a (make-instance 'standard-specialization
                          :lambda-list '((a integer))
                          :function (lambda (a) (1+ a))
                          :expand-function (compiler-macro-lambda (value)
                                             value))))
    (add-specialization store a)
    (is (eql 2 (expand-store store '(test))))))

(test completion-functions
  (let ((b-value 1))
    (labels ((init-b ()
               (incf b-value)
               b-value)
             (value-function (continuation)
               (lambda (a &optional (b (init-b)))
                 (funcall continuation a b)))
             (type-function (continuation)
               (lambda (a &optional (b '(integer 0)))
                 (funcall continuation a b))))
      (let ((store (make-instance 'standard-store
                                  :lambda-list '(a &optional b)
                                  :value-completion-function #'value-function
                                  :type-completion-function #'type-function))
            (a (make-instance 'standard-specialization
                              :lambda-list '(a (b (integer 0)))
                              :function (lambda (a b)
                                          (list a b))
                              :expand-function (compiler-macro-lambda (a b)
                                                 (declare (ignore a))
                                                 b))))
        (add-specialization store a)
        (is (equal (list "hey" 2) (funcall-store store "hey")))
        (is (equal (list "there" 3) (funcall-store store "there")))
        (is (equal (list "foobar" 10) (funcall-store store "foobar" 10)))
        (is (equal (list "hello" 4) (funcall-store store "hello")))
        (is (equal 2 (expand-store store '(example "hello" 2) nil)))
        ;; Doesn't match anything
        (let ((form '(test "hey" "there")))
          (is (eq form (expand-store store form))))))))

;;;; Dispatching

(test dispatch-function/basic
  (let* ((store (make-instance 'standard-store :lambda-list '(a &optional (b 2) &key (c 3)))))
    (labels ((make (lambda-list function)
               (make-instance 'standard-specialization :lambda-list lambda-list :function function))
             (add (lambda-list function)
               (add-specialization store (make lambda-list function))))
      (add '((a integer) b &key c) (lambda (a b &key c)
                                     (declare (ignore a b c))
                                     1))
      (add '(a (b float) &key (c (integer 0))) (lambda (a b &key c)
                                                 (declare (ignore a b c))
                                                 2))
      (add '(a (b float) &key (c (integer * (0)))) (lambda (a b &key c)
                                                     (declare (ignore a b c))
                                                     3))
      (add '((a (integer 10)) b &key c) (lambda (a b &key c)
                                          (declare (ignore a b c))
                                          4))
      (is (= 1 (funcall-store store 1)))
      (is (= 1 (funcall-store store 1 "here")))
      (is (= 1 (funcall-store store 1 "here" :c "there")))
      (is (= 2 (funcall-store store "blah" 2.0)))
      (is (= 2 (funcall-store store "blah" 3.0 :c 4)))
      (is (= 3 (funcall-store store "blah" 5.0 :c -1)))
      (is (= 4 (funcall-store store 10)))
      (is (= 4 (funcall-store store 10 "here")))
      (is (= 4 (funcall-store store 10 "here" :c "there")))
      (signals inapplicable-arguments-error (funcall-store store "blah" 3.0 :c 4.0)))))

(test dispatch-function/rest
  (let* ((store (make-instance 'standard-store :lambda-list '(a &rest args))))
    (labels ((make (lambda-list function)
               (make-instance 'standard-specialization :lambda-list lambda-list :function function))
             (add (lambda-list function)
               (add-specialization store (make lambda-list function))))
      (add '((a integer)) (lambda (a)
                            (declare (ignore a))
                            1))
      (add '(a (b float)) (lambda (a b)
                            (declare (ignore a b))
                            2))
      (add '(a (b float) (c (integer * (0)))) (lambda (a b c)
                                                (declare (ignore a b c))
                                                3))
      (add '((a (integer 10)) b c) (lambda (a b c)
                                     (declare (ignore a b c))
                                     4))
      (add '(a b c &rest args) (lambda (&rest args)
                                 (declare (ignore args))
                                 5))
      (is (= 1 (funcall-store store 1)))
      (is (= 1 (funcall-store store 10)))
      (is (= 2 (funcall-store store "blah" 2.0)))
      (is (= 3 (funcall-store store 1 2d0 -1)))
      (is (= 4 (funcall-store store 10 2d0 5)))
      (is (= 4 (funcall-store store 10 "here" "there")))
      (is (= 5 (funcall-store store 1 "here" :c "there")))
      (is (= 5 (funcall-store store "blah" 3.0 :c 4)))
      (is (= 5 (funcall-store store "blah" 5.0 :c -1)))
      (signals inapplicable-arguments-error (funcall-store store 1 "here"))
      (signals inapplicable-arguments-error (funcall-store store "blah")))))

(test dispatch-function/key-with-null-type
  (let* ((store (make-instance 'standard-store :lambda-list '(&key c))))
    (add-specialization store (make-instance 'standard-specialization
                                             :lambda-list '(&key (c integer))
                                             :function (lambda (&key c)
                                                         (1+ c))))
    (add-specialization store (make-instance 'standard-specialization
                                             :lambda-list '(&key (c null))
                                             :function (lambda (&key c)
                                                         (declare (ignore c))
                                                         'null)))
    (is (= 2 (funcall-store store :c 1)))
    (is (eql 'null (funcall-store store)))))

(test dispatch-function/fixed-arity/positional
  (labels ((collate (list offsets)
             (let ((length (length list)))
               (loop
                  for index from 0 below length
                  collect
                    (loop
                       for offset in offsets
                       collect (elt list (mod (+ index offset) length))))))
           (specialization< (a b)
             (cond ((and (null a) (null b))
                    t)
                   ((and a b)
                    (let ((a-type (first a))
                          (b-type (first b)))
                      (cond ((alexandria:type= a-type b-type)
                             (specialization< (rest a) (rest b)))
                            ((subtypep a-type b-type)
                             t)))))))
    (let* ((types '((integer * (0)) (integer 0)
                    double-float single-float
                    string null))
           (values '(-1 -10 10 5 5d0 2.0 1.0 -10.0 -15d0 "hello" "there" nil))
           (type-count (length types)))
      (dotimes (a-offset type-count)
        (dotimes (b-offset type-count)
          (dotimes (c-offset type-count)
            (let* ((argument-types (collate types (list a-offset b-offset c-offset)))
                   (argument-values (collate values (list a-offset b-offset c-offset)))
                   (store (make-instance 'standard-store :lambda-list '(a b &optional c)))
                   (table (loop
                             for specialization-index from 0 below type-count
                             for (a-type b-type c-type) in argument-types
                             for lambda-list = `((a ,a-type) (b ,b-type) (c ,c-type))
                             for specialization = (make-instance 'standard-specialization :lambda-list lambda-list)
                             do
                               (reinitialize-instance specialization :function (let ((specialization specialization))
                                                                                 (lambda (a b c)
                                                                                   (declare (ignore a b c))
                                                                                   specialization)))
                               (add-specialization store specialization)
                             collect (list (list a-type b-type c-type) specialization))))
              (dolist (function-inputs argument-values)
                (let ((precedence (sort (remove-if-not #'(lambda (specialization-types)
                                                           (every #'typep function-inputs specialization-types))
                                                       table
                                                       :key #'first)
                                        #'specialization<
                                        :key #'first)))
                  (cond (precedence
                         (let* ((expected-specialization (second (first precedence)))
                                (actual-specialization (apply-store store function-inputs)))
                           (is (eql expected-specialization actual-specialization)
                               "Incorrect specialization ~W selected from specializations ~W for input arguments ~W. Expected ~W."
                               actual-specialization
                               (mapcar #'second table)
                               function-inputs
                               expected-specialization)))
                        (t
                         (signals inapplicable-arguments-error (apply-store store function-inputs)))))))))))))

(test dispatch-function/fixed-arity/keywords
  (labels ((collate (list offsets)
             (let ((length (length list)))
               (loop
                  for index from 0 below length
                  collect
                    (loop
                       for offset in offsets
                       collect (elt list (mod (+ index offset) length))))))
           (specialization< (a b)
             (cond ((and (null a) (null b))
                    t)
                   ((and a b)
                    (let ((a-type (first a))
                          (b-type (first b)))
                      (cond ((alexandria:type= a-type b-type)
                             (specialization< (rest a) (rest b)))
                            ((subtypep a-type b-type)
                             t)))))))
    (let* ((types '((integer * (0)) (integer 0)
                    double-float single-float
                    string null))
           (values '(-1 -10 10 5 5d0 2.0 1.0 -10.0 -15d0 "hello" "there" nil))
           (type-count (length types)))
      (dotimes (a-offset type-count)
        (dotimes (b-offset type-count)
          (dotimes (c-offset type-count)
            (let* ((argument-types (collate types (list a-offset b-offset c-offset)))
                   (argument-values (collate values (list a-offset b-offset c-offset)))
                   (store (make-instance 'standard-store :lambda-list '(a &key b c)))
                   (table (loop
                             for specialization-index from 0 below type-count
                             for (a-type b-type c-type) in argument-types
                             for lambda-list = `((a ,a-type) &key (b ,b-type) (c ,c-type))
                             for specialization = (make-instance 'standard-specialization :lambda-list lambda-list)
                             do
                               (reinitialize-instance specialization :function (let ((specialization specialization))
                                                                                 (lambda (a &key b c)
                                                                                   (declare (ignore a b c))
                                                                                   specialization)))
                               (add-specialization store specialization)
                             collect (list (list a-type b-type c-type) specialization))))
              (dolist (function-inputs argument-values)
                (let ((precedence (sort (remove-if-not #'(lambda (specialization-types)
                                                           (every #'typep function-inputs specialization-types))
                                                       table
                                                       :key #'first)
                                        #'specialization<
                                        :key #'first)))
                  (destructuring-bind (a b c) function-inputs
                    (cond (precedence
                           (let* ((expected-specialization (second (first precedence)))
                                  (actual-specialization (funcall-store store a :b b :c c)))
                             (is (eql expected-specialization actual-specialization)
                                 "Incorrect specialization ~W selected from specializations ~W for input arguments ~W. Expected ~W."
                                 actual-specialization
                                 (mapcar #'second table)
                                 function-inputs
                                 expected-specialization)))
                          (t
                           (signals inapplicable-arguments-error
                             (funcall-store store a :b b :c c))))))))))))))
