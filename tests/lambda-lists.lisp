(in-package "SPECIALIZATION-STORE.LAMBDA-LISTS.TESTS")
(in-suite lambda-list-tests)


;;;; parse-store-lambda-list

(test parse-store-lambda-list
  (flet ((do-trial (store-lambda-list req opt rest key? keys allow-other-keys?)
           (let ((store-parameters (parse-store-lambda-list store-lambda-list)))
             (is-true (typep store-parameters 'store-parameters))
             (is (and (equal req (mapcar #'parameter-lambda-list-specification (required-parameters store-parameters)))
                      (equal opt (when (optional-parameters-p store-parameters)
                                   (mapcar #'parameter-lambda-list-specification (optional-parameters store-parameters))))
                      (eql rest (when (rest-parameter-p store-parameters)
                                  (parameter-lambda-list-specification (rest-parameter store-parameters))))
                      (eql key? (keyword-parameters-p store-parameters))
                      (equal keys (when (keyword-parameters-p store-parameters)
                                    (mapcar #'parameter-lambda-list-specification (keyword-parameters store-parameters))))
                      (eql allow-other-keys? (allow-other-keys-p store-parameters)))
                 "Failed to parse store lambda list ~W correctly." store-lambda-list))))
    (let ((required '((nil . nil)
                      ((a) . (a))
                      ((a b) . (a b))))
          (optional '((nil . nil)
                      ((&optional) . nil)
                      ((&optional d) . (d))
                      ((&optional (e 5) d) . ((e 5) d))
                      ((&optional (d 5 dp) (e d ep)) . ((d 5 dp) (e d ep)))))
          (rest '((nil . nil)
                  ((&rest args) . args)))
          (keys '((nil nil nil nil)
                  ((&key) nil t nil)
                  ((&key m) (m) t nil)
                  ((&key m (n 2)) (m (n 2)) t nil)
                  ((&key (o 5) &allow-other-keys) ((o 5)) t t)
                  ((&key ((:o p) 5)) (((:o p) 5)) t nil)
                  ((&key (m 5 mp) (n m np)) ((m 5 mp) (n m np)) t nil))))
      (loop for (r-list . r) in required do
           (loop for (o-list . o) in optional do
                (loop for (rest-list . rest-var) in rest do
                     (loop for (keys-list k keys? allow-other-keys?) in keys do
                          (let ((lambda-list (append r-list o-list rest-list keys-list)))
                            (do-trial lambda-list r o rest-var keys? k allow-other-keys?)))))))))

(test parse-store-lambda-list/invalid-store-lambda-lists
  (flet ((trial (store-lambda-list)
           (signals parse-store-lambda-list-error (parse-store-lambda-list store-lambda-list))))
    ;; Invalid markers
    (trial '(&rest))
    (trial '(&rest &rest))
    (trial '(&rest &key))
    (trial '(&rest &optional))
    (trial '(&rest &allow-other-keys))
    (trial '(&rest var &optional))
    (trial '(&rest var &allow-other-keys))
    (trial '(&rest var var2))
    (trial '(&optional &allow-other-keys))
    (trial '(&key &optional))
    (trial '(&key &rest))
    (trial '(&key &key))
    (trial '(&key &allow-other-keys &allow-other-keys))
    (trial '(&allow-other-keys))
    ;; Invalid parameter specifications
    (trial '((a 1)))
    (trial '(nil))
    (trial '(&optional nil))
    (trial '(&optional (nil nil)))
    (trial '(&rest nil))
    (trial '(&rest (args 1)))
    (trial '(&key nil))
    (trial '(&key (nil nil)))
    ;; Duplicate Keywords
    (trial '(&key a a))
    (trial '(&key a ((:a b))))
    ;; Duplicate variables
    (trial '(a a))
    (trial '(a &optional a))
    (trial '(a &optional (a 5)))
    (trial '(a &optional (b 5 a)))
    (trial '(a &rest a))
    (trial '(a &key ((:m a))))
    (trial '(a &key (c 5 a)))
    (trial '(&optional a &key ((:m a))))
    (trial '(&optional (a nil bad) &key (b nil bad)))))

;;;; parse-specialization-lambda-list

(test parse-specialization-lambda-list
  (flet ((do-trial (specialization-lambda-list req opt rest key? keys allow-other-keys?)
           (let ((specialization-parameters (parse-specialization-lambda-list specialization-lambda-list)))
             (is-true (typep specialization-parameters 'specialization-parameters))
             (is (and (equal req (mapcar #'parameter-lambda-list-specification (required-parameters specialization-parameters)))
                      (equal opt (when (optional-parameters-p specialization-parameters)
                                   (mapcar #'parameter-lambda-list-specification (optional-parameters specialization-parameters))))
                      (eql rest (when (rest-parameter-p specialization-parameters)
                                  (parameter-lambda-list-specification (rest-parameter specialization-parameters))))
                      (eql key? (keyword-parameters-p specialization-parameters))
                      (equal keys (when (keyword-parameters-p specialization-parameters)
                                    (mapcar #'parameter-lambda-list-specification (keyword-parameters specialization-parameters))))
                      (eql allow-other-keys? (allow-other-keys-p specialization-parameters)))
                 "Failed to parse specialization lambda list ~W correctly." specialization-lambda-list))))
    (let ((required '((nil . nil)
                      ((a) . ((a t)))
                      ((a b) . ((a t) (b t)))
                      ((a (b integer)) . ((a t) (b integer)))))
          (optional '((nil . nil)
                      ((&optional) . nil)
                      ((&optional d) . (d))
                      ((&optional (e 5) d) . ((e 5) d))
                      ((&optional (e 6 e-p)) . ((e 6 e-p)))))
          (rest '((nil . nil)
                  ((&rest args) . args)))
          (keys '((nil nil nil nil)
                  ((&key) nil t nil)
                  ((&key m) (m) t nil)
                  ((&key m (n 2)) (m (n 2)) t nil)
                  ((&key (o 5) &allow-other-keys) ((o 5)) t t)
                  ((&key (m 10 m-p)) ((m 10 m-p)) t nil)
                  ((&key ((:k m) 10 m-p) &allow-other-keys) (((:k m) 10 m-p)) t t))))
      (loop for (r-list . r) in required do
           (loop for (o-list . o) in optional do
                (loop for (rest-list . rest-var) in rest do
                     (loop for (keys-list k keys? allow-other-keys?) in keys do
                          (let ((lambda-list (append r-list o-list rest-list keys-list)))
                            (do-trial lambda-list r o rest-var keys? k allow-other-keys?)))))))))

(test parse-specialization-lambda-list/invalid-specialization-lambda-lists
  (flet ((trial (specialization-lambda-list)
           (signals parse-specialization-lambda-list-error (parse-specialization-lambda-list specialization-lambda-list))))
    ;; Invalid markers
    (trial '(&rest))
    (trial '(&rest &rest))
    (trial '(&rest &key))
    (trial '(&rest &optional))
    (trial '(&rest &allow-other-keys))
    (trial '(&rest var &optional))
    (trial '(&rest var &allow-other-keys))
    (trial '(&rest var a))
    (trial '(&optional &allow-other-keys))
    (trial '(&key &optional))
    (trial '(&key &rest))
    (trial '(&key &key))
    (trial '(&key &allow-other-keys &allow-other-keys))
    (trial '(&allow-other-keys))
    ;; Invalid parameter specifications
    (trial '(nil))
    (trial '(&optional nil))
    (trial '(&optional (nil nil)))
    (trial '(&rest nil))
    (trial '(&rest (args 1)))
    (trial '(&key nil))
    (trial '(&key (nil nil)))
    ;; Duplicate keywords
    (trial '(&key a a))
    (trial '(&key a a b))
    (trial '(&key a (a 1)))
    (trial '(&key a ((:a b))))
    (trial '(&key ((:a b)) (a 2)))
    ;; Duplicate variables
    (trial '(a a))
    (trial '(a &optional a))
    (trial '(a &optional b a))
    (trial '(a &optional b (c nil a)))
    (trial '(a &rest a))
    (trial '(a &key a))
    (trial '(a &key b a))
    (trial '(a &key b (c nil a)))
    (trial '(&optional a a))
    (trial '(&optional a &key a))
    (trial '(&optional a &key b a))
    (trial '(&optional a &key b (c nil a)))))

;;;; Congruent Lambda lists

(test congruent-specialization-parameters
  (flet ((do-trial (expected store-lambda-list specialization-lambda-list)
           (let* ((store-parameters (parse-store-lambda-list store-lambda-list))
                  (specialization-parameters (parse-specialization-lambda-list specialization-lambda-list))
                  (congruence (congruent-parameters-p store-parameters specialization-parameters)))
             (if expected
                 (is-true congruence
                          "congruent-parameters-p failed with store lambda list ~A and specialization lambda list ~A."
                          store-lambda-list
                          specialization-lambda-list)
                 (is-false congruence
                           "The specialized lambda list ~A should be congruent with the store lambda list ~A."
                           specialization-lambda-list
                           store-lambda-list)))))
    (macrolet ((true (store-lambda-list specialization-lambda-list)
                 `(do-trial t ',store-lambda-list ',specialization-lambda-list))
               (false (store-lambda-list specialization-lambda-list)
                 `(do-trial nil ',store-lambda-list ',specialization-lambda-list)))
      ;; Positional
      (true (a) (b))
      (false (a) ())
      (false (a) (&rest args))

      (true (a &optional b) (b c))
      (false (a &optional b) (b))
      (false (a &optional b) (a &optional b))

      (true (a &rest args) (a))
      (true (a &rest args) (a b))
      (true (a &rest args) (a &optional b))
      (false (a &rest args) ())

      ;; Keys
      (true (a &optional b &key c) (a b &key c))
      (true (a &optional b &key c) (a b &key c d))
      (true (&key c) (&key ((:c d))))
      (true (&key c) (&rest args &key c))
      (true (&rest args &key c) (&key c))
      (true (&key c &allow-other-keys) (&key c d))
      (false (a &key c d) (a &key d))
      (false (a &key c d) (a &key c))
      (false (a) (b &key))
      (false (a &key) (b))
      (false (&key c &allow-other-keys) (&key d))

      ;; Key Order
      (true (&key a b) (&key a b))
      (false (&key a b) (&key b a)))))

(test congruent-store-parameters
  (flet ((do-trial (expected lambda-list-1 lambda-list-2)
           (let* ((p1 (parse-store-lambda-list lambda-list-1))
                  (p2 (parse-store-lambda-list lambda-list-2))
                  (congruence-1 (congruent-parameters-p p1 p2))
                  (congruence-2 (congruent-parameters-p p2 p1)))
             (is (eql congruence-1 congruence-2) "The function ~W should be commutative with arguments ~W and ~W."
                 'congruent-parameters-p lambda-list-1 lambda-list-2)
             (if expected
                 (is-true congruence-1 "Store lambda list ~W should not be congruent with ~W." lambda-list-1 lambda-list-2)
                 (is-false congruence-1 "Store lambda list ~W should be congruent with ~W." lambda-list-1 lambda-list-2)))))
    (macrolet ((true (lambda-list-1 lambda-list-2)
                 `(do-trial t ',lambda-list-1 ',lambda-list-2))
               (false (lambda-list-1 lambda-list-2)
                 `(do-trial nil ',lambda-list-1 ',lambda-list-2)))
      ;; Required
      (true (a) (b))
      (true (a b) (c d))
      (false (a) ())
      (false (a) (b c))
      (false (a) (&optional b))
      (false (a) (&rest args))
      (false (a) (&key))
      (false (a) (&key hey))
      (false (a) (&key hey &allow-other-keys))

      ;; Optional
      (true (&optional a) (&optional b))
      (true (&optional a b) (&optional b c))
      (false (&optional c) ())
      (false (&optional c) (a))
      (false (&optional c) (a b))
      (false (&optional c) (&rest args))
      (false (&optional c) (&key))
      (false (&optional c) (&key d))
      (false (&optional c) (&key d &allow-other-keys))

      ;; Rest
      (true (&rest args) (&rest args))
      (true (&rest args) (&rest others))
      (false (&rest args) ())
      (false (&rest args) (a))
      (false (&rest args) (&optional a))
      (false (&rest args) (&key c))
      (false (&rest args) (&key c &allow-other-keys))

      ;; Keys
      (true (&key) (&key))
      (true (&key) (&key &allow-other-keys))
      (true (&key &allow-other-keys) (&key &allow-other-keys))
      (true (&key c) (&key c))
      (true (&key c) (&key ((:c d))))
      (true (&rest args &key c) (&rest others &key c))
      (true (&rest args &key c) (&key c))
      (false (&key c) (a))
      (false (&key c) (&optional a))
      (false (&key c) (&rest args))
      (false (&key c) (&key c d))
      (false (&key c) (&key))
      (false (&key c) (&key &allow-other-keys))

      ;; Complex
      (true (a b &optional c &rest args) (e f &optional g &rest others))
      (true (a &optional b &rest args &key f) (z &optional y &rest others &key ((:f x))))
      (false (a b &optional c &rest args) (e f &optional g &rest others &key &allow-other-keys))
      (false (a b &optional c &rest args) (e f &optional g))
      (false (a b &optional c &rest args) (e &optional g)))))


;;;; Conversions

(test ordinary-lambda-list
  (flet ((do-trial (store specialization expected)
           (let* ((store-parameters (parse-store-lambda-list store))
                  (specialization-parameters (parse-specialization-lambda-list specialization)))
             (is (equal expected (ordinary-lambda-list store-parameters specialization-parameterS))))))
    (macrolet ((trial (store specialization expected)
                 `(do-trial ',store ',specialization ',expected)))
      ;; Required
      (trial (a) (a) (a))
      (trial (a) ((a integer)) (a))
      (trial (a b) ((a integer) b) (a b))
      (trial (a b) (a (b integer)) (a b))
      ;; Optional
      (trial (a &optional b) (a b) (a b))
      (trial (a &optional b) ((a integer) b) (a b))
      (trial (a &optional b) (a (b integer)) (a b))
      (trial (a &optional (b t)) (a (b integer)) (a b))
      ;; Rest
      (trial (&rest args) (a) (a))
      (trial (&rest args) (a &optional b) (a &optional b))
      (trial (&rest args) (a &optional (b t b-p)) (a &optional (b t b-p)))
      (trial (a &rest args) (a b) (a b))
      (trial (a &optional b &rest args) ((a integer) (b integer) &optional c) (a b &optional c))
      (trial (a &optional b &rest args) ((a integer) (b integer) &optional (c t)) (a b &optional (c t)))
      (trial (a &optional b &rest args) ((a integer) (b integer) &optional (c t c-p)) (a b &optional (c t c-p)))
      (trial (a &optional b &rest args) ((a integer) (b integer) &optional (c t) &rest args) (a b &optional (c t) &rest args))
      ;; Keywords
      (trial (&key a &allow-other-keys) (&key (a integer) &allow-other-keys) (&key a &allow-other-keys))
      (trial (&key a) (&key (a integer a-p)) (&key (a nil a-p)))
      (trial (&key a &allow-other-keys) (&key (a integer) (b t b-p)) (&key a (b t b-p))))))

(test type-declarations
  (flet ((do-trial (store specialization expected)
           (let* ((store-parameters (parse-store-lambda-list store))
                  (specialization-parameters (parse-specialization-lambda-list specialization)))
             (is (equal expected (type-declarations store-parameters specialization-parameters))))))
    (macrolet ((trial (store specialization expected)
                 `(do-trial ',store ',specialization ',expected)))
      (trial (a) (a) ())
      (trial (a) ((a integer)) ((type integer a)))
      (trial (a b) ((a integer) b) ((type integer a)))
      (trial (&key a &allow-other-keys) (&key a) ())
      (trial (&key a &allow-other-keys) (&key (a integer)) ((type integer a)))
      (trial (&key a &allow-other-keys) (&key (a integer a-p) (b t b-p)) ((type integer a) (type (eql T) a-p))))))

(test make-value-completion-lambda-form
  (flet ((init-b ()
           1)
         (init-c ()
           2))
    (macrolet ((def (store-lambda-list)
                 (make-value-completion-lambda-form (parse-store-lambda-list store-lambda-list))))
      ;; keywords and allow-other-keys
      (let ((fn (funcall (def (a &optional (b (init-b)) &key (c (init-c)) &allow-other-keys))
                         (lambda (&rest args)
                           args))))
        (is (equal '(0 1 :c 2) (funcall fn 0)))
        (is (equal '(0 2 :c 2 :d 5) (funcall fn 0 2 :d 5)))
        (is (equal '(0 2 :c 4 :d 8 :c 4) (funcall fn 0 2 :d 8 :c 4)))
        (signals error (funcall fn)))

      ;; keywords
      (let ((fn (funcall (def (a &optional (b (init-b)) &key (c (init-c))))
                         (lambda (&rest args)
                           args))))
        (is (equal '(0 1 :c 2) (funcall fn 0)))
        (is (equal '(0 2 :c 4 :c 4) (funcall fn 0 2 :c 4)))
        (is (equal '(0 2 :c 2 :d 5 :allow-other-keys t) (funcall fn 0 2 :d 5 :allow-other-keys t)))
        (signals error (funcall fn 0 2 :d 5)))

      ;; Rest
      (let ((fn (funcall (def (a &optional (b (init-b)) &rest args))
                         (lambda (&rest args)
                           args))))
        (is (equal '(0 1) (funcall fn 0)))
        (is (equal '(0 1) (funcall fn 0 1)))
        (is (equal '(0 1 2) (funcall fn 0 1 2)))
        (signals error (funcall fn)))

      ;; Positional
      (let ((fn (funcall (def (a &optional (b (init-b)) (c (init-c))))
                         (lambda (&rest args)
                           args))))
        (is (equal '(0 1 2) (funcall fn 0)))
        (is (equal '(0 2 2) (funcall fn 0 2)))
        (is (equal '(0 2 4) (funcall fn 0 2 4)))
        (signals error (funcall fn)))

      ;; Dependents
      (let ((fn (funcall (def (a &optional (b (init-b)) (c b)))
                         (lambda (&rest args)
                           args))))
        (is (equal '(0 1 1) (funcall fn 0)))
        (is (equal '(0 2 2) (funcall fn 0 2)))
        (is (equal '(0 2 4) (funcall fn 0 2 4))))

      (let ((fn (funcall (def (a &optional (b (init-b)) &key (c b)))
                         (lambda (&rest args)
                           args))))
        (is (equal '(0 1 :c 1) (funcall fn 0)))
        (is (equal '(0 2 :c 2) (funcall fn 0 2)))
        (is (equal '(0 2 :c 4 :c 4) (funcall fn 0 2 :c 4))))

      ;; Supplied p vars
      (let ((fn (funcall (def (a &optional (b nil bp) (c bp)))
                         (lambda (&rest args)
                           args))))
        (is (equal '(1 nil nil) (funcall fn 1)))
        (is (equal '(1 2 t) (funcall fn 1 2)))
        (is (equal '(1 2 3) (funcall fn 1 2 3)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro let-type-completion-function ((name store-lambda-list) &body body &environment env)
    (let* ((parameters (parse-store-lambda-list store-lambda-list))
           (lambda-form (make-type-completion-lambda-form parameters env)))
      (alexandria:with-gensyms (completion-function fn)
        `(let* ((,completion-function ,lambda-form)
                (,fn (funcall ,completion-function (lambda (form env completed)
                                                     (declare (ignore form env))
                                                     completed))))
           (flet ((,name (form &optional environment)
                    (funcall ,fn form environment)))
             ,@body))))))

(test make-type-completion-lambda-form
  ;; Keywords
  (flet ((init-b ()
           1)
         (init-c ()
           2))
    (declare (ignorable (function init-b) (function init-c)))
    (let-type-completion-function (compute (a &optional (b (init-b)) (c b) &key (d 5) (e d)))
      (flet ((trial (expected input)
               (is (equal expected (compute input))
                   "The type completion function did not produce ~A for form ~A."
                   expected input)))
        (trial '((eql 1) t t :d (eql 5) :e t)
               '(test 1))
        (trial (list '(eql 1) (type-of "hey") t :d '(eql 5) :e t)
               '(test 1 "hey"))
        (trial (list '(eql 1) (type-of "hey") '(eql 5) :d '(eql 5) :e t)
               '(test 1 "hey" 5))
        (trial (list '(eql 1) (type-of "hey") '(eql 5) :d '(eql 10) :e t)
               '(test 1 "hey" 5 :d 10))
        (trial (list '(eql 1) (type-of "hey") '(eql 5) :d '(eql 5) :e '(eql 20))
               '(test 1 "hey" 5 :e 20))
        (trial (list '(eql 1) (type-of "hey") '(eql 5) :d '(eql 30) :e '(eql 20))
               '(test 1 "hey" 5 :e 20 :d 30))))

    ;; Rest
    (let-type-completion-function (compute (a &optional (b (init-b)) (c b) &rest args))
      (flet ((trial (expected input)
               (is (equal expected (compute input))
                   "The type completion function did not produce ~A for form ~A."
                   expected input)))
        (trial '((eql 1) t t)
               '(test 1))
        (trial (list '(eql 1) (type-of "hey") t)
               '(test 1 "hey"))
        (trial (list '(eql 1) (type-of "hey") '(eql 5))
               '(test 1 "hey" 5))
        (trial (list '(eql 1) (type-of "hey") '(eql 5) '(eql 10))
               '(test 1 "het" 5 10))))

    ;; Positional
    (let-type-completion-function (compute (a &optional (b (init-b)) (c (init-c))))
      (flet ((trial (expected input)
               (is (equal expected (compute input))
                   "The type completion function did not produce ~A for form ~A."
                   expected input)))
        (trial '((eql 1) t t)
               '(test 1))
        (trial (list (type-of "hey") t t)
               '(test "hey"))
        (trial (list (type-of "hey") '(eql 5) '(eql 1))
               '(test "hey" 5 1))))

    ;; &allow-other-keys
    (let-type-completion-function (compute (&key d &allow-other-keys))
      (flet ((trial (expected input)
               (is (equal expected (compute input))
                   "The type completion function did not produce ~A for form ~A."
                   expected input)))
        (trial '(:d null) '(test))
        (trial '(:d null) '(test :e 1))
        (trial '(:d (eql 1)) '(test :d 1 :f 2))))

    ;; :allow-other-keys t
    (let-type-completion-function (compute (&key d))
      (flet ((trial (expected input)
               (is (equal expected (compute input))
                   "The type completion function did not produce ~A for form ~A."
                   expected input)))
        (trial '(:d null) '(test))
        (trial '(:d null) '(test :allow-other-keys t :e 2))
        (trial '(:d (eql 1)) '(test :allow-other-keys t :d 1 :e 2))))

    ;; Function Declarations
    #+specialization-store.features:function-declarations
    (locally (declare (ftype (function () integer) init-b init-c))
      (let-type-completion-function (compute (&optional (b (init-b)) (c (init-c))))
        (flet ((trial (expected input)
                 (is (equal expected (compute input))
                     "The type completion function did not produce ~A for form ~A."
                     expected input)))
          (trial '(integer integer)
                 '(test)))))))

(test make-type-completion-lambda-form/errors
  ;; Invalid number of arguments
  (let-type-completion-function (compute (a))
    (signals error (compute '(test)))
    (signals error (compute '(test 1 2)))
    (finishes (compute '(test 1))))

  ;; Not specifying :allow-other-keys t
  (let-type-completion-function (compute (&key d))
    (signals error (compute '(test :e 1)))))
