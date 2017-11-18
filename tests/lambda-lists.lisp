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


;;;; parse-store-object-lambda-list

(test parse-store-object-lambda-list
  (flet ((trial (store-object-lambda-list)
           (finishes (parse-store-lambda-list store-object-lambda-list))
           (finishes (parse-store-object-lambda-list store-object-lambda-list))))
    (trial '(a))
    (trial '(a &optional (b a)))
    (trial '(a &optional (b 1)))
    (trial '(a &optional b (c a)))
    (trial '(a &optional (b (identity a))))
    (trial '(a &optional b (c b)))
    (trial '(a &optional b &key c))
    (trial '(a &optional b &key (c a) (d (1+ c))))
    (trial '(a &optional b &rest args &key (c (append a b args))))
    (trial '(a &rest args))
    (trial '(a &key (b a)))
    (trial '(a &key b (c b)))))

(test parse-store-object-lambda-list/invalid
  (flet ((trial (store-object-lambda-list)
           (signals parse-store-lambda-list-error
             (parse-store-object-lambda-list store-object-lambda-list))))
    (trial '(a &optional (b c) c))
    (trial '(a &key (b (1+ b))))
    (trial '(a &rest args &key (b (append a args c))))
    (trial '(a &optional b &key (c (identity c))))))

;;;; parse-specialization-lambda-list

(test parse-specialization-lambda-list
  (flet ((do-trial (specialization-lambda-list req rest key? keys allow-other-keys?)
           (let ((specialization-parameters (parse-specialization-lambda-list specialization-lambda-list)))
             (is-true (typep specialization-parameters 'specialization-parameters))
             (is (and (equal req (mapcar #'parameter-lambda-list-specification (required-parameters specialization-parameters)))
                      (equal rest (when (rest-parameter-p specialization-parameters)
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
          (rest '((nil . nil)
                  ((&rest args) . args)
                  ((&rest (args integer)) . (args integer))))
          (keys '((nil nil nil nil)
                  ((&key) nil t nil)
                  ((&key m) (m) t nil)
                  ((&key m (n 2)) (m (n 2)) t nil)
                  ((&key (o 5) &allow-other-keys) ((o 5)) t t)
                  ((&key (m 10 m-p)) ((m 10 m-p)) t nil)
                  ((&key ((:k m) 10 m-p) &allow-other-keys) (((:k m) 10 m-p)) t t))))
      (loop for (r-list . r) in required do
        (loop for (rest-list . rest-var) in rest do
          (cond ((and rest-var (listp rest-var))
                 (let ((lambda-list (append r-list rest-list)))
                   (do-trial lambda-list r rest-var nil nil nil)))
                (t
                 (loop for (keys-list k keys? allow-other-keys?) in keys do
                   (let ((lambda-list (append r-list rest-list keys-list)))
                     (do-trial lambda-list r rest-var keys? k allow-other-keys?))))))))))

(test parse-specialization-lambda-list/invalid-specialization-lambda-lists
  (flet ((trial (specialization-lambda-list)
           (signals (parse-specialization-lambda-list-error
                     "Failed to signal ~A for lambda list ~A." ('parse-specialization-lambda-list-error specialization-lambda-list))
             (parse-specialization-lambda-list specialization-lambda-list))))
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
    (trial '(&optional a))
    (trial '(a &optional b))
    (trial '(a &optional b &key c))
    (trial '(a &optional b &rest args))
    (trial '(&optional (b t bp) (c t bp)))
    (trial '(&optional nil))
    (trial '(&optional (nil nil)))
    (trial '(&rest nil))
    (trial '(&rest (args)))
    (trial '(&rest (args t integer)))
    (trial '(&rest (args integer) &key blah))
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
    (trial '(a &rest (a t)))
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

      (true (a &rest args) (a))
      (true (a &rest args) (a b))
      (false (a &rest args) ())
      (false (a &rest args) (a &key b))

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
      (trial (&rest args) (a b) (a b))
      (trial (a &rest args) (a b) (a b))
      (trial (a &optional b &rest args) ((a integer) (b integer)) (a b))
      (trial (a &optional b &rest args) ((a integer) (b integer) c) (a b c))
      (trial (a &optional b &rest args) ((a integer) (b integer) c &rest args) (a b c &rest args))
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
      (trial (&key a &allow-other-keys) (&key (a integer)) ((type (or null integer) a)))
      (trial (&key a &allow-other-keys) (&key (a integer a-p) (b t b-p)) ((type (or null integer) a) (type (eql T) a-p))))))

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
                (,fn (funcall ,completion-function
                              (lambda (&rest args)
                                args))))
           (flet ((,name (&rest args)
                    (apply ,fn args)))
             ,@body))))))

(test make-type-completion-lambda-form
  (flet ((init-b ()
           1)
         (init-c ()
           2)
         (trial-helper (fn expected input)
           (let* ((actual (apply fn input)))
             (is (equal expected actual)
                 "~A: Expected ~A to be equal to ~A."
                 input actual expected))))
    (declare (ignorable (function init-b) (function init-c)))
    (symbol-macrolet ((hey "hey"))
      ;; Keyword arguments (No allow other keywords)
      (let-type-completion-function (compute (a &optional (b (init-b)) (c b) &key (d 5) (e d)))
        (flet ((trial (expected input)
                 (trial-helper #'compute expected input)))
          (trial '((eql 1) t t :d (eql 5) :e t)
                 '((eql 1)))
          (trial '(integer single-float t :d (eql 5) :e t)
                 '(integer single-float))
          (trial '(integer integer float :d (eql 5) :e t)
                 '(integer integer float))
          (trial '(bit bit bit :d integer :e t)
                 '(bit bit bit :d integer))
          (trial '(bit bit bit :d (eql 5) :e float)
                 '(bit bit bit :e float))
          (trial '(bit bit bit :d bit :e bit)
                 '(bit bit bit :e bit :d bit))

          (trial '(bit bit bit :d (eql 5) :e t)
                 '(bit bit bit :garbage t :allow-other-keys t))
          (signals error (compute t t t :garbage t))
          (signals error (compute))))

      ;; Keyword arguments (allow other keywords)
      (let-type-completion-function (compute (&key (d 1) &allow-other-keys))
        (flet ((trial (expected input)
                 (trial-helper #'compute expected input)))
          (trial '(:d (eql 1))
                 '())
          (trial '(:d (eql 1))
                 '(:e t))
          (trial '(:d float)
                 '(:e bit :d float))))

      ;; Rest arguments
      (let-type-completion-function (compute (a &optional (b (the bit (init-b))) &rest args))
        (flet ((trial (expected input)
                 (trial-helper #'compute expected input)))
          (trial '(bit bit)
                 '(bit))
          (trial '(bit float)
                 '(bit float))
          (trial '(bit float bit)
                 '(bit float bit))
          (trial '(bit float bit integer)
                 '(bit float bit integer))
          (signals error (compute))))

      ;; Positional
      (let-type-completion-function (compute (a b &optional (c (the number (init-c)))))
        (flet ((trial (expected input)
                 (trial-helper #'compute expected input)))
          (trial '(bit real number)
                 '(bit real))
          (trial '(real bit complex)
                 '(real bit complex))
          (signals error (compute))
          (signals error (compute t))
          (signals error (compute t t t t))))

      ;; Function Declarations
      #+specialization-store.features:function-declarations
      (locally (declare (ftype (function () integer) init-b init-c))
        (let-type-completion-function (compute (&optional (b (init-b)) (c (init-c))))
          (flet ((trial (expected input)
                   (trial-helper #'compute expected input)))
            (trial '(integer integer)
                   '())))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro let-form-types-function ((name store-lambda-list) &body body)
    (let* ((parameters (parse-store-lambda-list store-lambda-list))
           (lambda-form (make-form-types-lambda-form parameters)))
      (alexandria:with-gensyms (completion-function fn)
        `(let* ((,completion-function ,lambda-form)
                (,fn (funcall ,completion-function
                              (lambda (&rest args)
                                args))))
           (flet ((,name (form &optional env)
                    (funcall ,fn form env)))
             ,@body))))))

(test make-form-types-function
  (flet ((init-b ()
           1)
         (trial-helper (fn expected input)
           (let* ((actual (funcall fn input)))
             (is (equal expected actual)
                 "~A: Expected ~A to be equal to ~A."
                 input actual expected))))
    (declare (ignorable (function init-b)))
    (symbol-macrolet ((hey "hey"))
      (let* ((hey-type `(eql ,hey)))
        ;; Keyword arguments (No allow other keywords)
        (let-form-types-function (compute (a &optional (b (init-b)) (c b) &key (d 5) (e d)))
          (flet ((trial (expected input)
                   (trial-helper #'compute expected input)))
            (trial '((eql 1))
                   '(test 1))
            (trial `((eql 1) ,hey-type)
                   `(test 1 ,hey))
            (trial `(,hey-type ,hey-type (eql 1))
                   `(test ,hey ,hey 1))
            (trial `(,hey-type ,hey-type ,hey-type :d (eql 1))
                   `(test ,hey ,hey ,hey :d 1))
            (trial `(,hey-type ,hey-type ,hey-type :e (eql 1))
                   `(test ,hey ,hey ,hey :e 1))
            (trial `(,hey-type ,hey-type ,hey-type :d (eql 1) :e (eql 2))
                   `(test ,hey ,hey ,hey :e 2 :d 1))
            (trial `(,hey-type ,hey-type ,hey-type)
                   `(test ,hey ,hey ,hey :garbage 1 :allow-other-keys t))
            (signals error (compute '(test 1 2 3 :f 4)))
            (signals error (compute '(test)))
            (signals error (compute '(test 1 2 3 :d)))))

        ;; Keyword arguments (allow other keys)
        (let-form-types-function (compute (&key a b &allow-other-keys))
          (flet ((trial (expected input)
                   (trial-helper #'compute expected input)))
            (trial '()
                   '(test))
            (trial '()
                   '(test :c 5))))

        ;; Rest arguments
        (let-form-types-function (compute (a &optional b &rest args))
          (flet ((trial (expected input)
                   (trial-helper #'compute expected input)))
            (trial `(,hey-type)
                   `(test ,hey))
            (trial `(,hey-type (eql 1))
                   `(test ,hey 1))
            (trial `(,hey-type (eql 1) (eql 2))
                   `(test ,hey 1 2))
            (trial `(,hey-type (eql 1) (eql 2) (eql 3))
                   `(test ,hey 1 2 3))
            (signals error (compute '(test)))))

        ;; Positional
        (let-form-types-function (compute (a &optional b))
          (flet ((trial (expected input)
                   (trial-helper #'compute expected input)))
            (trial `(,hey-type)
                   `(test ,hey))
            (trial `((eql 1) (eql 2))
                   `(test 1 2))
            (signals error (compute '(test)))
            (signals error (compute '(test 1 2 3)))))))))

(defvar *rewrite-order* nil)

(defmacro ordered-form (var form)
  `(progn
     (alexandria:appendf *rewrite-order* (list ',var))
     ,form))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-rewrite-order ((name) &body body)
    `(let ((*rewrite-order* nil))
       (symbol-macrolet ((,name *rewrite-order*))
         ,@body))))

(test rewrite-store-function-form/required
  (flet ((%example (a b c)
           (+ a b c)))
    (macrolet ((example (&rest args &environment env)
                 (let* ((parameters (parse-store-object-lambda-list '(a b c)))
                        (form `(%example ,@args)))
                   (destructuring-bind (fn new-form) (rewrite-store-function-form parameters form env)
                     (funcall fn new-form env)))))
      (with-rewrite-order (r)
        (is (= 6 (example (ordered-form a 1)
                          (ordered-form b 2)
                          (ordered-form c 3))))
        (is (equal '(a b c) r)))
      (with-rewrite-order (r)
        (let* ((x 1)
               (y 2)
               (z 4))
          (is (= 7 (example (ordered-form x x)
                            (ordered-form y y)
                            (ordered-form z z))))
          (is (equal '(x y z) r)))))))

(test rewrite-store-function-form/optional
  (flet ((%example (a b)
           (+ a b)))
    (macrolet ((example (&rest args &environment env)
                 (let* ((form `(%example ,@args))
                        (parameters (parse-store-object-lambda-list '(a &optional (b (ordered-form b (1+ a)))))))
                   (destructuring-bind (fn new-form) (rewrite-store-function-form parameters form env)
                     (funcall fn new-form env)))))
      (with-rewrite-order (r)
        (is (= 3 (example 1)))
        (is (equal '(b) r)))
      (with-rewrite-order (r)
        (let ((x 1))
          (is (= 3 (example x)))
          (is (equal '(b) r))))
      (with-rewrite-order (r)
        (let ((x 1)
              (y 2))
          (is (= 3 (example (ordered-form x x) (ordered-form y y))))
          (is (equal '(x y) r)))))))

(test rewrite-store-function-form/rest
  (flet ((%example (a b &rest args)
           (reduce #'+ args :initial-value (+ a b))))
    (macrolet ((example (&rest args &environment env)
                 (let* ((form `(%example ,@args))
                        (parameters (parse-store-object-lambda-list '(a &optional (b (ordered-form b (1+ a))) &rest args))))
                   (destructuring-bind (fn new-form) (rewrite-store-function-form parameters form env)
                     (funcall fn new-form env)))))
      ;; Constants
      (with-rewrite-order (r)
        (is (= 3 (example (ordered-form x 1))))
        (is (equal '(x b) r)))
      ;; Variables
      (with-rewrite-order (r)
        (let* ((x 1))
          (is (= 3 (example (ordered-form x x))))
          (is (equal '(x b) r))))
      ;; Constants
      (with-rewrite-order (r)
        (is (= 10 (example (ordered-form w 1) (ordered-form x 2) (ordered-form y 3) (ordered-form z 4))))
        (is (equal '(w x y z) r)))
      ;; Variables
      (with-rewrite-order (r)
        (let* ((w 1) (x 2) (y 3) (z 4))
          (is (= 10 (example (ordered-form w w) (ordered-form x x) (ordered-form y y) (ordered-form z z))))
          (is (equal '(w x y z) r)))))))

(test rewrite-store-function-form/keywords/sans-rest
  (flet ((%example (a b &key c d &allow-other-keys)
           (+ a b c d)))
    (macrolet ((example (&rest args &environment env)
                 (let* ((form `(%example ,@args))
                        (parameters (parse-store-object-lambda-list `(a &optional (b (ordered-form b (1+ a)))
                                                                        &key (c (ordered-form c (+ a b)))
                                                                             (d (ordered-form d (+ a c)))))))
                   (destructuring-bind (fn new-form) (rewrite-store-function-form parameters form env)
                     (funcall fn new-form env)))))
      ;; constants
      (with-rewrite-order (r)
        (is (= 10 (example (ordered-form x 1) (ordered-form y 2))))
        (is (equal '(x y c d) r)))
      ;; variables
      (with-rewrite-order (r)
        (let* ((x 1) (y 2))
          (is (= 10 (example (ordered-form x x) (ordered-form y y))))
          (is (equal '(x y c d) r))))
      ;; constants
      (with-rewrite-order (r)
        (is (= 9 (example (ordered-form x 1) (ordered-form y 2) :d (ordered-form z 3))))
        (is (equal '(x y z c) r)))
      ;; variables
      (with-rewrite-order (r)
        (let* ((x 1) (y 2) (z 3))
          (is (= 9 (example (ordered-form x x) (ordered-form y y) :d (ordered-form z z))))
          (is (equal '(x y z c) r))))
      ;; constants
      (with-rewrite-order (r)
        (is (= 10 (example (ordered-form v 1) (ordered-form w 2) :d (ordered-form x 3)
                                                                 :c (ordered-form y 4)
                                                                 :f (ordered-form z 5))))
        (is (equal '(v w x y z) r)))
      ;; Variables
      (with-rewrite-order (r)
        (let* ((v 1) (w 2) (x 3) (y 4) (z 5))
          (is (= 10 (example (ordered-form v v) (ordered-form w w) :d (ordered-form x x)
                                                                   :c (ordered-form y y)
                                                                   :f (ordered-form z z))))
          (is (equal '(v w x y z) r))))

      ;; Value precedents
      (with-rewrite-order (r)
        (is (= 12 (example 1 2 :c 3 :c 4 :d 6 :d 5)))
        (is (equal nil r))))))

(test rewrite-store-function-form/keywords/with-rest
  (flet ((%example (a b &key c d &allow-other-keys)
           (+ a b c d)))
    (macrolet ((example (&rest args &environment env)
                 (let* ((form `(%example ,@args))
                        (parameters (parse-store-object-lambda-list '(a b
                                                                      &rest args
                                                                      &key (c (ordered-form c (1+ b)))
                                                                           (d (ordered-form d (length args)))))))
                   (destructuring-bind (fn new-form) (rewrite-store-function-form parameters form env)
                     (funcall fn new-form env)))))
      ;; Constants
      (with-rewrite-order (r)
        (is (= 6 (example 1 2)))
        (is (equal '(c d) r)))
      ;; Variables
      (with-rewrite-order (r)
        (let* ((x 1) (y 2))
          (is (= 6 (example x y)))
          (is (equal '(c d) r))))
      ;; Constants
      (with-rewrite-order (r)
        (is (= 8 (example (ordered-form x 1) (ordered-form y 2) :c (ordered-form z 3))))
        (is (equal '(x y z d) r)))
      ;; Variables
      (with-rewrite-order (r)
        (let* ((x 1) (y 2) (z 3))
          (is (= 8 (example (ordered-form x x) (ordered-form y y) :c (ordered-form z z))))
          (is (equal '(x y z d) r))))
      ;; Other keywords
      (with-rewrite-order (r)
        (is (= 10 (example 1 2 :c 3 :f 4)))
        (is (equal '(d) r)))
      ;; Value precedents
      (with-rewrite-order (r)
        (is (= 17 (example 1 2 :c 4 :c 3 :d 10 :d 9)))
        (is (equal nil r))))))
