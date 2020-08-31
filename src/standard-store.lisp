(in-package "SPECIALIZATION-STORE.STANDARD-STORE")


;;;; Standard Store Class

(defclass standard-store-class (specialization-store.mop:funcallable-standard-class)
  ())

(defmethod specialization-store.mop:validate-superclass ((class standard-store-class) (superclass specialization-store.mop:funcallable-standard-class))
  t)

;;;; Standard Specialization Class

(defclass standard-specialization-class (specialization-store.mop:funcallable-standard-class)
  ())

(defmethod specialization-store.mop:validate-superclass ((class standard-specialization-class) (superclass specialization-store.mop:funcallable-standard-class))
  t)

;;;; Standard Store

(defgeneric store-parameters (standard-store))

(defgeneric compute-dispatch-lambda-forms (store))
(defgeneric compute-dispatch-functions (store))
(defgeneric update-dispatch-functions (store))
(defgeneric clear-dispatch-functions (store))

(defclass standard-store ()
  ((name :initarg :name
         :reader store-name)
   (lambda-list :initarg :lambda-list
                :initform (error "A store lambda list must be supplied.")
                :reader store-lambda-list)
   (parameters :initarg :parameters
               :reader store-parameters)
   (documentation :initarg :documentation
                  :accessor store-documentation)
   (specializations :initarg :specializations
                    :reader store-specializations)
   (specialization-class :initarg :specialization-class
                         :reader store-specialization-class)
   (%value-completion-function)
   (%type-completion-function)
   (%form-types-function)
   (%runtime-function)
   (%compile-time-function))
  (:metaclass standard-store-class)
  (:default-initargs
   :name nil
   :documentation nil
   :specializations nil
   :specialization-class (find-class 'standard-specialization)))

(defmethod print-object ((object standard-store) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((name (store-name object)))
      (when name
        (format stream "~W " name)))
    (princ (store-lambda-list object) stream)))

(defgeneric specialization-parameters (standard-specialization))

(defclass standard-specialization ()
  ((name :initarg :name
         :reader specialization-name)
   (lambda-list :initarg :lambda-list
                :reader specialization-lambda-list)
   (value-type :initarg :value-type
               :reader specialization-value-type)
   (parameters :initarg :parameters
               :reader specialization-parameters)
   (documentation :initarg :documentation
                  :accessor specialization-documentation)
   (function :initarg :function
             :reader specialization-function)
   (expand-function :initarg :expand-function
                    :reader specialization-expand-function))
  (:metaclass standard-specialization-class)
  (:default-initargs
   :name nil
   :expand-function nil
   :documentation nil))

(defmethod print-object ((object standard-specialization) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (specialization-lambda-list object) stream)))

;;;; Standard Store Implementation (Object Layer)

(defun make-default-value-completion-function (store-parameters)
  (compile nil (specialization-store.lambda-lists:make-value-completion-lambda-form store-parameters)))

(defun make-default-type-completion-function (store-parameters)
  (compile nil (specialization-store.lambda-lists:make-type-completion-lambda-form store-parameters nil)))

(defun make-default-form-types-function (store-parameters)
  (compile nil (specialization-store.lambda-lists:make-form-types-lambda-form store-parameters)))

(defmethod initialize-instance :after ((instance standard-store)
                                       &key
                                         (lambda-list nil lambda-list-p)
                                         &allow-other-keys)
  (assert lambda-list-p)
  (let ((new-parameters (specialization-store.lambda-lists:parse-store-lambda-list lambda-list)))
    (with-slots (parameters) instance
      (setf parameters new-parameters))
    (with-slots (%value-completion-function) instance
      (setf %value-completion-function (make-default-value-completion-function new-parameters)))
    (with-slots (%type-completion-function) instance
      (setf %type-completion-function (make-default-type-completion-function new-parameters)))
    (with-slots (%form-types-function) instance
      (setf %form-types-function (make-default-form-types-function new-parameters))))
  (clear-dispatch-functions instance))

(defmethod reinitialize-instance :after ((instance standard-store)
                                         &key
                                           ((:name new-name) nil new-name-p)
                                           ((:lambda-list new-lambda-list) nil new-lambda-list-p)
                                           &allow-other-keys)
  (when new-name-p
    (unless (eql (store-name instance) new-name)
      (error "Cannot change names of standard store objects.")))

  (when new-lambda-list-p
    (let* ((old-parameters (store-parameters instance))
           (new-parameters (specialization-store.lambda-lists:parse-store-lambda-list new-lambda-list))
           (congruent? (specialization-store.lambda-lists:congruent-parameters-p old-parameters new-parameters)))
      (cond ((or congruent? (null (store-specializations instance)))
             (with-slots (parameters) instance
               (setf parameters new-parameters))
             (with-slots (%value-completion-function) instance
               (setf %value-completion-function (make-default-value-completion-function new-parameters)))
             (with-slots (%type-completion-function) instance
               (setf %type-completion-function (make-default-type-completion-function new-parameters)))
             (with-slots (%form-types-function) instance
               (setf %form-types-function (make-default-form-types-function new-parameters)))
             (clear-dispatch-functions instance))
            (t
             (error 'simple-store-error
                    :store instance
                    :message (format nil "Unable to change store object lambda list.")))))))

(defmethod funcall-store ((store standard-store) &rest args)
  (with-slots (%runtime-function) store
    (apply %runtime-function args)))

(defmethod apply-store ((store standard-store) &rest args)
  (with-slots (%runtime-function) store
    (apply #'apply %runtime-function args)))

(defmethod expand-store ((store standard-store) form &optional env)
  (with-slots (%compile-time-function) store
    (funcall %compile-time-function form env)))

(defmethod add-specialization ((store standard-store) (specialization standard-specialization))
  (unless (specialization-store.lambda-lists:congruent-parameters-p (store-parameters store) (specialization-parameters specialization))
    (error 'incongruent-specialization-error :store store :specialization specialization))
  (loop
     for sublist on (store-specializations store)
     for existing-specialization = (car sublist)
     when (specialization-equal store specialization existing-specialization)
     return (progn (setf (car sublist) specialization)
                   nil)
     finally
       (alexandria:appendf (slot-value store 'specializations) (list specialization)))
  (clear-dispatch-functions store)
  store)

(defmethod remove-specialization ((store standard-store) (specialization standard-specialization))
  (alexandria:deletef (slot-value store 'specializations) specialization
                      :test #'(lambda (a b)
                                (specialization-equal store a b)))
  (clear-dispatch-functions store)
  store)

(defmethod (setf store-specializations) (value (store standard-store))
  (setf (slot-value store 'specializations) nil)
  (map nil #'(lambda (specialization)
               (add-specialization store specialization))
       value))

(defmethod specialization-equal ((store standard-store) (a standard-specialization) (b standard-specialization))
  (let* ((parameters-a (specialization-parameters a))
         (parameters-b (specialization-parameters b))
         (parameters (store-parameters store)))
    (labels ((compare/value (keys object)
               (reduce #'funcall keys :initial-value object :from-end t))
             (compare (test &rest keys)
               (funcall test (compare/value keys parameters-a) (compare/value keys parameters-b)))
             (ensure-key (match keyword specialization)
               (assert match nil "Unable to find keyword argument specification ~W in specialization ~W." keyword specialization)))
      (and (compare #'= #'length #'specialization-store.lambda-lists:required-parameters)
           (compare #'= #'length #'specialization-store.lambda-lists:optional-parameters)
           (compare #'eql #'specialization-store.lambda-lists:keyword-parameters-p)
           (let* ((rest-a (specialization-store.lambda-lists:rest-parameter parameters-a))
                  (rest-b (specialization-store.lambda-lists:rest-parameter parameters-b)))
             (or (and (null rest-a) (null rest-b))
                 (and rest-a rest-b
                      (alexandria:type= (specialization-store.lambda-lists:parameter-each-type rest-a)
                                        (specialization-store.lambda-lists:parameter-each-type rest-b)))))
           (loop
              for a in (specialization-store.lambda-lists:required-parameters parameters-a)
              for b in (specialization-store.lambda-lists:required-parameters parameters-b)
              always
              (alexandria:type= (specialization-store.lambda-lists:parameter-type a)
                                (specialization-store.lambda-lists:parameter-type b)))
           (loop
              with keys-a = (specialization-store.lambda-lists:keyword-parameters parameters-a)
              with keys-b = (specialization-store.lambda-lists:keyword-parameters parameters-b)
              for st-keyword in (specialization-store.lambda-lists:keyword-parameters parameters)
              for keyword = (specialization-store.lambda-lists:parameter-keyword st-keyword)
              for key-a = (find keyword keys-a :key #'specialization-store.lambda-lists:parameter-keyword)
              for key-b = (find keyword keys-b :key #'specialization-store.lambda-lists:parameter-keyword)
              for type-a = (specialization-store.lambda-lists:parameter-type key-a)
              for type-b = (specialization-store.lambda-lists:parameter-type key-b)
              do
                (ensure-key key-a keyword a)
                (ensure-key key-b keyword b)
              always
                (alexandria:type= type-a type-b))))))

;;;; Standard Specialization Implementation (Object Layer)

(defmethod shared-initialize :after ((instance standard-specialization) slot-names &key (lambda-list nil lambda-list-p))
  (declare (ignore lambda-list))
  (let* ((initialisingp (eql slot-names t))
         (reinitialisingp (eql slot-names nil)))
    (when initialisingp
      (setf (slot-value instance 'parameters) (specialization-store.lambda-lists:parse-specialization-lambda-list (specialization-lambda-list instance))))

    (when (and reinitialisingp lambda-list-p)
      (error "Cannot reinitialize the lambda list of standard specialization."))

    (when (slot-boundp instance 'function)
      (with-slots (function) instance
        (specialization-store.mop:set-funcallable-instance-function instance function)))))

;;;; Standard Store Implementation (Glue Layer)

(defmethod make-store-unbound ((store standard-store))
  (map nil #'(lambda (specialization)
               (let ((specialization-name (specialization-name specialization)))
                 (when specialization-name
                   (fmakunbound specialization-name)
                   (setf (compiler-macro-function specialization-name) nil))))
       (store-specializations store))
  (values))

(defmethod ensure-store-using-object ((class standard-store-class) store-name store-lambda-list
                                      &rest args
                                      &key
                                        specialization-class documentation
                                        &allow-other-keys)
  (declare (ignore specialization-class documentation))
  (apply #'make-instance 'standard-store
         :name store-name
         :lambda-list store-lambda-list
         args))

(defmethod ensure-store-using-object ((instance standard-store) store-name store-lambda-list
                                      &rest args
                                      &key
                                        specialization-class documentation
                                      &allow-other-keys)
  (declare (ignore specialization-class documentation))
  (apply #'reinitialize-instance instance
         :name store-name
         :lambda-list store-lambda-list
         args))

(defmethod ensure-specialization-using-object ((store standard-store) specialized-lambda-list value-type function
                                               &rest args &key name expand-function &allow-other-keys)
  (let* ((specialization-expand-function (or expand-function
                                             (when name
                                               (lambda (form env)
                                                 (declare (ignore env))
                                                 (cons name
                                                       (if (and (listp form) (eql (first form) 'funcall))
                                                           (cddr form)
                                                           (cdr form)))))))
         (specialization (apply #'make-instance (store-specialization-class store)
                               :lambda-list specialized-lambda-list
                               :value-type value-type
                               :function function
                               :expand-function specialization-expand-function
                               args)))
    (add-specialization store specialization)
    (when name
      (setf (fdefinition name) function))
    (when (and name expand-function)
      (setf (compiler-macro-function name) expand-function))
    specialization))


;;;; Standard Store (Syntax Layer)

(defmethod defstore-using-class ((class standard-store-class) name store-lambda-list
                                 &rest args
                                 &key
                                   environment
                                 &allow-other-keys)
  (alexandria:remove-from-plistf args :environment)
  (let* ((parameters (specialization-store.lambda-lists:parse-store-lambda-list store-lambda-list)))
    (destructuring-bind (new-parameters globals) (specialization-store.lambda-lists:parameter-init-forms-as-global-functions name parameters environment)
      `(progn
         ,@globals
         (ensure-store ',name ',(specialization-store.lambda-lists:original-lambda-list new-parameters)
                       ,@args)))))

(defun %augment-body (store-parameters specialization-parameters value-type body)
  (multiple-value-bind (forms declarations documentation) (alexandria:parse-body body :documentation t)
    (values (append declarations
                    (list `(declare ,@(specialization-store.lambda-lists:type-declarations store-parameters specialization-parameters))
                          `(the ,value-type
                                (progn
                                  ,@forms))))
            documentation)))

(defmethod defspecialization-using-object ((store standard-store) specialized-lambda-list value-type body
                                           &rest args &key name environment inline &allow-other-keys)
  (declare (ignore environment))
  (alexandria:remove-from-plistf args :name :environment :inline)
  (let* ((store-parameters (store-parameters store))
         (parameters (specialization-store.lambda-lists:parse-specialization-lambda-list specialized-lambda-list)))
    (multiple-value-bind (body documentation) (%augment-body store-parameters parameters value-type body)
      (let* ((lambda-list (specialization-store.lambda-lists:ordinary-lambda-list store-parameters parameters))
             (inlined-function `(lambda ,lambda-list
                                  ,@body))
             (function (if name
                           `(function ,name)
                           inlined-function))
             (inlined-expand-function (when inline
                                        `(compiler-macro-lambda (&rest args)
                                           (list* ',inlined-function
                                                  args))))
             (named-expand-function (when name
                                      `(compiler-macro-lambda (&rest args)
                                         (list* ',name args))))
             (expand-function (cond ((and name inline)
                                     inlined-expand-function)
                                    (name
                                     named-expand-function)
                                    (inline
                                     inlined-expand-function)))
             (specialization-class-name (class-name (store-specialization-class store))))
        `(progn
           ,(when name
              `(defun ,name ,lambda-list
                 ,@body))
           ,(when (and name inline)
              `(setf (compiler-macro-function ',name) ,inlined-expand-function))
           ,(when name
              `(proclaim '(ftype ,(specialization-store.lambda-lists:function-type store-parameters parameters value-type) ,name)))
           (add-specialization (find-store ',(store-name store))
                               (make-instance ',specialization-class-name
                                              :name ',name
                                              :lambda-list ',specialized-lambda-list
                                              :value-type ',value-type
                                              :function ,function
                                              :expand-function ,expand-function
                                              :documentation ,documentation
                                              ,@args)))))))

(defmethod define-specialization-using-object ((store standard-store) specialized-lambda-list value-type
                                               &rest args &key name environment function expand-function &allow-other-keys)
  (declare (ignore environment))
  (unless function
    (error "DEFINE-SPECIALIZATION-USING-OBJECT requres a function argument."))
  (alexandria:remove-from-plistf args :name :environment :function :expand-function)
  (destructuring-bind (&key (inline nil inlinep) &allow-other-keys) args
    (declare (ignore inline))
    (when inlinep
      (warn "Inline option is not supported inside DEFINE-SPECIALIZATION.")))

  (let* ((sp-function (if name
                          `(function ,name)
                          function))
         (sp-expand-function (cond ((and name expand-function)
                                    `(compiler-macro-function ',name))
                                   (name
                                    `(compiler-macro-lambda (&rest args)
                                       (cons ',name args)))
                                   (t
                                    expand-function)))
         (specialization-class-name (class-name (store-specialization-class store)))
         (store-parameters (store-parameters store))
         (parameters (specialization-store.lambda-lists:parse-specialization-lambda-list specialized-lambda-list)))
    `(progn
       ,(when name
          `(setf (fdefinition ',name) ,function))
       ,(when (and name expand-function)
          `(setf (compiler-macro-function ',name) ,expand-function))
       ,(when name
          `(proclaim '(ftype ,(specialization-store.lambda-lists:function-type store-parameters parameters value-type) ,name)))
       (add-specialization (find-store ',(store-name store))
                           (make-instance ',specialization-class-name
                                          :name ',name
                                          :lambda-list ',specialized-lambda-list
                                          :value-type ',value-type
                                          :function ,sp-function
                                          :expand-function ,sp-expand-function
                                          ,@args)))))

;;;; Dispatch Functions

(defmethod clear-dispatch-functions ((store standard-store))
  (with-slots (%runtime-function %compile-time-function) store
    (labels ((update-runtime (&rest args)
               (update-dispatch-functions store)
               (apply %runtime-function args))
             (update-compile-time (form &optional env)
               (update-dispatch-functions store)
               (funcall %compile-time-function form env)))
      (let* ((initial-runtime-function (with-slots (%value-completion-function) store
                                         (funcall %value-completion-function #'update-runtime))))
        (setf %runtime-function initial-runtime-function
              %compile-time-function #'update-compile-time)
        (specialization-store.mop:set-funcallable-instance-function store initial-runtime-function)))))

(defmethod update-dispatch-functions ((store standard-store))
  (with-slots (%runtime-function %compile-time-function) store
    (with-slots (%value-completion-function %type-completion-function %form-types-function) store
      (labels ((cascade (compile-time)
                 (let* ((type-fn (funcall %form-types-function
                                          (funcall %type-completion-function
                                                   (lambda (&rest args)
                                                     args)))))
                   (lambda (form env)
                     (let* ((completed-types (funcall type-fn form env)))
                       (destructuring-bind (lexical-fn rewritten-form) (specialization-store.lambda-lists:rewrite-store-function-form (store-parameters store) form env)
                         (let* ((new-form (funcall compile-time rewritten-form env completed-types)))
                           (if (eql new-form rewritten-form)
                               form
                               (funcall lexical-fn new-form env)))))))))
        (destructuring-bind (runtime compile-time) (compute-dispatch-functions store)
          (setf %runtime-function (funcall %value-completion-function runtime)
                %compile-time-function (cascade compile-time))
          (specialization-store.mop:set-funcallable-instance-function store %runtime-function)))))
  (values))

;;;; Dispatch function environment
(defclass function-environment ()
  ((store :initarg :store)
   (fail :initarg :fail))
  (:default-initargs
   :fail (gensym "FAIL")))

(defclass value-function-environment (function-environment)
  ())

(defclass type-function-environment (function-environment)
  ((form :initarg :form)
   (environment :initarg :environment)
   (completed-types :initarg :completed-types))
  (:default-initargs
   :form (gensym "FORM")
   :environment (gensym "ENV")
   :completed-types (gensym "COMPLETED-TYPES")))

(defun make-function-environment (store code-type)
  (ecase code-type
    (:value (make-instance 'value-function-environment :store store))
    (:type (make-instance 'type-function-environment :store store))))

;;;; Destructuring environment
(defclass positional-environment ()
  ((positional :initarg :positional)))

(defclass keywords-environment (positional-environment)
  ((keywords :initarg :keywords)
   (args :initarg :args)
   (allow-others-p :initarg :allow-others-p))
  (:default-initargs
   :args (gensym "ARGS")))

(defclass variable-environment (positional-environment)
  ((argument-count :initarg :argument-count)
   (args :initarg :args))
  (:default-initargs
   :argument-count (gensym "ARGUMENT-COUNT")
   :args (gensym "ARGS")))

(defun make-destructuring-environment (store-parameters)
  (let* ((required (mapcar #'specialization-store.lambda-lists:parameter-var (specialization-store.lambda-lists:required-parameters store-parameters)))
         (optional (mapcar #'specialization-store.lambda-lists:parameter-var (specialization-store.lambda-lists:optional-parameters store-parameters)))
         (keywords (loop
                      for parameter in (specialization-store.lambda-lists:keyword-parameters store-parameters)
                      collect (cons (specialization-store.lambda-lists:parameter-keyword parameter)
                                    (specialization-store.lambda-lists:parameter-var parameter))))
         (positional (append required optional)))
    (cond ((specialization-store.lambda-lists:keyword-parameters-p store-parameters)
           (make-instance 'keywords-environment
                          :positional positional
                          :keywords keywords
                          :allow-others-p (specialization-store.lambda-lists:allow-other-keys-p store-parameters)))
          ((specialization-store.lambda-lists:rest-parameter store-parameters)
           (make-instance 'variable-environment
                          :positional positional))
          (t
           (make-instance 'positional-environment
                          :positional positional)))))

(defgeneric generate-code (object function-env destructuring-env))

(defmethod generate-code ((node specialization-store.dispatch:node) function-env destructuring-env)
  (cond ((specialization-store.dispatch:leafp node)
         (with-slots (fail) function-env
           (or (generate-code (specialization-store.dispatch:node-value node) function-env destructuring-env)
               `(,fail))))
        (t
         (let* ((condition (generate-code (specialization-store.dispatch:node-value node) function-env destructuring-env))
                (pass (generate-code (specialization-store.dispatch:node-pass node) function-env destructuring-env))
                (fail (generate-code (specialization-store.dispatch:node-fail node) function-env destructuring-env)))
           (cond ((eql condition t)
                  pass)
                 ((null condition)
                  fail)
                 (t
                  `(if ,condition
                       ,pass
                       ,fail)))))))

(defmethod generate-code ((rule specialization-store.dispatch:fixed-argument-count-rule) f-env (d-env positional-environment))
  (declare (ignore f-env))
  (let ((count (specialization-store.dispatch:argument-count rule)))
    (with-slots (positional) d-env
      (= count (length positional)))))

(defmethod generate-code ((rule specialization-store.dispatch:fixed-argument-count-rule) f-env (d-env variable-environment))
  (declare (ignore f-env))
  (let ((count (specialization-store.dispatch:argument-count rule)))
    (with-slots (argument-count) d-env
      `(= ,argument-count ,count))))

(defmethod generate-code ((rule specialization-store.dispatch:accepts-argument-count-rule) f-env (d-env positional-environment))
  (declare (ignore f-env))
  (let ((count (specialization-store.dispatch:argument-count rule)))
    (with-slots (positional) d-env
      (>= (length positional) count))))

(defmethod generate-code ((rule specialization-store.dispatch:accepts-argument-count-rule) f-env (d-env variable-environment))
  (declare (ignore f-env))
  (let ((count (specialization-store.dispatch:argument-count rule)))
    (with-slots (argument-count) d-env
      `(>= ,argument-count ,count))))

(defmethod generate-code ((rule specialization-store.dispatch:positional-parameter-type-rule) f-env (d-env positional-environment))
  (with-slots (positional) d-env
    (let* ((position (specialization-store.dispatch:parameter-position rule))
           (type (specialization-store.dispatch:parameter-type rule))
           (symbol (elt positional position)))
      (assert (and (<= 0 position) (< position (length positional))))
      (etypecase f-env
        (value-function-environment
         `(typep ,symbol ',type))
        (type-function-environment
         `(subtypep ,symbol ',type))))))

(defmethod generate-code ((rule specialization-store.dispatch:positional-parameter-type-rule) f-env (d-env variable-environment))
  (with-slots (positional args) d-env
    (let* ((position (specialization-store.dispatch:parameter-position rule))
           (type (specialization-store.dispatch:parameter-type rule))
           (reader (cond ((< position (length positional))
                          (elt positional position))
                         (t
                          `(elt ,args ,(- position (length positional)))))))
      (assert (<= 0 position))
      (etypecase f-env
        (value-function-environment
         `(typep ,reader ',type))
        (type-function-environment
         `(subtypep ,reader ',type))))))

(defmethod generate-code ((rule specialization-store.dispatch:keyword-parameter-type-rule) f-env (d-env keywords-environment))
  (with-slots (keywords) d-env
    (let* ((keyword (specialization-store.dispatch:parameter-keyword rule))
           (type (specialization-store.dispatch:parameter-type rule))
           (symbol (cdr (assoc keyword keywords))))
      (assert symbol)
      (etypecase f-env
        (value-function-environment
         `(typep ,symbol ',type))
        (type-function-environment
         `(subtypep ,symbol ',type))))))

(defmethod generate-code ((rule specialization-store.dispatch:constantly-rule) f-env d-env)
  (declare (ignore f-env d-env))
  (specialization-store.dispatch:constantly-rule-value rule))

(defmethod generate-code ((rule specialization-store.dispatch:rest-objects-rule) (f-env value-function-environment) (d-env variable-environment))
  (with-slots (positional) d-env
    (let* ((type (specialization-store.dispatch:rest-objects-rule-type rule))
           (position (specialization-store.dispatch:rest-objects-rule-position rule))
           (offset (- position (length positional))))
      (assert (typep offset '(integer 0)))
      (with-slots (args) d-env
        (alexandria:with-gensyms (arg)
          `(loop
             for ,arg in (nthcdr ,offset ,args)
             always (typep ,arg ',type)))))))

(defmethod generate-code ((rule specialization-store.dispatch:rest-objects-rule) (f-env type-function-environment) (d-env variable-environment))
  (with-slots (positional) d-env
    (let* ((type (specialization-store.dispatch:rest-objects-rule-type rule))
           (position (specialization-store.dispatch:rest-objects-rule-position rule))
           (offset (- position (length positional))))
      (assert (typep offset '(integer 0)))
      (with-slots (args) d-env
        (alexandria:with-gensyms (arg)
          `(loop
             for ,arg in (nthcdr ,offset ,args)
             always (subtypep ,arg ',type)))))))

(defmethod generate-code ((parameters specialization-store.lambda-lists:specialization-parameters) (f-env value-function-environment) (d-env positional-environment))
  (with-slots (store) f-env
    (with-slots (positional) d-env
      (let* ((specialization (find parameters (store-specializations store) :key #'specialization-parameters)))
        (assert specialization)
        `(funcall (specialization-function ,specialization) ,@positional)))))

(defmethod generate-code ((parameters specialization-store.lambda-lists:specialization-parameters) (f-env value-function-environment) (d-env keywords-environment))
  (with-slots (store) f-env
    (with-slots (positional args) d-env
      (let ((specialization (find parameters (store-specializations store) :key #'specialization-parameters)))
        (assert specialization)
        `(apply (specialization-function ,specialization) ,@positional ,args)))))

(defmethod generate-code ((parameters specialization-store.lambda-lists:specialization-parameters) (f-env value-function-environment) (d-env variable-environment))
  (with-slots (store) f-env
    (with-slots (positional args) d-env
      (let ((specialization (find parameters (store-specializations store) :key #'specialization-parameters)))
        (assert specialization)
        `(apply (specialization-function ,specialization) ,@positional ,args)))))

(defmethod generate-code ((parameters specialization-store.lambda-lists:specialization-parameters) (f-env type-function-environment) d-env)
  (declare (ignore d-env))
  (with-slots (store form environment) f-env
    (let* ((specialization (find parameters (store-specializations store) :key #'specialization-parameters))
           (fn (gensym "FN")))
      (assert specialization)
      `(let ((,fn (specialization-expand-function ,specialization)))
         (if ,fn
             (funcall ,fn ,form ,environment)
             ,form)))))

(defmethod generate-code ((null null) (f-env function-environment) d-env)
  (declare (ignore d-env))
  (with-slots (fail) f-env
    `(,fail)))

(defgeneric compute-dispatch-lambda-form (function-environment destructuring-environment dispatch-tree))

(defmethod compute-dispatch-lambda-form ((f-env value-function-environment) (d-env positional-environment) dispatch-tree)
  (with-slots (store fail) f-env
    (with-slots (positional) d-env
      `(lambda ,positional
         (declare (ignorable ,@positional))
         (flet ((,fail ()
                  (error 'inapplicable-arguments-error :arguments (list ,@positional) :store ,store)))
           (declare (ignorable (function ,fail)))
           ,(generate-code dispatch-tree f-env d-env))))))

(defmethod compute-dispatch-lambda-form ((f-env value-function-environment) (d-env keywords-environment) dispatch-tree)
  (with-slots (store fail) f-env
    (with-slots (positional keywords args allow-others-p) d-env
      (let ((keys (loop
                     for (keyword . name) in keywords
                     collect `((,keyword ,name))))
            (allow-other-keys (when allow-others-p
                                '(&allow-other-keys))))
        `(lambda (,@positional &rest ,args &key ,@keys ,@allow-other-keys)
           (declare (ignorable ,@positional ,args ,@(mapcar #'cdr keywords)))
           (flet ((,fail ()
                    (error 'inapplicable-arguments-error :arguments (append (list ,@positional) ,args) :store ,store)))
             (declare (ignorable (function ,fail)))
             ,(generate-code dispatch-tree f-env d-env)))))))

(defmethod compute-dispatch-lambda-form ((f-env value-function-environment) (d-env variable-environment) dispatch-tree)
  (with-slots (store fail) f-env
    (with-slots (positional args argument-count) d-env
      `(lambda (,@positional &rest ,args)
         (declare (ignorable ,@positional ,args))
         (flet ((,fail ()
                  (error 'inapplicable-arguments-error :arguments (append (list ,@positional) ,args) :store ,store)))
           (declare (ignorable (function ,fail)))
           (let ((,argument-count (+ ,(length positional) (length ,args))))
             (declare (ignorable ,argument-count))
             ,(generate-code dispatch-tree f-env d-env)))))))

(defmethod compute-dispatch-lambda-form ((f-env type-function-environment) (d-env positional-environment) dispatch-tree)
  (with-slots (store fail form environment completed-types) f-env
    (with-slots (positional) d-env
      `(lambda (,form ,environment ,completed-types)
         (declare (ignorable ,form ,environment ,completed-types))
         (destructuring-bind ,positional ,completed-types
           (declare (ignorable ,@positional))
           (flet ((,fail ()
                    ,form))
             (declare (ignorable (function ,fail)))
             ,(generate-code dispatch-tree f-env d-env)))))))

(defmethod compute-dispatch-lambda-form ((f-env type-function-environment) (d-env keywords-environment) dispatch-tree)
  (with-slots (store fail form environment completed-types) f-env
    (with-slots (positional keywords allow-others-p args) d-env
      (let ((allow-other-keys (when allow-others-p '(&allow-other-keys)))
            (keys (loop
                     for (keyword . name) in keywords
                     collect `((,keyword ,name)))))
        `(lambda (,form ,environment ,completed-types)
           (declare (ignorable ,form ,environment ,completed-types))
           (destructuring-bind (,@positional &rest ,args &key ,@keys ,@allow-other-keys) ,completed-types
             (declare (ignorable ,@positional ,args ,@(mapcar #'cdr keywords)))
             (flet ((,fail ()
                      ,form))
               (declare (ignorable (function ,fail)))
               ,(generate-code dispatch-tree f-env d-env))))))))

(defmethod compute-dispatch-lambda-form ((f-env type-function-environment) (d-env variable-environment) dispatch-tree)
  (with-slots (store fail form environment completed-types) f-env
    (with-slots (positional args argument-count) d-env
      `(lambda (,form ,environment ,completed-types)
         (declare (ignorable ,form ,environment ,completed-types))
         (destructuring-bind (,@positional &rest ,args) ,completed-types
           (declare (ignorable ,@positional ,args))
           (flet ((,fail ()
                    ,form))
             (declare (ignorable (function ,fail)))
             (let ((,argument-count (+ ,(length positional) (length ,args))))
               (declare (ignorable ,argument-count))
               ,(generate-code dispatch-tree f-env d-env))))))))

(defmethod compute-dispatch-lambda-forms ((store standard-store))
  (let* ((parameters (store-parameters store))
         (all-specialization-parameters (mapcar #'specialization-parameters (store-specializations store)))
         (dispatch-tree (specialization-store.dispatch:make-dispatch-tree parameters all-specialization-parameters))
         (destructuring-env (make-destructuring-environment parameters))
         (value-lambda-form (compute-dispatch-lambda-form (make-function-environment store :value)
                                                          destructuring-env
                                                          dispatch-tree))
         (type-lambda-form (compute-dispatch-lambda-form (make-function-environment store :type)
                                                         destructuring-env
                                                         dispatch-tree)))
    (list value-lambda-form type-lambda-form)))

(defmethod compute-dispatch-functions ((store standard-store))
  (destructuring-bind (value-lambda-form type-lambda-form)
      (compute-dispatch-lambda-forms store)
    (multiple-value-bind (value-fn value-warnings value-failure) (compile nil value-lambda-form)
      (declare (ignore value-warnings))
      (multiple-value-bind (type-fn type-warnings type-failure) (compile nil type-lambda-form)
        (declare (ignore type-warnings))
        (when value-failure
          (error "Unable to compile value dispatch function for store ~A." store))
        (when type-failure
          (error "Unable to compile type dispatch function for store ~A." store))
        (list value-fn type-fn)))))
