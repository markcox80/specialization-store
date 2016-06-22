(in-package "SPECIALIZATION-STORE.STANDARD-STORE")

;;;; Notes
;;
;; Store Function Invocation (Runtime)
;;
;; 1. Complete optional and keyword arguments according to the store
;;    lambda list.
;; 2. Compute the most specific specialization using the type of each argument.
;; 3. Invoke the specialization function with the completed arguments.
;;
;; Store Function Invocation (Compile Time)
;;
;; 1. Compute the types of all arguments, including completeing all of
;;    the optional and keyword argument types (using information from
;;    the store lambda list).
;; 2. Compute the most specific specialization.
;; 3. Invoke the specialization expander function with the 

;;;; Standard Store Class

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
   (value-completion-function :initarg :value-completion-function
                              :reader store-value-completion-function)
   (type-completion-function :initarg :type-completion-function
                             :reader store-type-completion-function)
   (form-completion-function :initarg :form-completion-function
                             :reader store-form-completion-function)
   (runtime-function :initarg :runtime-function)
   (compile-time-function :initarg :compile-time-function))
  (:metaclass specialization-store.mop:funcallable-standard-class)
  (:default-initargs
   :name nil
   :documentation nil    
   :specializations nil
   :specialization-class (find-class 'standard-specialization)
   :value-completion-function nil
   :type-completion-function nil
   :form-completion-function nil))

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
  (:metaclass specialization-store.mop:funcallable-standard-class)
  (:default-initargs
   :name nil
   :expand-function nil
   :documentation nil))

(defmethod print-object ((object standard-specialization) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (specialization-lambda-list object) stream)))

;;;; Standard Store Implementation (Object Layer)

(defun completion-functions-required-p (store-parameters)
  (check-type store-parameters store-parameters)
  (or (not (loop
              for (nil init-form) in (optional-parameters store-parameters)
              always (constantp init-form)))
      (not (loop
              for (nil nil init-form) in (keyword-parameters store-parameters)
              always (constantp init-form)))))

(defun make-default-value-completion-function (store-parameters)
  (compile nil (make-value-completion-lambda-form store-parameters)))

(defun make-default-type-completion-function (store-parameters)
  (compile nil (make-type-completion-lambda-form store-parameters nil)))

(defun make-default-form-completion-function (store-parameters)
  (destructuring-bind (lambda-form globals) (make-form-completion-lambda-form store-parameters nil)
    (unless (null globals)
      (error "Cannot create default form completion function as the store lambda list requires the introduction of global functions."))
    (compile nil lambda-form)))

(defmethod initialize-instance :after ((instance standard-store)
                                       &key
                                         (lambda-list nil lambda-list-p)
                                         (value-completion-function nil value-completion-function-p)
                                         (type-completion-function nil type-completion-function-p)
                                         (form-completion-function nil form-completion-function-p)
                                         &allow-other-keys)
  (assert lambda-list-p)
  (let ((new-parameters (parse-store-lambda-list lambda-list)))
    (when (and (completion-functions-required-p new-parameters)
               (or (not (and value-completion-function-p
                             (functionp value-completion-function)))
                   (not (and type-completion-function-p
                             (functionp type-completion-function)))
                   (not (and form-completion-function-p
                             (functionp form-completion-function)))))
      (error 'missing-completion-functions-error :store instance))
    (setf (slot-value instance 'parameters) new-parameters
          (slot-value instance 'value-completion-function) (or value-completion-function
                                                               (make-default-value-completion-function new-parameters))
          (slot-value instance 'type-completion-function) (or type-completion-function
                                                              (make-default-type-completion-function new-parameters))
          (slot-value instance 'form-completion-function) (or form-completion-function
                                                              (make-default-form-completion-function new-parameters))))

  (clear-dispatch-functions instance))

(defmethod reinitialize-instance :after ((instance standard-store)
                                         &key
                                           ((:name new-name) nil new-name-p)
                                           ((:lambda-list new-lambda-list) nil new-lambda-list-p)
                                           (value-completion-function nil value-completion-function-p)
                                           (type-completion-function nil type-completion-function-p)
                                           (form-completion-function nil form-completion-function-p)
                                           &allow-other-keys)
  (when new-name-p
    (unless (eql (store-name instance) new-name)
      (error "Cannot change names of standard store objects.")))

  (when new-lambda-list-p
    (let* ((old-parameters (store-parameters instance))
           (new-parameters (parse-store-lambda-list new-lambda-list))
           (congruent? (congruent-parameters-p old-parameters new-parameters)))
      (cond ((or congruent? (null (store-specializations instance)))
             (when (and (completion-functions-required-p new-parameters)
                        (not congruent?)
                        (or (not (and value-completion-function-p
                                      (functionp value-completion-function)))
                            (not (and type-completion-function-p
                                      (functionp type-completion-function)))
                            (not (and form-completion-function-p
                                      (functionp form-completion-function)))))
               (error 'simple-store-error
                      :store instance
                      :message "New completion functions are required for the new store lambda list."))

             (with-slots (parameters) instance
               (setf parameters new-parameters))

             (clear-dispatch-functions instance))
            (t
             (error 'simple-store-error
                    :store instance
                    :message (format nil "Unable to change store lambda list.")))))))

(defmethod funcall-store ((store standard-store) &rest args)
  (with-slots (runtime-function) store
    (apply runtime-function args)))

(defmethod apply-store ((store standard-store) &rest args)
  (with-slots (runtime-function) store
    (apply #'apply runtime-function args)))

(defmethod expand-store ((store standard-store) form &optional env)
  (with-slots (compile-time-function) store
    (funcall compile-time-function form env)))

(defmethod add-specialization ((store standard-store) (specialization standard-specialization))
  (unless (congruent-parameters-p (store-parameters store) (specialization-parameters specialization))
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
      (and (compare #'= #'length #'required-parameters)
           (compare #'= #'length #'optional-parameters)
           (compare #'eql #'null #'rest-parameter)
           (compare #'eql #'keyword-parameters-p)
           (loop
              for (nil type-a) in (required-parameters parameters-a)
              for (nil type-b) in (required-parameters parameters-b)
              always
                (alexandria:type= type-a type-b))
           (loop
              with keys-a = (keyword-parameters parameters-a)
              with keys-b = (keyword-parameters parameters-b)
              for (keyword nil) in (keyword-parameters parameters)
              for key-a = (find keyword keys-a :key #'first)
              for key-b = (find keyword keys-b :key #'first)
              for type-a = (or (third key-a) t)
              for type-b = (or (third key-b) t)
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
      (setf (slot-value instance 'parameters) (parse-specialization-lambda-list (specialization-lambda-list instance))))

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

(defmethod ensure-store-using-object ((class (eql (find-class 'standard-store))) store-name store-lambda-list
                                      &rest args
                                      &key
                                        specialization-class documentation
                                        value-completion-function
                                        type-completion-function
                                        form-completion-function
                                        &allow-other-keys)
  (declare (ignore specialization-class documentation
                   value-completion-function type-completion-function
                   form-completion-function))
  (apply #'make-instance 'standard-store
         :name store-name
         :lambda-list store-lambda-list
         args))

(defmethod ensure-store-using-object ((instance standard-store) store-name store-lambda-list
                                      &rest args
                                      &key
                                        specialization-class documentation
                                        value-completion-function
                                        type-completion-function
                                        form-completion-function
                                      &allow-other-keys)
  (declare (ignore specialization-class documentation
                   value-completion-function type-completion-function
                   form-completion-function))
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

;;;; Dispatch Functions

(defmethod clear-dispatch-functions ((store standard-store))
  (with-slots (runtime-function compile-time-function) store
    (labels ((update-runtime (&rest args)
               (update-dispatch-functions store)
               (apply (slot-value store 'runtime-function) args))
             (update-compile-time (form &optional env)
               (update-dispatch-functions store)
               (funcall (slot-value store 'compile-time-function) form env)))
      (setf runtime-function #'update-runtime
            compile-time-function #'update-compile-time)
      (specialization-store.mop:set-funcallable-instance-function store #'update-runtime))))

(defmethod update-dispatch-functions ((store standard-store))
  (with-slots (runtime-function compile-time-function) store
    (with-slots (value-completion-function type-completion-function form-completion-function) store
      (labels ((third-argument (a b c)
                 (declare (ignore a b))
                 c)
               (cascade (compile-time)
                 (let ((type-fn (funcall type-completion-function #'third-argument))
                       (form-fn (funcall form-completion-function #'third-argument)))
                   (lambda (form env)
                     (funcall compile-time form env
                              (funcall type-fn form env)
                              (funcall form-fn form env))))))
        (destructuring-bind (runtime compile-time) (compute-dispatch-functions store)
          (setf runtime-function (funcall value-completion-function runtime)
                compile-time-function (cascade compile-time))
          (specialization-store.mop:set-funcallable-instance-function store runtime)))))
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
   (completed-types :initarg :completed-types)
   (completed-form :initarg :completed-form))
  (:default-initargs
   :form (gensym "FORM")
   :environment (gensym "ENV")
   :completed-types (gensym "COMPLETED-TYPES")
   :completed-form (gensym "COMPLETED-FORM")))

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
  (let* ((required (required-parameters store-parameters))
         (optional (optional-parameters store-parameters))
         (keywords (loop
                      for (keyword name) in (keyword-parameters store-parameters)
                      collect (cons keyword name)))
         (positional (append required (mapcar #'first optional))))
    (cond ((keyword-parameters-p store-parameters)
           (make-instance 'keywords-environment
                          :positional positional
                          :keywords keywords
                          :allow-others-p (allow-other-keys-p store-parameters)))
          ((rest-parameter store-parameters)
           (make-instance 'variable-environment
                          :positional positional))
          (t
           (make-instance 'positional-environment
                          :positional positional)))))

(defgeneric generate-code (object function-env destructuring-env))

(defmethod generate-code ((node node) function-env destructuring-env)
  (cond ((leafp node)
         (with-slots (fail) function-env
           (or (generate-code (node-value node) function-env destructuring-env)
               `(,fail))))
        (t
         (let* ((condition (generate-code (node-value node) function-env destructuring-env))
                (pass (generate-code (node-pass node) function-env destructuring-env))
                (fail (generate-code (node-fail node) function-env destructuring-env)))
           (cond ((eql condition t)
                  pass)
                 ((null condition)
                  fail)
                 (t
                  `(if ,condition
                       ,pass
                       ,fail)))))))

(defmethod generate-code ((rule fixed-argument-count-rule) f-env (d-env positional-environment))
  (declare (ignore f-env))
  (let ((count (argument-count rule)))
    (with-slots (positional) d-env
      (= count (length positional)))))

(defmethod generate-code ((rule fixed-argument-count-rule) f-env (d-env variable-environment))
  (declare (ignore f-env))
  (let ((count (argument-count rule)))
    (with-slots (argument-count) d-env
      `(= ,argument-count ,count))))

(defmethod generate-code ((rule accepts-argument-count-rule) f-env (d-env positional-environment))
  (let ((count (argument-count rule)))
    (with-slots (positional) d-env
      (>= (length positional) count))))

(defmethod generate-code ((rule accepts-argument-count-rule) f-env (d-env variable-environment))
  (let ((count (argument-count rule)))
    (with-slots (argument-count) d-env
      `(>= ,argument-count ,count))))

(defmethod generate-code ((rule positional-parameter-type-rule) f-env (d-env positional-environment))
  (with-slots (positional) d-env
    (let* ((position (parameter-position rule))
           (type (parameter-type rule))
           (symbol (elt positional position)))
      (assert (and (<= 0 position) (< position (length positional))))
      (etypecase f-env
        (value-function-environment
         `(typep ,symbol ',type))
        (type-function-environment
         `(subtypep ,symbol ',type))))))

(defmethod generate-code ((rule positional-parameter-type-rule) f-env (d-env variable-environment))
  (with-slots (positional args) d-env
    (let* ((position (parameter-position rule))
           (type (parameter-type rule))
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

(defmethod generate-code ((rule keyword-parameter-type-rule) f-env (d-env keywords-environment))
  (with-slots (keywords) d-env
    (let* ((keyword (parameter-keyword rule))
           (type (parameter-type rule))
           (symbol (cdr (assoc keyword keywords))))
      (assert symbol)
      (etypecase f-env
        (value-function-environment
         `(typep ,symbol ',type))
        (type-function-environment
         `(subtypep ,symbol ',type))))))

(defmethod generate-code ((rule constantly-rule) f-env d-env)
  (declare (ignore f-env d-env))
  (constantly-rule-value rule))

(defmethod generate-code ((parameters specialization-parameters) (f-env value-function-environment) (d-env positional-environment))
  (with-slots (store) f-env
    (with-slots (positional) d-env
      (let* ((specialization (find parameters (store-specializations store) :key #'specialization-parameters)))
        (assert specialization)
        `(funcall (specialization-function ,specialization) ,@positional)))))

(defmethod generate-code ((parameters specialization-parameters) (f-env value-function-environment) (d-env keywords-environment))
  (with-slots (store) f-env
    (with-slots (positional args) d-env
      (let ((specialization (find parameters (store-specializations store) :key #'specialization-parameters)))
        (assert specialization)
        `(apply (specialization-function ,specialization) ,@positional ,args)))))

(defmethod generate-code ((parameters specialization-parameters) (f-env value-function-environment) (d-env variable-environment))
  (with-slots (store) f-env
    (with-slots (positional args) d-env
      (let ((specialization (find parameters (store-specializations store) :key #'specialization-parameters)))
        (assert specialization)
        `(apply (specialization-function ,specialization) ,@positional ,args)))))

(defmethod generate-code ((parameters specialization-parameters) (f-env type-function-environment) d-env)
  (with-slots (store form environment completed-form) f-env
    (let* ((specialization (find parameters (store-specializations store) :key #'specialization-parameters))
           (fn (gensym "FN")))
      (assert specialization)
      `(let ((,fn (specialization-expand-function ,specialization)))
         (if ,fn
             (funcall ,fn ,completed-form ,environment)
             ,form)))))

(defmethod generate-code ((null null) (f-env function-environment) d-env)
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
  (with-slots (store fail form environment completed-types completed-form) f-env
    (with-slots (positional) d-env
      `(lambda (,form ,environment ,completed-types ,completed-form)
         (declare (ignorable ,form ,environment ,completed-types ,completed-form))
         (destructuring-bind ,positional ,completed-types
           (declare (ignorable ,@positional))
           (flet ((,fail ()
                    ,form))
             (declare (ignorable (function ,fail)))
             ,(generate-code dispatch-tree f-env d-env)))))))

(defmethod compute-dispatch-lambda-form ((f-env type-function-environment) (d-env keywords-environment) dispatch-tree)
  (with-slots (store fail form environment completed-types completed-form) f-env
    (with-slots (positional keywords allow-others-p args) d-env
      (let ((allow-other-keys (when allow-others-p '(&allow-other-keys)))
            (keys (loop
                     for (keyword . name) in keywords
                     collect `((,keyword ,name)))))
        `(lambda (,form ,environment ,completed-types ,completed-form)
           (declare (ignorable ,form ,environment ,completed-types ,completed-form))
           (destructuring-bind (,@positional &rest ,args &key ,@keys ,@allow-other-keys) ,completed-types
             (declare (ignorable ,@positional ,args ,@(mapcar #'cdr keywords)))
             (flet ((,fail ()
                      ,form))
               (declare (ignorable (function ,fail)))
               ,(generate-code dispatch-tree f-env d-env))))))))

(defmethod compute-dispatch-lambda-form ((f-env type-function-environment) (d-env variable-environment) dispatch-tree)
  (with-slots (store fail form environment completed-types completed-form) f-env
    (with-slots (positional args argument-count) d-env
      `(lambda (,form ,environment ,completed-types ,completed-form)
         (declare (ignorable ,form ,environment ,completed-types ,completed-form))
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
         (dispatch-tree (make-dispatch-tree parameters all-specialization-parameters))
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
    (list (compile nil value-lambda-form)
          (compile nil type-lambda-form))))
