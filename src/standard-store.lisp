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
   (runtime-function :initarg :runtime-function)
   (compile-time-function :initarg :compile-time-function))
  (:metaclass specialization-store.mop:funcallable-standard-class)
  (:default-initargs
   :name nil
   :documentation nil    
   :specializations nil
   :value-completion-function nil
   :type-completion-function nil))

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
  (or (loop
         for (nil init-form) in (optional-parameters store-parameters)
         thereis init-form)
      (loop
         for (nil nil init-form) in (keyword-parameters store-parameters)
         thereis init-form)))

(defun default-value-completion-function (continuation)
  (lambda (&rest args)
    (apply continuation args)))

(defun default-type-completion-function (continuation)
  (compiler-macro-lambda (&whole form &rest args &environment env)
    (funcall continuation form env (loop
                                      for arg in args
                                      collect (determine-form-value-type arg env)))))

(defmethod initialize-instance :after ((instance standard-store)
                                       &key
                                         (lambda-list nil lambda-list-p)
                                         (value-completion-function nil value-completion-function-p)
                                         (type-completion-function nil type-completion-function-p)
                                         &allow-other-keys)
  (assert lambda-list-p)
  (let ((new-parameters (parse-store-lambda-list lambda-list)))
    (when (and (completion-functions-required-p new-parameters)
               (or (not (and value-completion-function-p
                             (functionp value-completion-function)))
                   (not (and type-completion-function-p
                             (functionp type-completion-function)))))
      (error 'missing-completion-functions-error :store instance))
    (setf (slot-value instance 'parameters) new-parameters
          (slot-value instance 'value-completion-function) (or value-completion-function
                                                               #'default-value-completion-function)
          (slot-value instance 'type-completion-function) (or type-completion-function
                                                              #'default-type-completion-function))))

(defmethod reinitialize-instance :after ((instance standard-store)
                                         &key
                                           ((:lambda-list new-lambda-list) nil new-lambda-list-p)
                                           (value-completion-function nil value-completion-function-p)
                                           (type-completion-function nil type-completion-function-p)
                                         &allow-other-keys)
  (when new-lambda-list-p
    (let* ((old-parameters (store-parameters instance))
           (new-parameters (parse-store-lambda-list new-lambda-list)))
      (cond ((or (congruent-parameters-p old-parameters new-parameters)
                 (null (store-specializations instance)))
             (when (and (completion-functions-required-p new-parameters)
                        (not (parameters-equal old-parameters new-parameters))
                        (or (not (and value-completion-function-p
                                      (functionp value-completion-function)))
                            (not (and type-completion-function-p
                                      (functionp type-completion-function)))))
               (warn "No new completion functions were supplied with new store lambda list."))

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
  (let ((store-name (store-name store)))
    (fmakunbound store-name)
    (setf (compiler-macro-function store-name) nil)
    (map nil #'(lambda (specialization)
                 (let ((specialization-name (specialization-name specialization)))
                   (when specialization-name
                     (fmakunbound specialization-name)
                     (setf (compiler-macro-function specialization-name) nil))))
         (store-specializations store)))
  (values))


;;;; Dispatch Functions

(defmethod clear-dispatch-functions ((store standard-store))
  (with-slots (runtime-function compile-time-function) store
    (labels ((update-runtime (&rest args)
               (update-dispatch-functions store)
               (apply (slot-value store 'runtime-function) args))
             (update-compile-time (form env)
               (update-dispatch-functions store)
               (apply (slot-value store 'compile-time-function) form env)))
      (setf runtime-function #'update-runtime
            compile-time-function #'update-compile-time)
      (specialization-store.mop:set-funcallable-instance-function store #'update-runtime))))

(defmethod update-dispatch-functions ((store standard-store))
  (with-slots (runtime-function compile-time-function) store
    (destructuring-bind (runtime compile-time) (compute-dispatch-functions store)
      (setf runtime-function runtime
            compile-time-function compile-time)
      (specialization-store.mop:set-funcallable-instance-function store runtime)))
  (values))

(defun compute-dispatch-function/runtime (store)
  )

(defun compute-dispatch-function/compile-time (store)
  )

(defmethod compute-dispatch-functions ((store standard-store))
  (list (compute-dispatch-function/runtime store)
        (compute-dispatch-function/compile-time store)))
