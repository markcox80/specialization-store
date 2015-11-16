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

(defclass standard-store ()
  ((name :initarg :name
         :reader store-name)
   (lambda-list :initarg :lambda-list
                :reader store-lambda-list)
   (documentation :initarg :documentation
                  :accessor store-documentation)
   (specializations :initarg :specializations
                    :accessor store-specializations)
   (specialization-class :initarg :specialization-class
                         :reader store-specialization-class)
   (completion-function :initarg :completion-function)
   (form-type-completion-function :initarg :form-type-completion-function)
   (discriminating-function))
  (:default-initargs
   :name nil
   :documentation nil    
   :specializations nil))

(defclass standard-specialization ()
  ((name :initarg :name
         :reader specialization-name)
   (lambda-list :initarg :lambda-list
                :reader specialization-lambda-list)
   (documentation :initarg :documentation
                  :accessor specialization-documentation)
   (function :initarg :function
             :reader specialization-function)
   (expand-function :initarg :expand-function
                    :reader specialization-expand-function))
  (:default-initargs
   :name nil
   :expand-function nil
   :documentation nil))

;;;; Standard Store Implementation (Object Layer)

(defun compute-discriminating-function (store specializations)
  )

(defun update-discriminating-function (store)
  (check-type store standard-store)
  (with-slots (discriminating-function) store
    (let* ((specializations (store-specializations store)))
      (setf discriminating-function (compute-discriminating-function store specializations)))))

(defmethod funcall-store ((store standard-store) &rest args)
  )

(defmethod apply-store ((store standard-store) &rest args)
  (apply #'apply #'funcall-store args))

(defmethod expand-store ((store standard-store) form &optional env)
  )

(defmethod add-specialization ((store standard-store) (specialization standard-specialization))
  )

(defmethod remove-specialization ((store standard-store) (specialization standard-specialization))
  )

;;;; Standard Specialization Implementation (Object Layer)

(defmethod specialization-equal ((a standard-specialization) (b standard-specialization))
  )


;;;; Standard Store Implementation (Glue Layer)

(defmethod ensure-store-using-class ((class standard-store) store-name lambda-list completion-function form-type-completion-function
                                     &rest args &key store-class specialization-class documentation &allow-other-keys)
  (declare (ignore store-class))
  (let* ((parameters (specialization-store.lambda-lists:parse-store-lambda-list lambda-list)))
    (apply #'reinitialize-instance class
           :name store-name
           :lambda-list lambda-list
           :completion-function completion-function
           :form-type-completion-function form-type-completion-function
           :specialization-class specialization-class
           args)
    ;; Function
    (let* ((continuation (funcall (compile nil
                                           `(lambda ()
                                              (lambda (&rest args)
                                                (apply (funcall ,(make-runtime-type-of-lambda-form parameters)
                                                                (lambda (&rest arg-types)
                                                                  (print args)
                                                                  (print arg-types)
                                                                  nil))
                                                       args)))))))
      (setf (fdefinition store-name) (compile nil (funcall completion-function continuation))))

    ;; Compiler Macro
    (let* ()
      (setf (compiler-macro-function store-name) (compiler-macro-lambda (&rest args &environment env)
                                                   (apply form-type-completion-function
                                                          (lambda (environment &rest args)
                                                            (declare (ignore environment))
                                                            (print args)
                                                            nil)
                                                          env
                                                          args))))

    ;; Documentation
    (setf (documentation store-name 'function) documentation)
    class))


;;;; Standard Specialization Implementation (Glue Layer)

(defmethod ensure-specialization-using-class ((class standard-store) lambda-list function &key expand-function name documentation &allow-other-keys)
  (let* ((specialization (make-instance 'standard-specialization
                                        :name name
                                        :lambda-list lambda-list
                                        :documentation documentation
                                        :function function
                                        :expand-function expand-function)))    
    (when name
      (setf (fdefinition name) function))
    (when (and name expand-function)
      (setf (compiler-macro-function name) expand-function))
    (add-specialization class specialization)
    specialization))


;;;; Helpers

(defun convert-optional-to-required (parameters)
  (check-type parameters specialization-store.lambda-lists:store-parameters)
  (let* ((required (specialization-store.lambda-lists:required-parameters parameters))
         (optional (specialization-store.lambda-lists:optional-parameters parameters))
         (rest (specialization-store.lambda-lists:rest-parameter parameters))
         (keywordsp (specialization-store.lambda-lists:keyword-parameters-p parameters))
         (keywords (specialization-store.lambda-lists:keyword-parameters parameters))
         (allow-other-keys (specialization-store.lambda-lists:allow-other-keys-p parameters))
         (positional-lambda-list (append required (mapcar #'first optional)))         
         (keyword-lambda-list (when keywordsp
                                `(&key ,@(loop
                                            for (keyword var init-form) in keywords
                                            collect `((,keyword ,var) ,init-form))
                                       ,@(when allow-other-keys
                                            '(&allow-other-keys)))))
         (rest-lambda-list (when rest
                             `(&rest ,rest)))
         (lambda-list `(,@positional-lambda-list ,@rest-lambda-list ,@keyword-lambda-list)))
    (specialization-store.lambda-lists:parse-store-lambda-list lambda-list)))

(defun make-runtime-type-of-lambda-form (parameters)
  (check-type parameters specialization-store.lambda-lists:store-parameters)
  (let* ((parameters (convert-optional-to-required parameters))
         (original-lambda-list (specialization-store.lambda-lists:original-lambda-list parameters))
         (required (specialization-store.lambda-lists:required-parameters parameters))
         (optional (specialization-store.lambda-lists:optional-parameters parameters))
         (rest (specialization-store.lambda-lists:rest-parameter parameters))
         (keywordsp (specialization-store.lambda-lists:keyword-parameters-p parameters))
         (keywords (specialization-store.lambda-lists:keyword-parameters parameters))
         (continuation (gensym "CONTINUATION"))
         (positional-vars (loop
                             for var in required
                             collect `(type-of ,var)))
         (keywords-vars (loop
                           for (keyword-name var nil) in keywords
                           append `(,keyword-name (type-of ,var)))))
    (assert (null optional))
    (cond
      (keywordsp
       (let* ((rest (or rest (gensym "REST")))
              (lambda-list `(,@required &rest ,rest &key ,@keywords &allow-other-keys)))
         `(lambda (,continuation)
            (lambda ,lambda-list
              (apply ,continuation ,@positional-vars ,keywords-vars ,rest)))))
      (rest
       `(lambda (,continuation)
          (lambda ,original-lambda-list
            (apply ,continuation ,@positional-vars (mapcar #'type-of ,rest)))))
      (t
       `(lambda (,continuation)
          (lambda ,original-lambda-list
            (funcall ,continuation ,@positional-vars)))))))
