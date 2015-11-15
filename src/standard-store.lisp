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
   (run-time-discriminating-function)
   (compile-time-discriminating-function))
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

(defun compute-run-time-discriminating-function (store specializations)
  )

(defun compute-compile-time-discriminating-function (store specializations)
  )

(defun update-discriminating-functions (store)
  (check-type store standard-store)
  (with-slots (run-time-discriminating-function compile-time-discriminating-function) store
    (let* ((specializations (store-specializations store)))
      (setf run-time-discriminating-function (compute-run-time-discriminating-function store specializations)
            compile-time-discriminating-function (compute-compile-time-discriminating-function store specializations)))))

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

(defmethod ensure-store-using-class ((class standard-store) store-name lambda-list completion-function expand-completion-function &key store-class specialization-class &allow-other-keys)
  )


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
