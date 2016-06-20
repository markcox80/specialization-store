(in-package "SPECIALIZATION-STORE")

;;;; Object Layer

;;;; The store object protocol.
(defgeneric funcall-store (store &rest args))
(defgeneric apply-store (store &rest args))
(defgeneric expand-store (store form &optional environment))
(defgeneric add-specialization (store specialization))
(defgeneric remove-specialization (store specialization))
(defgeneric specialization-equal (store specialization-a specializabion-b))
(defgeneric store-specializations (store))
(defgeneric (setf store-specializations) (value store))
(defgeneric store-lambda-list (store))
(defgeneric store-documentation (store))

;; The specialization object protocol
(defgeneric specialization-function (specialization))
(defgeneric specialization-expand-function (specialization))
(defgeneric specialization-lambda-list (specialization))
(defgeneric specialization-value-type (specialization))
(defgeneric specialization-documentation (specialization))
(defgeneric (setf specialization-documentation) (value specialization))

;;;; Conditions
;; store-error
(defgeneric store-error-store (store-error))

(define-condition store-error (error)
  ((store :initarg :store
          :reader store-error-store          
          :initform (error "A value for the slot :store must be specified.")))
  (:report (lambda (condition stream)
	     (format stream "The store function ~W is in error."
                     (store-error-store condition)))))

;; simple-store-error
(defgeneric simple-store-error-message (store-error))

(define-condition simple-store-error (store-error)
  ((message :initarg :message
            :reader simple-store-error-message
            :initform (error "A value for the slot :message must be specified.")))
  (:report (lambda (condition stream)
	     (write-string (simple-store-error-message condition)
                           stream))))

;; invalid-store-name-error
(defgeneric invalid-store-name (invalid-store-name-error))

(define-condition invalid-store-name-error (error)
  ((name :initarg :name
         :reader invalid-store-name
         :initform (error "A value for the slot :name must be specified.")))
  (:report (lambda (condition stream)
             (format stream "No store exists with name ~W."
                     (invalid-store-name condition)))))

;; inapplicable-arguments-error
(defgeneric inapplicable-arguments (store-error))

(define-condition inapplicable-arguments-error (store-error)
  ((arguments :initarg :arguments
              :reader inapplicable-arguments
              :initform (error "A value for the slot :arguments must be specified.")))
  (:report (lambda (condition stream)
             (format stream "None of the specializations in store object ~W are applicable to the arguments: ~W."
                     (store-error-store condition)
                     (inapplicable-arguments condition)))))

;; incongruent-specialization-error
(defgeneric incongruent-specialization (store-error))

(define-condition incongruent-specialization-error (store-error)
  ((specialization :initarg :specialization
                   :reader incongruent-specialization
                   :initform (error "A value for the slot :specialization must be specified.")))
  (:report (lambda (condition stream)
             (format stream "The specialized lambda list ~W is not congruent with the store lambda list ~W."
                     (specialization-lambda-list (incongruent-specialization condition))
                     (store-lambda-list (store-error-store condition))))))

;; Duplicate specialization error
(define-condition duplicate-specialization-error (store-error)
  ())

;;;; Glue Layer

(define-condition ensure-store-error (error)
  ((object :initarg :object
           :reader ensure-store-error-object)))

(define-condition invalid-store-lambda-list-error (ensure-store-error)
  ((store-lambda-list :initarg :store-lambda-list
                      :reader invalid-store-lamba-list)))

(define-condition invalid-specialization-class-error (ensure-store-error)
  ((specialization-class :initarg :specialization-class
                         :reader invalid-specialization-class)))

(define-condition invalid-store-class-error (ensure-store-error)
  ((store-class :initarg :store-class
                :reader invalid-store-class)))

(define-condition missing-completion-functions-error (store-error)
  ())

(defgeneric store-name (store))
(defgeneric store-value-completion-function (store))
(defgeneric store-type-completion-function (store))
(defgeneric store-form-completion-function (store))
(defgeneric specialization-name (specialization))

(defun %find-store-helper (name)
  (flet ((valid-symbol-p (symbol)
	   (and (symbolp symbol)
		symbol)))
    (cond
      ((valid-symbol-p name)
       (values name 'store))
      ((and (listp name)
	    (= 2 (length name))
	    (eql 'setf (first name))
	    (and (valid-symbol-p (second name))))
       (values (second name) 'setf-store))
      (t
       (error 'invalid-store-name-error :name name)))))

(defun find-store (name)
  (multiple-value-bind (name indicator) (%find-store-helper name)
    (let ((store (get name indicator)))
      (if store
	  store
	  (error 'invalid-store-name-error :name name)))))

(defun (setf find-store) (value name)
  (multiple-value-bind (name indicator) (%find-store-helper name)
    (if value
        (setf (get name indicator) value)
        (make-store-unbound name))))

(defgeneric ensure-store-using-object (object store-name store-lambda-list
                                       &key
                                         specialization-class documentation
                                         value-completion-function type-completion-function
                                         &allow-other-keys))

(defun ensure-store (name store-lambda-list &rest args
                     &key
                       store-class specialization-class documentation
                       value-completion-function type-completion-function
                       &allow-other-keys)
  (declare (ignore specialization-class
                   documentation
                   value-completion-function type-completion-function))
  (let* ((store-class (etypecase store-class
                        (null (find-class 'standard-store))
                        (symbol (find-class store-class))
                        (t store-class)))
         (current-store (multiple-value-bind (name indicator) (%find-store-helper name)
                          (get name indicator)))
         (object (if current-store
                     current-store
                     store-class))
         (store (apply #'ensure-store-using-object object store-lambda-list
                       :store-class store-class
                       args)))
    (setf (find-store name) store)
    store))

(defgeneric make-store-unbound (store))

(defmethod make-store-unbound ((store symbol))
  (when store
    (multiple-value-bind (name indicator) (%find-store-helper store)
      (let ((store-object (get name indicator)))
        (when store-object
          (make-store-unbound store-object))))))

(defmethod make-store-unbound :after (store)
  (flet ((perform (name)
           (multiple-value-bind (name indicator) (%find-store-helper name)
             (remprop name indicator))))
    (typecase store
      (null nil)
      (symbol (perform store))
      (t (perform (store-name store))))))

(defgeneric ensure-specialization-using-object (store specialized-lambda-list value-type function &rest args
					       &key expand-function name documentation &allow-other-keys))

(defun ensure-specialization (store-name specialized-lambda-list value-type function
			      &rest args &key expand-function documentation name &allow-other-keys)
  (declare (ignore expand-function documentation name))
  (let* ((store (find-store store-name)))
    (apply #'ensure-specialization-using-object
           store specialized-lambda-list value-type function
           args)))

;; Store Object Requirements for the Glue Layer
(defgeneric store-specialization-class (store))

;;;; Syntax Layer

(defgeneric defstore-using-class (store-class name store-lambda-list
                                  &rest args
                                  &key
                                    documentation specialization-class environment
                                    &allow-other-keys))

(defgeneric defspecialization-using-object (store specialized-lambda-list value-type body
                                            &key name environment &allow-other-keys))

(defgeneric define-specialization-using-object (store specialized-lambda-list value-type
                                                &key
                                                  function expand-function name
                                                  documentation environment
                                                &allow-other-keys))

;; DEFSTORE

(defmacro defstore (store-name store-lambda-list &body body &environment env)
  (let* ((store-class (find :store-class body :key #'first))
         (store-class (typecase store-class
                        (null (find-class 'standard-store))
                        (symbol (find-class store-class))
                        (t store-class)))
         (form (apply #'defstore-using-class store-class store-name store-lambda-list
                      :environment env
                      (mapcan #'(lambda (item)
                                  (cond ((eql (first item) :store-class)
                                         nil)
                                        ((and (member (first item) '(:documentation :specialization-class)))
                                         (if (= 2 (length item))
                                             item
                                             (error "Invalid store definition option: ~W." item)))
                                        (t
                                         (list (first item) (rest item)))))
                              body))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,form)))

;; DEFSPECIALIZATION
(defun canonicalize-store-name (store-name)  
  (cond
    ((symbolp store-name)
     (list store-name))
    ((and (listp store-name)
	  (eql 'setf (first store-name)))
     (list store-name))
    ((listp store-name)
     store-name)
    (t
     (error "Invalid store name ~W." store-name))))

(defmacro defspecialization (store-name specialized-lambda-list value-type &body body &environment env)
  (destructuring-bind (store-name &rest args &key &allow-other-keys) (canonicalize-store-name store-name)
    (let* ((store (find-store store-name))
           (form (apply #'defspecialization-using-object store specialized-lambda-list value-type body
                        :environment env args)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,form))))

;; DEFINE-SPECIALIZATION

(defmacro define-specialization (store-name specialized-lambda-list value-type &body body &environment env)
  (let* ((store (find-store store-name))
         (form (apply #'define-specialization-using-object store specialized-lambda-list value-type
                      :environment env
                      (mapcan #'(lambda (item)
                                  (if (member (first item) '(:documentation :name :function :expand-function))
                                      (if (= 2 (length item))
                                          item
                                          (error "Invalid specialization definition option: ~W." item))
                                      (list (first item) (rest item))))
                              body))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,form)))
