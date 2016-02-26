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
(defgeneric store-name (store))
(defgeneric store-lambda-list (store))
(defgeneric store-documentation (store))

;; The specialization object protocol
(defgeneric specialization-name (specialization))
(defgeneric specialization-function (specialization))
(defgeneric specialization-expand-function (specialization))
(defgeneric specialization-lambda-list (specialization))
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
	     (write-string (slot-value condition 'message) stream))))

;; simple-store-error
(defgeneric simple-store-error-message (store-error))

(define-condition simple-store-error (store-error)
  ((message :initarg :message
            :reader simple-store-error-message
            :initform (error "A value for the slot :message must be specified.")))
  (:report (lambda (condition stream)
	     (write-string (slot-value condition 'message) stream))))

;; invalid-store-name-error
(defgeneric invalid-store-name (invalid-store-name-error))

(define-condition invalid-store-name-error (store-error)
  ((name :initarg :name
         :reader invalid-store-name
         :initform (error "A value for the slot :name must be specified.")))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (format stream "No store exists with name ~W." name)))))

;; inapplicable-arguments-error
(defgeneric inapplicable-arguments (store-error))

(define-condition inapplicable-arguments-error (store-error)
  ((arguments :initarg :arguments
              :reader inapplicable-arguments
              :initform (error "A value for the slot :arguments must be specified.")))
  (:report (lambda (condition stream)
             (with-slots (store arguments) condition
               (format stream "None of the specializations in store object ~W are applicable to the arguments: ~W."
                       store arguments)))))

;; incongruent-specialization-error
(defgeneric incongruent-specialization (store-error))

(define-condition incongruent-specialization-error (store-error)
  ((specialization :initarg :specialization
                   :reader incongruent-specialization
                   :initform (error "A value for the slot :specialization must be specified.")))
  (:report (lambda (condition stream)
             (with-slots (store specialization) condition
               (format stream "The specialized lambda list ~W is not congruent with the store lambda list ~W."
                       (specialization-lambda-list specialization)
                       (store-lambda-list store))))))

;;;; Glue Layer

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

(defgeneric ensure-store-using-class (class store-name lambda-list completion-function form-type-completion-function
                                      &key store-class specialization-class documentation &allow-other-keys))

(defmethod ensure-store-using-class ((class null) store-name lambda-list
                                     completion-function form-type-completion-function
                                     &rest args
				     &key store-class specialization-class documentation &allow-other-keys)
  (declare (ignore specialization-class documentation))
  (ensure-store-using-class (apply #'make-instance
				   (or store-class 'standard-store)
				   :name store-name
				   :lambda-list lambda-list
                                   :completion-function completion-function
                                   :form-type-completion-function form-type-completion-function
				   args)
			    store-name lambda-list completion-function form-type-completion-function))

(defun ensure-store (name store-lambda-list completion-function form-type-completion-function &rest args
                     &key store-class specialization-class documentation
                       &allow-other-keys)
  (declare (ignore store-class specialization-class documentation))
  (let* ((current-store (multiple-value-bind (name indicator) (%find-store-helper name)
                          (get name indicator)))
         (store (apply #'ensure-store-using-class
                       current-store name store-lambda-list completion-function form-type-completion-function
                       args)))
    (setf (find-store name) store)
    store))

(defun make-store-unbound (name)
  (fmakunbound name)
  (setf (compiler-macro-function name) nil)
  (multiple-value-bind (name indicator) (%find-store-helper name)
    (remprop name indicator)))

(defgeneric ensure-specialization-using-class (store-class specialized-lambda-list function &rest args
					       &key expand-function name documentation &allow-other-keys))

(defun ensure-specialization (store-name specialized-lambda-list function
			      &rest args &key expand-function documentation name &allow-other-keys)
  (declare (ignore expand-function documentation name))
  (let* ((store (find-store store-name)))
    (apply #'ensure-specialization-using-class
           store specialized-lambda-list function
           args)))

;; Store Object Requirements for the Glue Layer
(defgeneric store-specialization-class (store))

;;;; Syntax Layer

;; DEFSTORE

(defmacro defstore (store-name store-lambda-list &body body &environment env)
  (let* ((parameters (specialization-store.lambda-lists:parse-store-lambda-list store-lambda-list)))
    (destructuring-bind (rewritten-lambda-list definitions names) (specialization-store.lambda-lists:rewrite-init-forms parameters env)
      (declare (ignore names))
      (let* ((rewritten-parameters (specialization-store.lambda-lists:parse-store-lambda-list rewritten-lambda-list))
             (completion (specialization-store.lambda-lists:make-runtime-completion-lambda-form rewritten-parameters))
             (form-type-completion (specialization-store.lambda-lists:make-form-type-completion-lambda-form parameters env)))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           ;; Introduce global function definitions for initforms
           ,@definitions
           ;; Register the store.
           (ensure-store ',store-name ',rewritten-lambda-list ,completion ,form-type-completion
                         ,@(mapcan #'(lambda (item)
                                       (alexandria:destructuring-case item
                                         ((:documentation doc)
                                          (list :documentation doc))
                                         ((:store-class name)
                                          (list :store-class `',name))
                                         ((:specialization-class name)
                                          (list :specialization-class `',name))
                                         ((t &rest args)
                                          (list (first item) args))))
                                   body)))))))

;; DEFSPECIALIZATION
(defun canonicalize-store-name (store-name)  
  (cond
    ((symbolp store-name)
     (list store-name))
    ((and (listp store-name)
	  (eql 'setf (first store-name)))
     (list store-name))
    (t
     store-name)))

(defmacro defspecialization (store-name specialized-lambda-list &body body)
  (destructuring-bind (store-name &rest args &key inline name &allow-other-keys)
      (canonicalize-store-name store-name)
    (declare (ignore inline))
    (alexandria:remove-from-plistf args :name)
    (multiple-value-bind (body declarations doc-string) (alexandria:parse-body body :documentation t)
      (let* ((store (find-store store-name))
             (store-parameters (specialization-store.lambda-lists:parse-store-lambda-list (store-lambda-list store)))
	     (specialization-parameters (specialization-store.lambda-lists:parse-specialization-lambda-list specialized-lambda-list)))
	`(define-specialization ,store-name ,specialized-lambda-list
	   (:function (lambda ,(specialization-store.lambda-lists:ordinary-lambda-list store-parameters specialization-parameters)
			(declare ,@(specialization-store.lambda-lists:type-declarations store-parameters specialization-parameters))
			,@declarations
			,@body))
	   (:documentation ,doc-string)
	   ,@(when name
		   `((:name ,name)))
	   ,@(loop
		for (key value) on args :by #'cddr
		collect
		  `(,key ,value)))))))

;; DEFINE-SPECIALIZATION
(defun lambda-form-p (form)
  (and (listp form)
       (eql 'lambda (first form))
       (>= (length form) 2)))

(defun function-form-p (form)
  (and (listp form)
       (eql 'function (first form))
       (= 2 (length form))))

(defun function-form->inlined-expand-function (function-form)
  (cond
    ((lambda-form-p function-form)
     (destructuring-bind (lambda-list &rest body) (rest function-form)
       `(compiler-macro-lambda (&rest args)
	  (cons (quote (lambda ,lambda-list
			 ,@body))
		args))))
    ((function-form-p function-form)
     (function-form->inlined-expand-function (second function-form)))
    ((symbolp function-form)
     `(compiler-macro-lambda (&rest args)
	(append (list 'funcall (list 'function ',function-form))
		args)))))

(defun function-form->no-lexical-environment-lambda-form (function-form)
  (cond
    ((lambda-form-p function-form)
     `(compile nil ',function-form))
    ((function-form-p function-form)
     (function-form->no-lexical-environment-lambda-form (second function-form)))
    ((symbolp function-form)
     `(function ,function-form))
    (t
     (error "Invalid function form."))))

(defmacro define-specialization (store-name specialized-lambda-list &body body)
  (let ((function nil)
	(expand-function nil)
	(inline nil)
	(name nil)
        (others nil))
    (dolist (item body)
      (alexandria:destructuring-case item
        ((:function form)
         (setf function form))
	((:expand-function form)
	 (setf expand-function form))
	((:inline value)
	 (check-type value (member nil t))
	 (setf inline value))
	((:name value)
	 (setf name value))
        ((t &rest args)
         (declare (ignore args))
         (push item others))))

    (let ((expand-function (cond
			     ((and (null expand-function) inline)
			      (function-form->inlined-expand-function function))
			     ((and (null expand-function) name)			      
			      `(compiler-macro-lambda (&rest args)
                                 (append (list ',name)
                                         args)))
			     ((and expand-function (null inline))
			      expand-function)
			     ((and (null expand-function) (null inline))
			      nil)
			     (t
			      (warn "Invalid use of :expand-function, :inline and :name in defspecialization.")
			      expand-function)))
	  (function (cond
		      (inline
		       (function-form->no-lexical-environment-lambda-form function))
		      (t
		       function))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (ensure-specialization ',store-name ',specialized-lambda-list ,function
				:expand-function ,expand-function
				:name ',name
				,@(mapcan #'(lambda (item)
					      (list (first item) (second item)))
					  others))))))
