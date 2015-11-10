(in-package "SPECIALIZATION-STORE")

;;;; Object Layer
;; The STORE protocol.

(define-condition store-error (error)
  ((store :initarg :store)
   (message :initarg :message))
  (:report (lambda (condition stream)
	     (write-string (slot-value condition 'message) stream))))

(define-condition no-store-with-name-error (cell-error)
  ()
  (:report (lambda (condition stream)
	     (with-slots (name) condition
	       (format stream "No store exists with name ~A" name)))))

(defgeneric funcall-store (store &rest args)
  (:documentation "Call the most applictable function in the store for the given arguments."))

(defgeneric apply-store (store &rest args)
  (:documentation "The apply equivalent of FUNCALL-STORE. i.e. APPLY-STORE is to FUNCALL-STORE as APPLY is to FUNCALL."))

(defgeneric expand-store (store form &optional env)
  (:documentation "Return code that performs the equivalent of FUNCALL-STORE on the specified args."))

(defgeneric add-specialization (store specialization)
  (:documentation "Add a SPECIALIZATION to the set of specializations
  in STORE."))

(defgeneric remove-specialization (store specialization)
  (:documentation "Remove SPECIALIZATION from the set of specializations in STORE."))

(defgeneric store-specializations (store)
  (:documentation "Return a sequence of specializations used by the STORE."))

(defgeneric (setf store-specializations) (value store)
  (:documentation "Assign a sequence of functions that are to be used
  by the STORE. Should not be called in user code."))

(defgeneric store-name (store)
  (:documentation "The name of the store."))

(defgeneric store-lambda-list (store)
  (:documentation "Return the lambda list for STORE."))

(defgeneric store-documentation (store)
  (:documentation "Return the documentation associated with the store."))

;; The SPECIALIZATION protocol
(defgeneric specialization-name (specialization)
  (:documentation "A name designating the specialization."))

(defgeneric specialization-function (specialization)
  (:documentation "Return the function object rperesenting the
  behavior of the specialization."))

(defgeneric specialization-expand-function (specialization)
  (:documentation "Return a function which accepts two arguments, FORM
  and ENVIRONMENT, performs a specialization macro expansion on FORM
  using the given ENVIRONMENT. If no expansion is possible, the
  function should return FORM."))

(defgeneric specialization-equal (specialization-a specializabion-b)
  (:documentation "Return non-NIL if SPECIALIZATION-A is equal to SPECIALIZATION-B."))

(defgeneric specialization-lambda-list (specialization)
  (:documentation "Return the lambda list for the store function."))

(defgeneric specialization-documentation (specialization)
  (:documentation "Return the documentation for the specialization."))

(defgeneric (setf specialization-documentation) (value specialization)
  (:documentation "Change the documentation for the specialization."))

;;;; Glue Layer

(defun %find-store-helper (name)
  "Return the name and property indicator for the STORE name."
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
       (error "Invalid store name ~A." name)))))

(defun find-store (name)
  (multiple-value-bind (name indicator) (%find-store-helper name)
    (let ((store (get name indicator)))
      (if store
	  store
	  (error 'no-store-with-name-error :name name)))))

(defun (setf find-store) (value name)
  (multiple-value-bind (name indicator) (%find-store-helper name)
    (setf (get name indicator) value)))

(defun ensure-store-only (name store-lambda-list
			  &rest args
			  &key store-class specialization-class documentation
			    &allow-other-keys)
  (let* ((store-class (or store-class (find-class 'standard-store)))
         (specialization-class (or specialization-class (find-class 'standard-specialization)))
         (store (apply #'make-instance store-class
		       :name name
                       :lambda-list store-lambda-list
                       :specialization-class specialization-class
                       :documentation documentation
                       args)))
    (setf (find-store name) store)
    store))

(defun ensure-store (name store-lambda-list &rest args
                     &key store-class specialization-class documentation
                       &allow-other-keys)
  (declare (ignore store-class specialization-class))
  (let ((store (apply #'ensure-store-only name store-lambda-list args)))
    (setf (fdefinition name) #'(lambda (&rest args)
				 (apply-store (find-store name) args))
	  (compiler-macro-function name) #'(lambda (form env)
					     (expand-store (find-store name) form env))
	  ;; Documentation must be set last as (setf fdefinition) may
	  ;; clear the documentation for the function.
	  (documentation name 'function) documentation)
    store))

(defun ensure-specialization (store-name specialized-lambda-list function
			      &rest args &key expand-function documentation inline name &allow-other-keys)
  (alexandria:remove-from-plistf args :inline)
  (let* ((store (find-store store-name))
         (specialization-class (store-specialization-class store))
         (specialization (apply #'make-instance specialization-class
				:lambda-list specialized-lambda-list
				:function function
				args)))
    (add-specialization store specialization)
    
    (when name
      (setf (fdefinition name) function
	    (compiler-macro-function name) (if inline
					       expand-function
					       nil)
	    (documentation name 'function) documentation))
    specialization))

;; Store Object Requirements for the Glue Layer
(defgeneric store-specialization-class (store)
  (:documentation "Return the class of the store functions used by this store."))


;;;; Syntax Layer

;; DEFSTORE

(defstruct (defstore-defun (:conc-name defstore-defun-) (:constructor %make-defstore-defun))
  lambda-list apply-arguments variable-list)

(defun make-defstore-defun (store-lambda-list)
  (let* ((parameters (specialization-store.lambda-lists:parse-store-lambda-list store-lambda-list))
	 (parameters-required (specialization-store.lambda-lists:required-parameters parameters))
	 (parameters-optional (specialization-store.lambda-lists:optional-parameters parameters))
	 (parameters-rest-parameter (specialization-store.lambda-lists:rest-parameter parameters))
	 (parameters-keyword-arguments-p (specialization-store.lambda-lists:keyword-parameters-p parameters))
	 (parameters-keywords (specialization-store.lambda-lists:keyword-parameters parameters))
	 (parameters-allow-other-keys-p (specialization-store.lambda-lists:allow-other-keys-p parameters))
	 (rest-parameter (cond
			   (parameters-rest-parameter
			    parameters-rest-parameter)
			   (parameters-keyword-arguments-p
			    (gensym "KEYWORD-ARGS"))
			   (t
			    nil)))
	 (lambda-list (append parameters-required
			      (when parameters-optional
				`(&optional ,@parameters-optional))
			      (when rest-parameter
				`(&rest ,rest-parameter))
			      (when parameters-keyword-arguments-p
				`(&key ,@parameters-keywords))
			      (when parameters-allow-other-keys-p
				`(&allow-other-keys))))
	 (apply-arguments (append parameters-required
				  (mapcar #'first parameters-optional)
				  (cond
				    (parameters-keyword-arguments-p
				     (append (mapcar #'second parameters-keywords)
					     (list `(alexandria:delete-from-plist ,rest-parameter
										  ,@(mapcar #'first parameters-keywords)))))
				    (t
				     (list rest-parameter)))))
	 (variable-list (append parameters-required
				(mapcar #'first parameters-optional)
				(when rest-parameter
				  (list rest-parameter))
				(when parameters-keywords
				  (mapcar #'second parameters-keywords)))))
    (%make-defstore-defun :lambda-list lambda-list
			  :apply-arguments apply-arguments
			  :variable-list variable-list)))

(defmacro defstore (store-name store-lambda-list &body body)
  (let ((defstore-defun-data (make-defstore-defun store-lambda-list))
	documentation
	(form-var (gensym "FORM"))
	(env-var (gensym "ENV")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ;; Register the store.
       (ensure-store-only ',store-name ',store-lambda-list
			  ,@(mapcan #'(lambda (item)
					(alexandria:destructuring-case item
					  ((:documentation doc)
					   (setf documentation doc)
					   (list :documentation doc))
					  ((:store-class name)
					   (list :store-class `',name))
					  ((:specialization-class name)
					   (list :specialization-class `',name))
					  ((t &rest args)
					   (list (first item) args))))
				    body))
       
       ;; Install a function which invokes APPLY-STORE.
       (defun ,store-name ,(defstore-defun-lambda-list defstore-defun-data)
	 ,documentation
	 (apply-store (find-store ',store-name) ,@(defstore-defun-apply-arguments defstore-defun-data)))

       ;; Install a compiler macro which invokes EXPAND-STORE.
       (define-compiler-macro ,store-name (&whole ,form-var ,@store-lambda-list &environment ,env-var)
	 (declare (ignore ,@(defstore-defun-variable-list defstore-defun-data)))
	 (expand-store (find-system ',store-name) ,form-var ,env-var)))))

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
      (let* ((store-parameters (specialization-store.lambda-lists:parse-store-lambda-list (store-lambda-list store-name)))
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
				 (append (list 'funcall (list 'function ',name))
					 args)))
			     (expand-function
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
	 (ensure-specialization ',store-name ,specialized-lambda-list ,function
				:expand-function ,expand-function
				:name ',name
				:inline ,inline
				,@(mapcan #'(lambda (item)
					      (list (first item) (second item)))
					  others))))))
