(in-package "SPECIALIZATION-STORE.LAMBDA-LISTS")

;;;; The code in this file provides operations for working with the
;;;; different types of lambda lists used in the specialization store
;;;; system.

;;;; Parameters Protocol

;; Properties
(defgeneric original-lambda-list (parameters))
(defgeneric required-parameters (parameters))
(defgeneric optional-parameters (parameters))
(defgeneric rest-parameter (parameter))
(defgeneric keyword-parameters-p (parameters))
(defgeneric allow-other-keys-p (parameters))
(defgeneric keyword-parameters (parameters))

;; Operations
(defgeneric parameters-equal (parameters-1 parameters-2))

(defclass parameters ()
  ((original-lambda-list :initarg :original-lambda-list
			 :reader original-lambda-list)
   (required-parameters :initarg :required-parameters
			:reader required-parameters)
   (optional-parameters :initarg :optional-parameters
			:reader optional-parameters)
   (rest-parameter :initarg :rest-parameter
		   :reader rest-parameter)
   (keyword-parameters-p :initarg :keyword-parameters-p
			 :reader keyword-parameters-p)
   (allow-other-keys-p :initarg :allow-other-keys-p
		       :reader allow-other-keys-p)
   (keyword-parameters :initarg :keyword-parameters
		       :reader keyword-parameters)))

(defmethod print-object ((object parameters) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (original-lambda-list object) :stream stream :pretty t)))


;;;; Parsing an ordinary lambda list
;;;;
;;;; An ORDINARY-LAMBDA-LIST is not the common lisp ordinary lambda
;;;; list. This code is used by parse-store-lambda-list and
;;;; parse-specialization-lambda-list.

(defvar *lambda-list-description*)

(defun throw-ordinary-lambda-list-error (control-string &rest args)
  (throw 'ordinary-lambda-list-error (apply #'format nil control-string args)))

(defun invalid-ordinary-lambda-list-item (item)
  (throw-ordinary-lambda-list-error "Invalid item ~W in ~A lambda list." item *lambda-list-description*))

(defun parse-ordinary-lambda-list/required (fn list)
  (let ((item (first list)))
    (cond
      ((null list)
       nil)
      ((member item '(&optional &rest &key))
       list)      
      ((or (null item) (eql item '&allow-other-keys))
       (invalid-ordinary-lambda-list-item item))
      (t
       (funcall fn :required item)
       (parse-ordinary-lambda-list/required fn (rest list))))))

(defun parse-ordinary-lambda-list/optional (fn list)
  (labels ((process (list)
	     (let ((item (first list)))
	       (cond
		 ((null list)
		  nil)
		 ((member item '(&rest &key))
		  list)
		 ((or (null item) (member item '(&optional &allow-other-keys)))
		  (invalid-ordinary-lambda-list-item item))
		 (t
		  (funcall fn :optional item)
		  (process (rest list)))))))
    (let ((item (first list)))
      (cond
	((null list)
	 list)
	((eql '&optional item)
	 (process (rest list)))
	((member item '(&rest &key))
	 list)
	(t
	 (invalid-ordinary-lambda-list-item item))))))

(defun parse-ordinary-lambda-list/rest (fn list)
  (flet ((process (list)
	   (let ((item (first list)))
	     (cond
	       ((null list)
		(throw-ordinary-lambda-list-error "&rest marker requires a symbol."))
	       ((or (null item) (listp item) (member item '(&optional &rest &key &allow-other-keys)))
		(invalid-ordinary-lambda-list-item item))
	       (t
		(funcall fn :rest item)
		(rest list))))))
    (let ((item (first list)))
      (cond
	((null list)
	 list)
	((eql '&rest item)
	 (process (rest list)))
	((eql '&key item)
	 list)
	(t
	 (invalid-ordinary-lambda-list-item item))))))

(defun parse-ordinary-lambda-list/keys (fn list)
  (labels ((process (list)
	     (let ((item (first list)))
	       (cond
		 ((or (null list) (eql '&allow-other-keys item))
		  list)
		 ((or (null item) (member item '(&rest &optional &key)))
		  (invalid-ordinary-lambda-list-item item))
		 (t
		  (funcall fn :keyword item)
		  (process (rest list)))))))
    (let ((item (first list)))
      (cond	  
	((eql '&key item)
	 (funcall fn :keys? t)
	 (process (rest list)))
	((null list)
	 list)
	(t
	 (invalid-ordinary-lambda-list-item item))))))

(defun parse-ordinary-lambda-list/allow-other-keys (fn list)
  (let ((item (first list)))
    (cond
      ((null list)
       list)
      ((eql '&allow-other-keys item)
       (funcall fn :allow-other-keys? t)
       (when (rest list)
	 (throw-ordinary-lambda-list-error "Found elements ~W after &allow-other-keys." (rest list)))
       (rest list))
      (t
       (invalid-ordinary-lambda-list-item item)))))

(defun parse-ordinary-lambda-list (class-name function ordinary-lambda-list)
  (catch 'ordinary-lambda-list-error
    (let (required optional rest keys? keywords allow-other-keys?)
      (flet ((process (what value)
	       (ecase what
		 (:required (push (funcall function what value) required))
		 (:optional (push (funcall function what value) optional))
		 (:rest (setf rest value))
		 (:keys? (setf keys? t))
		 (:keyword (push (funcall function what value) keywords))
		 (:allow-other-keys? (setf allow-other-keys? t)))))
	(let* ((after-required (parse-ordinary-lambda-list/required #'process ordinary-lambda-list))
	       (after-optional (parse-ordinary-lambda-list/optional #'process after-required))
	       (after-rest (parse-ordinary-lambda-list/rest #'process after-optional))
	       (after-keys (parse-ordinary-lambda-list/keys #'process after-rest))
	       (after-allow-other-keys (parse-ordinary-lambda-list/allow-other-keys #'process after-keys)))
	  (assert (null after-allow-other-keys))
	  (make-instance class-name
			 :original-lambda-list ordinary-lambda-list
			 :required-parameters (nreverse required)
			 :optional-parameters (nreverse optional)
			 :rest-parameter rest
			 :keyword-parameters-p keys?
			 :keyword-parameters (nreverse keywords)
			 :allow-other-keys-p allow-other-keys?))))))

;;;; Store Lambda Lists

;;;; Parsing

(define-condition parse-store-lambda-list-error (error)
  ((original-lambda-list :initarg :original-lambda-list
			 :reader original-lambda-list)
   (message :initarg :message))
  (:report (lambda (condition stream)
	     (with-slots (original-lambda-list message) condition
	       (format stream "Error parsing store lambda list ~W.~%~%~A." original-lambda-list message)))))

(defun throw-store-lambda-list-error (control-string &rest arguments)
  (apply #'throw-ordinary-lambda-list-error control-string arguments))

(defclass store-parameters (parameters)
  ())

(defun parse-store-lambda-list (store-lambda-list &optional (errorp t) error-value)
  (labels ((process (command value)
	     (case command 
	       (:required (if (symbolp value)
			      value
			      (throw-store-lambda-list-error "Invalid required parameter name ~W." value)))
	       (:optional (cond ((symbolp value)
				 (list value nil))
				((and (listp value) (<= 1 (length value) 2))
				 (destructuring-bind (var &optional init-form) value
				   (cond ((null var)
					  (throw-store-lambda-list-error "Invalid optional parameter specification ~W." value))
					 (t
					  (list var init-form)))))
				(t
				 (throw-store-lambda-list-error "Invalid optional parameter specification ~W." value))))
	       (:keyword (cond ((symbolp value)
				(list (intern (symbol-name value) "KEYWORD") nil))
			       ((and (listp value) (<= 1 (length value) 2))
				(destructuring-bind (name &optional init-form) value
				  (cond ((and name (symbolp name))
					 (list (intern (symbol-name name) "KEYWORD") init-form))
					(t
					 (throw-store-lambda-list-error "Invalid keyword parameter specification." value)))))
			       (t
				(throw-store-lambda-list-error "Invalid keyword parameter specification ~W." value)))))))
    (let* ((*lambda-list-description* "store-lambda-list")
	   (rv (parse-ordinary-lambda-list 'store-parameters #'process store-lambda-list)))
      (cond
	((and (stringp rv) errorp)
	 (error 'parse-store-lambda-list-error :original-lambda-list store-lambda-list :message rv))
	((and (stringp rv) (null errorp))
	 (values error-value rv))
	((typep rv 'store-parameters)
	 rv)
	(t
	 (error "Should not get here."))))))


;;;; Specialization Lambda Lists

(define-condition parse-specialization-lambda-list-error (error)
  ((lambda-list :initarg :lambda-list)
   (message :initarg :message))
  (:report (lambda (condition stream)
	     (with-slots (lambda-list message) condition
	       (format stream "Error parsing specialization lambda list ~W.~%~%~A." lambda-list message)))))

(defun throw-parse-specialization-lambda-list-error (control-string &rest arguments)
  (apply #'throw-ordinary-lambda-list-error control-string arguments))

(defclass specialization-parameters (parameters)
  ())

(defun parse-specialization-lambda-list (specialization-lambda-list &optional (errorp t) error-value)
  (labels ((process (what value)
	     (ecase what
	       (:required (cond ((symbolp value)
				 (list value t))
				((and (listp value) (<= 1 (length value) 2))
				 (destructuring-bind (name &optional (type t)) value
				   (list name type)))
				(t
				 (throw-parse-specialization-lambda-list-error "Invalid required parameter specification ~W." value))))
	       (:optional (cond ((symbolp value)
				 (list value nil nil))
				((and (listp value) (<= 1 (length value) 3))
				 (destructuring-bind (var &optional init-form (supplied-p-var nil supplied-p-var?)) value
				   (when (not (and var
						   (or (not supplied-p-var?)
						       (and supplied-p-var supplied-p-var?))))
				     (throw-parse-specialization-lambda-list-error "Invalid optional parameter specification ~W." value))
				   (list var init-form supplied-p-var)))))
	       (:keyword (cond ((symbolp value)
				(list (intern (symbol-name value) "KEYWORD")
				      value nil nil))
			       ((and (listp value) (<= 1 (length value) 3))
				(destructuring-bind (var &optional init-form (supplied-p-var nil supplied-p-var?)) value
				  (when (not (and (or (and var (symbolp var))
						      (and (listp var) (= 2 (length var))
							   (first var) (second var)))
						  (or (not supplied-p-var?)
						      (and supplied-p-var supplied-p-var?))))
				    (throw-parse-specialization-lambda-list-error "Invalid keyword parameter specification ~W."
										  value))
				  (destructuring-bind (keyword var) (if (listp var)
									var
									(list (intern (symbol-name var) "KEYWORD")
									      var))
				    (list keyword var init-form supplied-p-var)))))))))
    (let* ((*lambda-list-description* "specialization-lambda-list")
	   (rv (parse-ordinary-lambda-list 'specialization-parameters #'process specialization-lambda-list)))
      (cond
	((and (stringp rv) errorp)
	 (error 'parse-specialization-lambda-list-error :lambda-list specialization-lambda-list :message rv))
	((and (stringp rv) (null errorp))
	 (values error-value rv))
	((typep rv 'specialization-parameters)
	 rv)
	(t
	 (error "Should not get here."))))))
