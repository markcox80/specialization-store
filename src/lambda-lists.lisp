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

;;;; Store Lambda Lists

;;;; Parsing
;;;;
;;;; Implementing this using tagbody definitely produces shorter code
;;;; at the expense of readibility.

(define-condition parse-store-lambda-list-error (error)
  ((original-lambda-list :initarg :original-lambda-list
			 :reader original-lambda-list)
   (message :initarg :message))
  (:report (lambda (condition stream)
	     (with-slots (original-lambda-list message) condition
	       (format stream "Error parsing store lambda list ~W.~%~%~A." original-lambda-list message)))))

(defun throw-parse-store-lambda-list-error (format-control &rest args)
  (throw 'parse-store-lambda-list-error (apply #'format nil format-control args)))

(defun parse-store-lambda-list-error/invalid-item (item)
  (throw-parse-store-lambda-list-error "Encountered invalid item ~W in store lambda list." item))

(defun parse-store-lambda-list/required (fn list)
  (let ((item (first list)))
    (cond
      ((null list)
       nil)
      ((member item '(&optional &rest &key))
       list)
      ((or (listp item) (member item '(&allow-other-keys)))
       (parse-store-lambda-list-error/invalid-item item))
      ((symbolp item)
       (funcall fn :required item)
       (parse-store-lambda-list/required fn (rest list)))
      (t
       (error "Should not get here.")))))

(defun parse-store-lambda-list/optional (fn list)
  (labels ((process (list)
	     (let ((item (first list)))
	       (cond
		 ((null list)
		  nil)
		 ((member item '(&rest &key))
		  list)
		 ((member item '(&optional &allow-other-keys))
		  (parse-store-lambda-list-error/invalid-item item))
		 ((and (listp item) (<= 1 (length item) 2))
		  (destructuring-bind (var &optional init-form) item
		    (cond
		      ((and var (symbolp var))
		       (funcall fn :optional (list var init-form))
		       (process (rest list)))
		      (t
		       (throw-parse-store-lambda-list-error "Invalid optional parameter name ~W." var)))))
		 ((and item (symbolp item))
		  (funcall fn :optional (list item nil))
		  (process (rest list)))
		 (t
		  (parse-store-lambda-list-error/invalid-item item))))))
    (let ((item (first list)))
      (cond
	((null list)
	 list)
	((eql '&optional item)
	 (process (rest list)))
	((member item '(&rest &key))
	 list)
	(t
	 (parse-store-lambda-list-error/invalid-item item))))))

(defun parse-store-lambda-list/rest (fn list)
  (flet ((process (list)
	   (let ((item (first list)))
	     (cond
	       ((null list)
		(throw-parse-store-lambda-list-error "&rest marker requires a symbol."))
	       ((and item (symbolp item))
		(funcall fn :rest item)
		(rest list))
	       (t
		(parse-store-lambda-list-error/invalid-item item))))))
    (let ((item (first list)))
      (cond
	((null list)
	 list)
	((eql '&rest item)
	 (process (rest list)))
	((eql '&key item)
	 list)
	(t
	 (parse-store-lambda-list-error/invalid-item item))))))

(defun parse-store-lambda-list/keys (fn list)
  (let (keywords)
    (labels ((notify (keyword init-form)
	       (cond
		 ((find keyword keywords)
		  (throw-parse-store-lambda-list-error "Found duplicate keyword ~W in store lambda list." keyword))
		 (t
		  (push keyword keywords)
		  (funcall fn :keyword (list keyword init-form)))))
	     (process (list)
	       (let ((item (first list)))
		 (cond
		   ((or (null list) (eql '&allow-other-keys item))
		    list)
		   ((member item '(&rest &optional &key))
		    (parse-store-lambda-list-error/invalid-item item))
		   ((and item (symbolp item))
		    (notify (intern (symbol-name item) "KEYWORD") nil)
		    (process (rest list)))
		   ((and (listp item) (<= 1 (length item) 2))
		    (destructuring-bind (var &optional init-form) item
		      (cond
			((and var (symbolp var))
			 (notify (intern (symbol-name var) "KEYWORD") init-form)
			 (process (rest list)))
			(t
			 (throw-parse-store-lambda-list-error "Invalid keyword parameter name ~W." var)))))
		   (t
		    (parse-store-lambda-list-error/invalid-item item))))))
      (let ((item (first list)))
	(cond	  
	  ((eql '&key item)
	   (funcall fn :keys? t)
	   (process (rest list)))
	  ((null list)
	   list)
	  (t
	   (parse-store-lambda-list-error/invalid-item item)))))))

(defun parse-store-lambda-list/allow-other-keys (fn list)
  (let ((item (first list)))
    (cond
      ((null list)
       list)
      ((eql '&allow-other-keys item)
       (funcall fn :allow-other-keys? t)
       (when (rest list)
	 (throw-parse-store-lambda-list-error "Found elements ~W after &allow-other-keys." (rest list)))
       (rest list))
      (t
       (parse-store-lambda-list-error/invalid-item item)))))

(defclass store-parameters (parameters)
  ())

(defun parse-store-lambda-list (store-lambda-list &optional (errorp t) error-value)
  (let* (required optional rest keys? keywords allow-other-keys?)
    (labels ((process (command value)
	       (ecase command
		 (:required (push value required))
		 (:optional (push value optional))
		 (:rest (setf rest value))
		 (:keys? (setf keys? t))
		 (:keyword (push value keywords))
		 (:allow-other-keys? (setf allow-other-keys? t))))
	     (parse (store-lambda-list)
	       (catch 'parse-store-lambda-list-error
		 (let* ((after-required (parse-store-lambda-list/required #'process store-lambda-list))
			(after-optional (parse-store-lambda-list/optional #'process after-required))
			(after-rest (parse-store-lambda-list/rest #'process after-optional))
			(after-keys (parse-store-lambda-list/keys #'process after-rest))
			(after-allow-other-keys (parse-store-lambda-list/allow-other-keys #'process after-keys)))
		   (assert (null after-allow-other-keys))
		   (make-instance 'store-parameters
				  :original-lambda-list store-lambda-list
				  :required-parameters (nreverse required)
				  :optional-parameters (nreverse optional)
				  :rest-parameter rest
				  :keyword-parameters-p keys?
				  :allow-other-keys-p allow-other-keys?
				  :keyword-parameters (nreverse keywords))))))
      (let ((rv (parse store-lambda-list)))
	(cond
	  ((and (stringp rv) errorp)
	   (error 'parse-store-lambda-list-error :original-lambda-list store-lambda-list :message rv))
	  ((and (stringp rv) (null errorp))
	   (values error-value rv))
	  ((typep rv 'parameters)
	   rv)
	  (t
	   (error "Should not get here.")))))))
