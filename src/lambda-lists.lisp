(in-package "SPECIALIZATION-STORE.LAMBDA-LISTS")

;;;; The code in this file provides operations for working with the
;;;; different types of lambda lists used in the specialization store
;;;; system.


;;;; Helpers

(defun filter-duplicates (list &key (test #'eql) (key #'identity))
  (loop
     with duplicates = nil
     with processed = nil
     for item in list
     do
       (cond ((find item processed :test test :key key)
	      (pushnew item duplicates :test test :key key))
	     (t
	      (push item processed)))
     finally (return duplicates)))

;;;; Parameters Protocol

;; Properties
(defgeneric original-lambda-list (parameters))
(defgeneric required-parameters (parameters))
(defgeneric optional-parameters (parameters))
(defgeneric rest-parameter (parameter))
(defgeneric keyword-parameters-p (parameters))
(defgeneric allow-other-keys-p (parameters))
(defgeneric keyword-parameters (parameters))
(defgeneric positional-parameters-lower-bound (parameters))
(defgeneric positional-parameters-upper-bound (parameters))

;; Operations
(defgeneric parameters-equal (parameters-1 parameters-2))
(defgeneric duplicate-keywords-p (parameters))
(defgeneric duplicate-variables-p (parameters))

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

(defmethod positional-parameters-lower-bound ((parameters parameters))
  (length (required-parameters parameters)))

(defmethod positional-parameters-upper-bound ((parameters parameters))
  (cond
    ((and (rest-parameter parameters) (not (keyword-parameters-p parameters)))
     lambda-parameters-limit)
    (t
     (+ (length (required-parameters parameters))
	(length (optional-parameters parameters))))))

;;;; Parsing an ordinary lambda list
;;;;
;;;; An ORDINARY-LAMBDA-LIST is not the common lisp ordinary lambda
;;;; list. This code is used by parse-store-lambda-list and
;;;; parse-specialization-lambda-list.

(defvar *lambda-list*)
(defvar *lambda-list-description*)
(defvar *parse-lambda-list-error-class*)

(define-condition parse-lambda-list-error (error)
  ((message :initarg :message)
   (lambda-list-description :initarg :lambda-list-description)
   (lambda-list :initarg :lambda-list))
  (:default-initargs
   :lambda-list *lambda-list*
   :lambda-list-description *lambda-list-description*)
  (:report (lambda (condition stream)
	     (with-slots (lambda-list-description lambda-list message) condition
	       (format stream "Error parsing ~A ~W.~%~%~A." lambda-list-description lambda-list message)))))

(defun parse-lambda-list-error-message (parse-lambda-list-error)
  (slot-value parse-lambda-list-error 'message))

(defun parse-lambda-list-error-lambda-list (parse-lambda-list-error)
  (slot-value parse-lambda-list-error 'lambda-list))

(defun signal-parse-lambda-list-error (control-string &rest args)
  (signal *parse-lambda-list-error-class* :message (apply #'format nil control-string args)))

(defun invalid-ordinary-lambda-list-item (item)
  (signal-parse-lambda-list-error "Invalid item ~W in ~A lambda list." item *lambda-list-description*))

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
		(signal-parse-lambda-list-error "&rest marker requires a symbol."))
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
	 (signal-parse-lambda-list-error "Found elements ~W after &allow-other-keys." (rest list)))
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

(define-condition parse-store-lambda-list-error (parse-lambda-list-error)
  ())

(defclass store-parameters (parameters)
  ())

(defmethod duplicate-keywords-p ((parameters store-parameters))
  (let ((keywords (mapcar #'first (keyword-parameters parameters))))
    (loop
       with duplicates = nil
       with processed = nil
       for keyword in keywords
       do
	 (cond ((find keyword processed)
		(pushnew keyword duplicates))
	       (t
		(push keyword processed)))
       finally (return duplicates))))

(defun parse-store-lambda-list (store-lambda-list)
  (labels ((process (command value)
	     (case command 
	       (:required (if (symbolp value)
			      value
			      (signal-parse-lambda-list-error "Invalid required parameter name ~W." value)))
	       (:optional (cond ((symbolp value)
				 (list value nil))
				((and (listp value) (<= 1 (length value) 2))
				 (destructuring-bind (var &optional init-form) value
				   (cond ((null var)
					  (signal-parse-lambda-list-error "Invalid optional parameter specification ~W." value))
					 (t
					  (list var init-form)))))
				(t
				 (signal-parse-lambda-list-error "Invalid optional parameter specification ~W." value))))
	       (:keyword (cond ((symbolp value)
				(list (intern (symbol-name value) "KEYWORD")
				      value
				      nil))
			       ((and (listp value) (<= 1 (length value) 2))
				(destructuring-bind (name &optional init-form) value
				  (cond ((and name (symbolp name))
					 (list (intern (symbol-name name) "KEYWORD") name init-form))
					((and name (listp name))
					 (destructuring-bind (keyword var) name
					   (list keyword var init-form)))
					(t
					 (signal-parse-lambda-list-error "Invalid keyword parameter specification." value)))))
			       (t
				(signal-parse-lambda-list-error "Invalid keyword parameter specification ~W." value)))))))
    (let* ((*lambda-list* store-lambda-list)
	   (*lambda-list-description* "store-lambda-list")
	   (*parse-lambda-list-error-class* 'parse-store-lambda-list-error)
	   (rv (parse-ordinary-lambda-list 'store-parameters #'process store-lambda-list))
	   (duplicate-keywords (duplicate-keywords-p rv)))
      (when duplicate-keywords
	(signal-parse-lambda-list-error "The keywords ~W are used more than once in the store lambda list." duplicate-keywords))
      rv)))

;;;; Specialization Lambda Lists

(define-condition parse-specialization-lambda-list-error (parse-lambda-list-error)
  ())

(defclass specialization-parameters (parameters)
  ())

(defmethod duplicate-keywords-p ((parameters specialization-parameters))
  (let ((keywords (mapcar #'first (keyword-parameters parameters))))
    (filter-duplicates keywords)))

(defmethod duplicate-variables-p ((parameters specialization-parameters))
  (let ((variables (append (mapcar #'first (required-parameters parameters))
			   (mapcar #'first (optional-parameters parameters))
			   (mapcar #'third (optional-parameters parameters))
			   (alexandria:when-let ((v (rest-parameter parameters)))
			     (list v))
			   (mapcar #'second (keyword-parameters parameters))
			   (mapcar #'fourth (keyword-parameters parameters)))))
    (filter-duplicates variables)))

(defun parse-specialization-lambda-list (specialization-lambda-list)
  (labels ((process (what value)
	     (ecase what
	       (:required (cond ((symbolp value)
				 (list value t))
				((and (listp value) (<= 1 (length value) 2))
				 (destructuring-bind (name &optional (type t)) value
				   (list name type)))
				(t
				 (signal-parse-lambda-list-error "Invalid required parameter specification ~W." value))))
	       (:optional (cond ((symbolp value)
				 (list value nil nil))
				((and (listp value) (<= 1 (length value) 3))
				 (destructuring-bind (var &optional init-form (supplied-p-var nil supplied-p-var?)) value
				   (when (not (and var
						   (or (not supplied-p-var?)
						       (and supplied-p-var supplied-p-var?))))
				     (signal-parse-lambda-list-error "Invalid optional parameter specification ~W." value))
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
				    (signal-parse-lambda-list-error "Invalid keyword parameter specification ~W." value))
				  (destructuring-bind (keyword var) (if (listp var)
									var
									(list (intern (symbol-name var) "KEYWORD")
									      var))
				    (list keyword var init-form supplied-p-var)))))))))
    (let* ((*lambda-list* specialization-lambda-list)
	   (*lambda-list-description* "specialization-lambda-list")
	   (*parse-lambda-list-error-class* 'parse-specialization-lambda-list-error)
	   (rv (parse-ordinary-lambda-list 'specialization-parameters #'process specialization-lambda-list))
	   (duplicate-keywords (duplicate-keywords-p rv))
	   (duplicate-variables (duplicate-variables-p rv)))
      (when duplicate-keywords
	(signal-parse-lambda-list-error "The keywords ~W are used more than once in the specialization lambda list." duplicate-keywords))
      (when duplicate-variables
	(signal-parse-lambda-list-error "The variables ~W appear more than once in the specialization lambda list." duplicate-variables))
      rv)))

;;;; Congruency
;;
;; This section contains the code for establishing if a specialization
;; lambda list is congruent with a store lambda list. The predicate
;; congruent-lambda-list-p is responsible for this task. If a
;; specialization lambda list is congruent with a store lambda list
;; then an object of type CONGRUENCE is returned. Otherwise NIL.
;;
;; The reason a object with the class CONGRUENCE is returned is that
;; these objects are used to order the specializations.

;;;; Congruence Protocol

(defgeneric store-parameters (congruence))
(defgeneric specialization-parameters (congruence))

;;;; Congruence Operations
(defgeneric congruent-lambda-list-p (store-lambda-list specialization-lambda-list))

;;;; Congruence Class

(defclass congruence ()
  ((store-parameters :initarg :store-parameters
		     :reader store-parameters)
   (specialization-parameters :initarg :specialization-parameters
			      :reader specialization-parameters)))

(defmethod congruent-lambda-list-p ((store-lambda-list list) specialization-lambda-list)
  (congruent-lambda-list-p (parse-store-lambda-list store-lambda-list) specialization-lambda-list))

(defmethod congruent-lambda-list-p (store-lambda-list (specialization-lambda-list list))
  (congruent-lambda-list-p store-lambda-list (parse-specialization-lambda-list specialization-lambda-list)))

(defmethod congruent-lambda-list-p ((store store-parameters) (specialization specialization-parameters))
  (and (<= (+ (length (required-parameters store))
	      (length (optional-parameters store)))
	   (length (required-parameters specialization))
	   (positional-parameters-upper-bound specialization)
	   (positional-parameters-upper-bound store))
       (cond ((keyword-parameters-p store)
	      (and (keyword-parameters specialization)
		   ;; All keyword parameters in the store must be
		   ;; present in the specialization and must be in the
		   ;; type form.
		   (loop
		      with st-keys = (keyword-parameters store)
		      with sp-keys = (keyword-parameters specialization)
		      for (st-key-name nil) in st-keys
		      for sp-key = (find st-key-name sp-keys :key #'first)
		      always
			sp-key)))
	     (t
	      (not (keyword-parameters-p specialization))))
       (make-instance 'congruence
		      :store-parameters store
		      :specialization-parameters specialization)))


;;;; Rewriting lexical functions as global functions

(defun rewrite-form-p (form environment)
  (cond ((and form
              (symbolp form)
              (eql :symbol-macro (introspect-environment:variable-information form environment)))
         t)
        ((atom form)
         nil)
        (t t)))

(defgeneric rewrite-init-forms (parameters environment)
  (:documentation "Ensure all init forms are either constant or invoke global functions."))

(defun random-init-function-name ()
  (let ((symbol (gensym "INIT-FUNCTION")))
    (multiple-value-bind (symbol status) (intern (symbol-name symbol) "SPECIALIZATION-STORE.GLOBALS")
      (if status
          (random-init-function-name)
          symbol))))

(defmethod rewrite-init-forms ((parameters store-parameters) environment)
  (labels ((rewrite (fn vars parameters)
             (loop
                for parameter in parameters
                for (var data name definition) = (funcall fn vars parameter)
                do
                  (alexandria:appendf vars (list var))
                collect data into new-data
                when name collect name into names
                when definition collect definition into definitions
                finally (return (list new-data definitions names))))
           (rewrite-optional (required optional)
             (rewrite #'(lambda (vars optional)
                          (destructuring-bind (var init-form) optional
                            (cond
                              ((rewrite-form-p init-form environment)
                               (let ((name (random-init-function-name)))
                                 (list var `(,var (,name ,@vars))
                                       name `(defun ,name ,vars
                                               (declare (ignorable ,@vars))
                                               ,init-form))))
                              (t
                               (list var optional)))))
                      required optional))
           (rewrite-keywords (required optional rest keywords)
             (rewrite #'(lambda (vars keyword)
                          (destructuring-bind (keyword-name var init-form) keyword
                            (cond
                              ((rewrite-form-p init-form environment)
                               (let ((name (random-init-function-name)))
                                 (list var `(,keyword-name ,var (,name ,@vars))
                                       name `(defun ,name ,vars
                                               (declare (ignorable ,@vars))
                                               ,init-form))))
                              (t
                               (list var keyword)))))
                      (append required
                              (mapcar #'first optional)
                              (when rest
                                (list rest)))
                      keywords)))
    (let* ((required (required-parameters parameters))
           (optional (optional-parameters parameters))
           (rest (rest-parameter parameters))
           (keywordsp (keyword-parameters-p parameters))
           (keywords (keyword-parameters parameters))
           (allow-other-keys? (allow-other-keys-p parameters)))
      (destructuring-bind (optional-data optional-definitions optional-names) (rewrite-optional required optional)
        (destructuring-bind (keyword-data keyword-definitions keyword-names) (rewrite-keywords required optional rest keywords)
          (let* ((new-lambda-list (append required
                                          (when optional
                                            `(&optional ,@optional-data))
                                          (when keywordsp
                                            `(&key ,@ (loop
                                                         for (key var init-form) in keyword-data
                                                         collect `((,key ,var) ,init-form))))
                                          (when allow-other-keys?
                                            '(&allow-other-keys))))
                 (global-functions (append optional-definitions keyword-definitions))
                 (global-names (append optional-names keyword-names)))
            (list new-lambda-list
                  global-functions
                  global-names)))))))


;;;; Lambda list conversions
(defgeneric ordinary-lambda-list (store-parameters specialization-parameters))
(defgeneric type-declarations (store-parameters specialization-parameters))
(defgeneric make-runtime-completion-lambda-form (parameters))
(defgeneric make-form-type-completion-lambda-form (parameters environment))

(defmethod ordinary-lambda-list ((store-parameters store-parameters) (specialization-parameters specialization-parameters))
  (append (mapcar #'first (required-parameters specialization-parameters))
	  (optional-parameters specialization-parameters)
	  (when (rest-parameter specialization-parameters)
	    `(&rest ,(rest-parameter specialization-parameters)))
	  (when (keyword-parameters-p specialization-parameters)
	    `(&key ,(loop
		       with store-keyword-parameters = (keyword-parameters store-parameters)
		       for (keyword var form supplied-p-var) in (keyword-parameters specialization-parameters)
		       for init-form = (if (find keyword store-keyword-parameters :key #'first)
					   nil
					   form)
		       collect `((,keyword ,var) ,init-form ,supplied-p-var))))
	  (when (allow-other-keys-p specialization-parameters)
	    `(&allow-other-keys))))

(defmethod type-declarations (store-parameters specialization-parameters)
  (append (loop
	     for (var type) in (required-parameters specialization-parameters)
	     collect `(type ,type ,var))
	  (loop
	     with store-keyword-parameters = (keyword-parameters store-parameters)
	     for (keyword var form supplied-p-var) in (keyword-parameters specialization-parameters)
	     when (find keyword store-keyword-parameters :key #'first)
	     append `((type ,form ,var) (type (eql t) ,supplied-p-var)))))

(defmethod make-runtime-completion-function ((parameters store-parameters))
  (let* ((original-lambda-list (original-lambda-list parameters))
         (required (required-parameters parameters))
         (optional (optional-parameters parameters))
         (positional-vars (append (required-parameters parameters)
                                  (mapcar #'first (optional-parameters parameters))))
         (rest (rest-parameter parameters))
         (keywordsp (keyword-parameters-p parameters))
         (keywords (keyword-parameters parameters))
         (keyword-vars (loop
                          for (keyword var nil) in keywords
                          append (list keyword var)))
         (allow-other-keys (allow-other-keys-p parameters))
         (continuation (gensym "CONTINUATION")))
    (cond
      ((and keywordsp allow-other-keys)
       (let* ((rest (or rest (gensym "REST")))
              (lambda-list `(,@required &optional ,@optional &rest ,rest &key ,@keywords &allow-other-keys)))
         `(lambda (,continuation ,@lambda-list)
            (apply ,continuation ,@positional-vars ,@keyword-vars ,rest))))
      (keywords
       `(lambda (,continuation ,@original-lambda-list)
          (funcall ,continuation ,@positional-vars ,@keyword-vars)))
      (rest
       `(lambda (,continuation ,@original-lambda-list)
          (apply ,continuation ,@positional-vars ,rest)))
      (t
       `(lambda (,continuation ,@original-lambda-list)
          (funcall ,continuation ,@positional-vars))))))

(defmethod make-form-type-completion-lambda-form ((parameters store-parameters) environment)
  (let* ((continuation (gensym "CONTINUATION"))
         (lambda-environment (gensym "ENVIRONMENT"))
         (required (required-parameters parameters))
         (optional (optional-parameters parameters))
         (optional-supplied-p (alexandria:make-gensym-list (length optional)))
         (optional-lambda-list (loop
                                  for (var init-form) in optional
                                  for suppliedp in optional-supplied-p
                                  for init-form-type = (specialization-store:determine-form-type init-form environment)
                                  collect `(,var ',init-form-type ,suppliedp)))
         (positional-vars (append (loop
                                     for var in required
                                     collect `(specialization-store:determine-form-type ,var ,lambda-environment))
                                  (loop
                                     for (var init-form suppliedp-var) in optional-lambda-list
                                     collect `(if ,suppliedp-var
                                                  (specialization-store:determine-form-type ,var ,lambda-environment)
                                                  ,var))))
         (positional-lambda-list (append required `(&optional ,@optional-lambda-list)))
         (rest (rest-parameter parameters))
         (keywordsp (keyword-parameters-p parameters))
         (keywords (keyword-parameters parameters))
         (keywords-supplied-p (alexandria:make-gensym-list (length keywords)))
         (keywords-lambda-list (loop
                                  for (keyword var init-form) in keywords
                                  for suppliedp in keywords-supplied-p
                                  for init-form-type = (specialization-store:determine-form-type init-form environment)
                                  collect `((,keyword ,var) ',init-form-type ,suppliedp)))
         (keywords-vars (loop
                           for ((keyword var) init-form suppliedp) in keywords-lambda-list
                           collect `(if ,suppliedp
                                        (specialization-store:determine-form-type ,var ,lambda-environment)
                                        ,var)))
         (allow-other-keys (allow-other-keys-p parameters)))
    (cond
      ((and keywordsp allow-other-keys)
       (let* ((rest (or rest (gensym "REST")))
              (lambda-list `(,@positional-lambda-list &rest ,rest &key ,@keywords-lambda-list &allow-other-keys)))
         `(lambda (,continuation ,lambda-environment ,@lambda-list)
            (alexandria:remove-from-plistf ,rest ,@(mapcar #'first keywords))
            (apply ,continuation ,lambda-environment ,@positional-vars ,@keywords-vars ,rest))))
      (keywordsp
       (let* ((lambda-list `(,@positional-lambda-list &key ,@keywords-lambda-list)))
         `(lambda (,continuation ,lambda-environment ,@lambda-list)
            (funcall ,continuation ,lambda-environment ,@positional-vars ,@keywords-vars))))
      (rest
       (let* ((lambda-list `(,@positional-lambda-list &rest ,rest)))
         `(lambda (,continuation ,lambda-environment ,@lambda-list)
            (apply ,continuation ,lambda-environment ,@positional-vars
                   (mapcar #'(lambda (form)
                               (specialization-store:determine-form-type form ,lambda-environment))
                           ,rest)))))
      (t
       `(lambda (,continuation ,lambda-environment ,@positional-lambda-list)
          (funcall ,continuation ,lambda-environment ,@positional-vars))))))
