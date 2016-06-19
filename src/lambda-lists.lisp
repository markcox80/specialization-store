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
;; This section contains code for the following tasks:
;;
;; 1. Determining if a specialization lambda list is congruent with a
;; store lambda list. This is needed when adding a specialization to a
;; store function.
;;
;; 2. Determining if a store lambda list is congruent with a store
;; lambda list. This is needed when reinitialising a store function.
;;
;; The generic function CONGRUENT-PARAMETERS-P provides an interface
;; to the two cases. 

(defgeneric congruent-parameters-p (store-parameters other-parameters))

(defmethod congruent-parameters-p ((store store-parameters) (specialization specialization-parameters))
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
       t))

(defmethod congruent-parameters-p ((store-a store-parameters) (store-b store-parameters))
  (flet ((compare (test &rest functions)
           (funcall test
                    (reduce #'funcall functions :from-end t :initial-value store-a)
                    (reduce #'funcall functions :from-end t :initial-value store-b))))
    (and (compare #'= #'length #'required-parameters)
         (compare #'= #'length #'optional-parameters)
         (compare #'eql #'not #'keyword-parameters-p)
         (if (and (keyword-parameters-p store-a) (keyword-parameters-p store-b))
             t
             (compare #'eql #'not #'rest-parameter))
         (alexandria:set-equal (keyword-parameters store-a) (keyword-parameters store-b)
                               :test #'(lambda (a b)
                                         ;; (keyword var init-form)
                                         (eql (first a) (first b)))))))

;;;; Lambda list conversions
(defgeneric ordinary-lambda-list (store-parameters specialization-parameters))
(defgeneric type-declarations (store-parameters specialization-parameters))
(defgeneric make-value-completion-lambda-form (parameters))
(defgeneric make-type-completion-lambda-form (parameters environment))

(defmethod ordinary-lambda-list ((store-parameters store-parameters) (specialization-parameters specialization-parameters))
  (append (mapcar #'first (required-parameters specialization-parameters))
          (when (optional-parameters specialization-parameters)
            `(&optional ,@(loop
                             for (var init-form supplied-p-var) in (optional-parameters specialization-parameters)
                             collect (cond (supplied-p-var
                                            `(,var ,init-form ,supplied-p-var))
                                           (init-form
                                            `(,var ,init-form))
                                           (t
                                            var)))))
	  (when (rest-parameter specialization-parameters)
	    `(&rest ,(rest-parameter specialization-parameters)))
	  (when (keyword-parameters-p specialization-parameters)
	    `(&key ,@(loop
                        with store-keyword-parameters = (keyword-parameters store-parameters)
                        for (keyword var form supplied-p-var) in (keyword-parameters specialization-parameters)
                        for init-form = (if (find keyword store-keyword-parameters :key #'first)
                                            nil
                                            form)
                        for first-form = (if (equal (symbol-name keyword) (symbol-name var))
                                             var
                                             `(,keyword ,var))
                        collect (cond (supplied-p-var
                                       `(,first-form ,init-form ,supplied-p-var))
                                      (init-form
                                       `(,first-form ,init-form))
                                      ((atom first-form)
                                       first-form)
                                      (t
                                       (list first-form))))))
	  (when (allow-other-keys-p specialization-parameters)
	    `(&allow-other-keys))))

(defmethod type-declarations ((store-parameters store-parameters) (specialization-parameters specialization-parameters))
  (append (loop
	     for (var type) in (required-parameters specialization-parameters)
             unless (eql type t)
	     collect `(type ,type ,var))
	  (loop
	     with store-keyword-parameters = (keyword-parameters store-parameters)
	     for (keyword var form supplied-p-var) in (keyword-parameters specialization-parameters)
	     when (find keyword store-keyword-parameters :key #'first)
	     append (append (when form
                              `((type ,form ,var)))
                            (when supplied-p-var
                              `((type (eql t) ,supplied-p-var)))))))

(defmethod make-value-completion-lambda-form ((parameters store-parameters))
  (let* ((required (required-parameters parameters))
         (required-forms required)
         (optional (optional-parameters parameters))
         (optional-forms (loop for (var) in optional collect var))
         (rest (rest-parameter parameters))
         (keywordsp (keyword-parameters-p parameters))
         (keywords (loop
                      for (keyword var init-form) in (keyword-parameters parameters)
                      collect `((,keyword ,var) ,init-form)))
         (keyword-forms (loop
                           for (keyword var) in (keyword-parameters parameters)
                           append (list keyword var)))
         (allow-other-keys (when (allow-other-keys-p parameters)
                             '(&allow-other-keys)))
         (continuation (gensym "CONTINUATION")))
    (cond ((and optional keywordsp)
           (let* ((rest (or rest (gensym "REST"))))
             `(lambda (,continuation)
                (lambda (,@required &optional ,@optional &rest ,rest &key ,@keywords ,@allow-other-keys)
                  (apply ,continuation ,@required-forms ,@optional-forms ,@keyword-forms ,rest)))))
          (keywordsp
           (let* ((rest (or rest (gensym "REST"))))
             `(lambda (,continuation)
                (lambda (,@required &rest ,rest &key ,@keywords ,@allow-other-keys)
                  (apply ,continuation ,@required-forms ,@keyword-forms ,rest)))))
          (rest
           `(lambda (,continuation)
              (lambda (,@required &optional ,@optional &rest ,rest)
                (apply ,continuation ,@required-forms ,@optional-forms ,rest))))
          (t
           `(lambda (,continuation)
              (lambda (,@required &optional ,@optional)
                (funcall ,continuation ,@required-forms ,@optional-forms)))))))

(defmethod make-type-completion-lambda-form ((parameters store-parameters) environment)
  (let* ((continuation (gensym "CONTINUATION"))
         (lambda-form (gensym "FORM"))
         (lambda-environment (gensym "ENVIRONMENT"))
         (required (required-parameters parameters))
         (required-vars required)
         (required-let*-forms (loop
                                 for var in required
                                 collect `(,var (determine-form-value-type ,var ,lambda-environment))))
         (required-forms required-vars)
         (optional (loop
                      for (var init-form) in (optional-parameters parameters)
                      for init-form-type = (determine-form-value-type init-form environment)
                      collect (list var `(quote ,init-form-type) (gensym "SUPPLIEDP"))))
         (optional-vars (mapcar #'first (optional-parameters parameters)))
         (optional-let*-forms (loop
                                 with left-vars = required-vars
                                 for (nil init-form) in (optional-parameters parameters)
                                 for expanded-init-form = (macroexpand init-form environment)
                                 for (var init-form-type suppliedp) in optional
                                 collect `(,var (if ,suppliedp
                                                    (determine-form-value-type ,var ,lambda-environment)
                                                    ,(if (find expanded-init-form left-vars)
                                                         expanded-init-form
                                                         init-form-type)))
                                 do
                                   (push var left-vars)))
         (optional-forms optional-vars)
         (rest (rest-parameter parameters))
         (keywordsp (keyword-parameters-p parameters))
         (keywords (loop
                      for (keyword var init-form) in (keyword-parameters parameters)
                      for init-form-type = (determine-form-value-type init-form environment)
                      collect (list (list keyword var) `(quote ,init-form-type) (gensym "SUPPLIEDP"))))
         (keyword-let*-forms (loop
                                with left-vars = (append required-vars optional-vars)
                                for (nil nil init-form) in (keyword-parameters parameters)
                                for expanded-init-form = (macroexpand init-form environment)
                                for ((keyword var) init-form-type suppliedp) in keywords
                                collect (list var `(if ,suppliedp
                                                       (determine-form-value-type ,var ,lambda-environment)
                                                       ,(if (find expanded-init-form left-vars)
                                                            expanded-init-form
                                                            init-form-type)))
                                do
                                  (push var left-vars)))
         (keyword-forms (loop
                           for (keyword var) in (keyword-parameters parameters)
                           append (list keyword var)))
         (allow-other-keys (when (allow-other-keys-p parameters)
                             '(&allow-other-keys))))
    (cond
      ;; The reason I test for this case is due to the warning some
      ;; implementations signal when optional and keyword arguments
      ;; are specified together. Who knows why specifying both is so
      ;; confusing to people?
      ((and optional keywordsp)
       `(lambda (,continuation)
          (compiler-macro-lambda (&whole ,lambda-form ,@required &optional ,@optional &key ,@keywords ,@allow-other-keys
                                         &environment ,lambda-environment)
            (let* (,@required-let*-forms ,@optional-let*-forms ,@keyword-let*-forms)
              (funcall ,continuation ,lambda-form ,lambda-environment
                       (list ,@required-forms ,@optional-forms ,@keyword-forms))))))
      (keywordsp
       `(lambda (,continuation)
          (compiler-macro-lambda (&whole ,lambda-form ,@required &key ,@keywords ,@allow-other-keys
                                         &environment ,lambda-environment)
            (let* (,@required-let*-forms ,@keyword-let*-forms)
              (funcall ,continuation ,lambda-form ,lambda-environment
                       (list ,@required-forms ,@keyword-forms))))))
      (rest
       (let ((rest-form (gensym "REST-FORM")))
         `(lambda (,continuation)
            (compiler-macro-lambda (&whole ,lambda-form ,@required &optional ,@optional &rest ,rest &environment ,lambda-environment)
              (let* (,@required-let*-forms ,@optional-let*-forms)
                (funcall ,continuation ,lambda-form ,lambda-environment
                         (append (list ,@required-forms ,@optional-forms)
                                 (mapcar #'(lambda (,rest-form)
                                             (determine-form-value-type ,rest-form ,lambda-environment))
                                         ,rest))))))))
      (t
       `(lambda (,continuation)
          (compiler-macro-lambda (&whole ,lambda-form ,@required &optional ,@optional &environment ,lambda-environment)
            (let* (,@required-let*-forms ,@optional-let*-forms)
              (funcall ,continuation ,lambda-form ,lambda-environment
                       (list ,@required-forms ,@optional-forms)))))))))
