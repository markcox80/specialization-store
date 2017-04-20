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


;;;; Parameter Protocol

(defgeneric parameter-var (parameter))
(defgeneric parameterp (parameter))
(defgeneric required-parameter-p (parameter))
(defgeneric optional-parameter-p (parameter))
(defgeneric rest-parameter-p (parameter))
(defgeneric keyword-parameter-p (parameter))

;;; Non required parameter protocol
(defgeneric parameter-init-form (parameter))
(defgeneric parameter-varp (parameter))
(defgeneric parameter-dependencies (parameter))

;; Keyword parameter protocol
(defgeneric parameter-keyword (parameter))

(defclass parameter ()
  ((var :initarg :var
        :reader parameter-var)))

(defclass required-parameter (parameter)
  ())

(defclass voluntary-parameter (parameter)
  ((dependencies :initarg :dependencies
                 :reader parameter-dependencies)
   (init-form :initarg :init-form
              :reader parameter-init-form)
   (varp :initarg :varp
         :reader parameter-varp)))

(defclass optional-parameter (voluntary-parameter)
  ())

(defclass keyword-parameter (voluntary-parameter)
  ((keyword :initarg :keyword
            :reader parameter-keyword)))

(defclass rest-parameter (parameter)
  ())

(defmethod parameterp ((object t))
  nil)

(defmethod parameterp ((object parameter))
  t)

(defmethod required-parameter-p ((object parameter))
  nil)

(defmethod required-parameter-p ((object required-parameter))
  t)

(defmethod optional-parameter-p ((object parameter))
  nil)

(defmethod optional-parameter-p ((object optional-parameter))
  t)

(defmethod keyword-parameter-p ((object parameter))
  nil)

(defmethod keyword-parameter-p ((object keyword-parameter))
  t)

(defmethod rest-parameter-p ((object parameter))
  nil)

(defmethod rest-parameter-p ((object rest-parameter))
  t)

(defmethod parameter-dependencies ((object required-parameter))
  nil)

(defmethod parameter-dependencies ((object rest-parameter))
  nil)

(defun make-required-parameter (var)
  (check-type var symbol)
  (make-instance 'required-parameter :var var))

(defun %parameter-dependencies-p (object)
  (and (listp object)
       (every #'parameterp object)))

(deftype parameter-dependencies ()
  '(satisfies %parameter-dependencies-p))

(defun make-optional-parameter (var &optional init-form dependencies varp)
  (check-type var symbol)
  (check-type dependencies parameter-dependencies)
  (check-type varp symbol)
  (make-instance 'optional-parameter :var var
                                     :init-form init-form
                                     :dependencies dependencies
                                     :varp varp))

(defun make-rest-parameter (var)
  (make-instance 'rest-parameter :var var))

(defun make-keyword-parameter (var &optional init-form dependencies varp (keyword nil keywordp))
  (check-type var symbol)
  (check-type dependencies parameter-dependencies)
  (check-type varp symbol)
  (let* ((keyword (if keywordp
                      keyword
                      (alexandria:make-keyword (symbol-name var)))))
    (check-type keyword keyword)
    (make-instance 'keyword-parameter :var var
                                      :init-form init-form
                                      :dependencies dependencies
                                      :varp varp
                                      :keyword keyword)))

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
  (error *parse-lambda-list-error-class* :message (apply #'format nil control-string args)))

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

(defun function-type (store-parameters specialization-parameters value-type)
  (let* ((required (required-parameters specialization-parameters))
         (optional (optional-parameters specialization-parameters))
         (keywordsp (keyword-parameters-p specialization-parameters))
         (keywords (keyword-parameters specialization-parameters))
         (rest (rest-parameter specialization-parameters))
         (input-types (append (mapcar #'second required)
                              (when optional
                                (cons '&optional (mapcar (constantly t) optional)))
                              (when (and rest (not keywordsp))
                                '(&rest t))
                              (when keywordsp
                                (cons '&key
                                      (loop
                                        with store-keyword-parameters = (keyword-parameters store-parameters)
                                        for (keyword nil form) in keywords
                                        when (find keyword store-keyword-parameters :key #'first)
                                          append (list keyword form)))))))
    `(function ,input-types ,value-type)))

(defmethod make-value-completion-lambda-form ((parameters store-parameters))
  (let* ((required (required-parameters parameters))
         (required-forms required)
         (optional (optional-parameters parameters))
         (optional-forms (loop for (var) in optional collect var))
         (positional (append required (when optional (append '(&optional) optional))))
         (positional-forms (append required-forms optional-forms))
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
    (cond (keywordsp
           (let* ((rest (or rest (gensym "REST"))))
             `(lambda (,continuation)
                (lambda (,@positional &rest ,rest &key ,@keywords ,@allow-other-keys)
                  (apply ,continuation ,@positional-forms ,@keyword-forms ,rest)))))
          (rest
           `(lambda (,continuation)
              (lambda (,@positional &rest ,rest)
                (apply ,continuation ,@positional-forms ,rest))))
          (t
           `(lambda (,continuation)
              (lambda (,@positional)
                (funcall ,continuation ,@positional-forms)))))))

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
         (positional (append required (when optional (append '(&optional) optional))))
         (positional-let*-forms (append required-let*-forms optional-let*-forms))
         (positional-forms (append required-forms optional-forms))
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
                               for ((nil var) init-form-type suppliedp) in keywords
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
      (keywordsp
       `(lambda (,continuation)
          (compiler-macro-lambda (&whole ,lambda-form ,@positional &key ,@keywords ,@allow-other-keys &environment ,lambda-environment)
            (let* (,@positional-let*-forms ,@keyword-let*-forms)
              (funcall ,continuation ,lambda-form ,lambda-environment
                       (list ,@positional-forms ,@keyword-forms))))))
      (rest
       (let ((rest-form (gensym "REST-FORM")))
         `(lambda (,continuation)
            (compiler-macro-lambda (&whole ,lambda-form ,@positional &rest ,rest &environment ,lambda-environment)
              (let* (,@positional-let*-forms)
                (funcall ,continuation ,lambda-form ,lambda-environment
                         (append (list ,@positional-forms)
                                 (mapcar #'(lambda (,rest-form)
                                             (determine-form-value-type ,rest-form ,lambda-environment))
                                         ,rest))))))))
      (t
       `(lambda (,continuation)
          (compiler-macro-lambda (&whole ,lambda-form ,@positional &environment ,lambda-environment)
            (let* (,@positional-let*-forms)
              (funcall ,continuation ,lambda-form ,lambda-environment
                       (list ,@required-forms ,@optional-forms)))))))))

;;;; function-and-macro

;; When a function call is made, all of the arguments are evaluated
;; first and then given to the function as a list. The arguments are
;; then destructured according to the lambda list of the
;; function.
;;
;; Thus, given the following function definition,
;;
;;   (defun example (a &key (b a))
;;     ;; %example is a function.
;;     (%example a b))
;;
;; and the following application
;;
;;   (example object)
;;
;; it can be concluded that the value of the variable B is EQL to A
;; for any object.
;;
;; Associated with the EXAMPLE function is the compiler macro:
;;
;;   (define-compiler-macro example (a &key (b a))
;;     `(%example ,a ,b))
;;
;; Unfortunately, this compiler macro changes how arguments are
;; evaluated. Consider the following example:
;;
;;   (let ((x 0))
;;     (defun generate ()
;;       (incf x)
;;       (list x)))
;;
;;   (example (generate))
;;
;; Applying the compiler macro to the EXAMPLE application
;;
;;   (compiler-macroexpand '(example (generate)))
;;   => (%example (generate) (generate))
;;
;; we see that there are now two invocations of the GENERATE function
;; and thus there exists cases where the object bound to B is not EQL
;; to the object bound to A.
;;
;; The purpose of the FUNCTION-AND-MACRO component is to ensure that
;; the function and the form generated by the compiler macro function
;; are consistent in the way arguments are evaluated.
;;
;; Consistency can only be achieved if the function and compiler macro
;; function are defined simultaneously. This is evident from the
;; following function definition
;;
;;   (flet ((init-b (a)
;;            ...))
;;     (defun example (a &key (b (init-b a)))
;;       ...))
;;
;; The lexical environment of the EXAMPLE function must be retained
;; for use by the compiler macro in order for the initialisation forms
;; to be equivalent.
;;
;; Care must also be taken to ensure to macroexpansion is consistent
;; too
;;
;;   (defvar *count* 0)
;;
;;   (defun example (a &key (b (macrolet ((g ()
;;                                           (incf *count*)))
;;                               (g)))))
;;
;;
;; The init form for the b argument should only be expanded once, when
;; generating the lexical environment used by the function and the
;; compiler macro function.
