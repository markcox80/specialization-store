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
(defgeneric parameter-lambda-list-specification (parameter))

;;; Non required parameter protocol
(defgeneric parameter-init-form (parameter))
(defgeneric parameter-varp (parameter))
(defgeneric parameter-dependencies (parameter))
(defgeneric parameter-vars (parameter))

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

(defmethod parameter-vars ((object required-parameter))
  (list (parameter-var object)))

(defmethod parameter-vars ((object rest-parameter))
  (list (parameter-var object)))

(defmethod parameter-vars ((object voluntary-parameter))
  (list (parameter-var object)
        (parameter-varp object)))

(defun make-required-parameter (var)
  (check-type var symbol)
  (make-instance 'required-parameter :var var))

(defun %parameter-dependencies-p (object)
  (and (listp object)
       (every #'parameterp object)))

(deftype parameter-dependencies ()
  '(satisfies %parameter-dependencies-p))

(defun make-optional-parameter (dependencies var &optional init-form varp)
  (check-type var symbol)
  (check-type dependencies parameter-dependencies)
  (check-type varp symbol)
  (make-instance 'optional-parameter :var var
                                     :init-form init-form
                                     :dependencies dependencies
                                     :varp varp))

(defun make-rest-parameter (var)
  (make-instance 'rest-parameter :var var))

(defun make-keyword-parameter (dependencies var &optional init-form varp (keyword nil keywordp))
  (check-type var (and (not null) symbol))
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

(defmethod print-object ((object required-parameter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (parameter-var object) :stream stream)))

(defmethod print-object ((object optional-parameter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (var init-form varp) object
      (write (list var init-form varp) :stream stream))))

(defmethod print-object ((object rest-parameter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (parameter-var object) :stream stream)))

(defmethod print-object ((object keyword-parameter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (keyword var init-form varp) object
      (write (list (list keyword var) init-form varp) :stream stream))))

(defmethod parameter-lambda-list-specification ((p required-parameter))
  (parameter-var p))

(defmethod parameter-lambda-list-specification ((p optional-parameter))
  (with-slots (var init-form varp) p
    (cond ((and (null init-form) (null varp))
           var)
          ((and (null varp))
           (list var init-form))
          (t
           (list var init-form varp)))))

(defmethod parameter-lambda-list-specification ((p rest-parameter))
  (parameter-var p))

(defmethod parameter-lambda-list-specification ((p keyword-parameter))
  (with-slots (keyword var init-form varp) p
    (let* ((name (if (eql keyword (alexandria:make-keyword (symbol-name var)))
                     var
                     (list keyword var))))
      (cond ((and (null init-form) (null varp))
             name)
            ((and (null varp))
             (list name init-form))
            (t
             (list name init-form varp))))))

;;;; Parameters Protocol

;; Properties
(defgeneric all-parameters (parameters))
(defgeneric original-lambda-list (parameters))
(defgeneric required-parameters (parameters))
(defgeneric optional-parameters (parameters))
(defgeneric optional-parameters-p (parameters))
;; These are defined above
;; (defgeneric rest-parameter (parameters))
;; (defgeneric rest-parameter-p (parameters))
(defgeneric keyword-parameters-p (parameters))
(defgeneric allow-other-keys-p (parameters))
(defgeneric keyword-parameters (parameters))
(defgeneric positional-parameters-lower-bound (parameters))
(defgeneric positional-parameters-upper-bound (parameters))
;; (defgeneric parameter-vars (parameters))

;; Operations
(defgeneric parameters-equal (parameters-1 parameters-2))
(defgeneric duplicate-keywords-p (parameters))
(defgeneric duplicate-variables-p (parameters))

(defclass parameters ()
  ((original-lambda-list :initarg :original-lambda-list
                         :reader original-lambda-list)
   (all-parameters :initarg :all-parameters
                   :reader all-parameters)
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

(defmethod optional-parameters-p ((parameters parameters))
  (and (optional-parameters parameters)
       t))

(defmethod rest-parameter-p ((parameters parameters))
  (and (rest-parameter parameters)
       t))

(defmethod duplicate-keywords-p ((parameters parameters))
  (let ((keywords (mapcar #'parameter-keyword (keyword-parameters parameters))))
    (filter-duplicates keywords)))

(defmethod duplicate-variables-p ((parameters parameters))
  (let* ((names (loop
                  for p in (all-parameters parameters)
                  append (if (or (required-parameter-p p)
                                 (rest-parameter-p p)
                                 (not (parameter-varp p)))
                             (list (parameter-var p))
                             (list (parameter-var p) (parameter-varp p))))))
    (filter-duplicates names)))

(defmethod parameter-vars ((parameters parameters))
  (loop
    for parameter in (all-parameters parameters)
    append (parameter-vars parameter)))

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

(defun parse-ordinary-lambda-list/optional (fn list dependencies)
  (labels ((process (list dependencies)
             (let ((item (first list)))
               (cond
                 ((null list)
                  nil)
                 ((member item '(&rest &key))
                  list)
                 ((or (null item) (member item '(&optional &allow-other-keys)))
                  (invalid-ordinary-lambda-list-item item))
                 (t
                  (let* ((new-dependency (funcall fn :optional item dependencies)))
                    (process (rest list)
                             (append dependencies (list new-dependency)))))))))
    (let ((item (first list)))
      (cond
        ((null list)
         list)
        ((eql '&optional item)
         (process (rest list) dependencies))
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

(defun parse-ordinary-lambda-list/keys (fn list dependencies)
  (labels ((process (list dependencies)
             (let ((item (first list)))
               (cond
                 ((or (null list) (eql '&allow-other-keys item))
                  list)
                 ((or (null item) (member item '(&rest &optional &key)))
                  (invalid-ordinary-lambda-list-item item))
                 (t
                  (let* ((new-dependency (funcall fn :keyword item dependencies)))
                    (process (rest list)
                             (append dependencies (list new-dependency)))))))))
    (let ((item (first list)))
      (cond
        ((eql '&key item)
         (funcall fn :keys? t)
         (process (rest list) dependencies))
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
      (macrolet ((%appendf-value/return-value (place new-value)
                   (alexandria:with-gensyms (tmp)
                     `(let* ((,tmp ,new-value))
                        (alexandria:appendf ,place (list ,tmp))
                        ,tmp))))
        (flet ((process (what value &optional dependencies)
                 (ecase what
                   (:required (%appendf-value/return-value required (funcall function what value dependencies)))
                   (:optional (%appendf-value/return-value optional (funcall function what value dependencies)))
                   (:rest (setf rest (funcall function what value)))
                   (:keys? (setf keys? t))
                   (:keyword (%appendf-value/return-value keywords (funcall function what value dependencies)))
                   (:allow-other-keys? (setf allow-other-keys? t)))))
          (let* ((after-required (parse-ordinary-lambda-list/required #'process ordinary-lambda-list))
                 (after-optional (parse-ordinary-lambda-list/optional #'process after-required required))
                 (after-rest (parse-ordinary-lambda-list/rest #'process after-optional))
                 (after-keys (parse-ordinary-lambda-list/keys #'process after-rest (append required
                                                                                           optional
                                                                                           (when rest
                                                                                             (list rest)))))
                 (after-allow-other-keys (parse-ordinary-lambda-list/allow-other-keys #'process after-keys)))
            (assert (null after-allow-other-keys))
            (make-instance class-name
                           :all-parameters (append required
                                                   optional
                                                   (when rest
                                                     (list rest))
                                                   keywords)
                           :original-lambda-list ordinary-lambda-list
                           :required-parameters required
                           :optional-parameters optional
                           :rest-parameter rest
                           :keyword-parameters-p keys?
                           :keyword-parameters keywords
                           :allow-other-keys-p allow-other-keys?)))))))

;;;; Store Lambda Lists

;;;; Parsing

(define-condition parse-store-lambda-list-error (parse-lambda-list-error)
  ())

(defclass store-parameters (parameters)
  ())

(defun parse-store-lambda-list (store-lambda-list)
  (labels ((process (command value &optional dependencies)
             (ecase command
               (:required (if (symbolp value)
                              (make-required-parameter value)
                              (signal-parse-lambda-list-error "Invalid required parameter name ~W." value)))
               (:optional (flet ((signal-invalid ()
                                   (signal-parse-lambda-list-error "Invalid optional parameter specification ~W." value)))
                            (cond ((symbolp value)
                                   (make-optional-parameter dependencies value))
                                  ((and (listp value) (<= 1 (length value) 3))
                                   (destructuring-bind (var &optional init-form varp) value
                                     (cond ((or (null var) (not (symbolp varp))
                                                (not (symbolp varp)))
                                            (signal-invalid))
                                           (t
                                            (make-optional-parameter dependencies var init-form varp)))))
                                  (t
                                   (signal-parse-lambda-list-error "Invalid optional parameter specification ~W." value)))))
               (:rest (if (and value (symbolp value))
                          (make-rest-parameter value)
                          (signal-parse-lambda-list-error "Invalid rest parameter specification ~W." value)))
               (:keyword (flet ((signal-invalid ()
                                  (signal-parse-lambda-list-error "Invalid keyword parameter specification ~W." value)))
                           (cond ((symbolp value)
                                  (make-keyword-parameter dependencies value))
                                 ((and (listp value) (<= 1 (length value) 3))
                                  (destructuring-bind (name &optional init-form varp) value
                                    (cond ((and name (symbolp name))
                                           (make-keyword-parameter dependencies name init-form varp))
                                          ((and name (listp name) (= 2 (length name)))
                                           (destructuring-bind (keyword var) name
                                             (cond ((and keyword (keywordp keyword)
                                                         var (symbolp var))
                                                    (make-keyword-parameter dependencies var init-form varp keyword))
                                                   (t
                                                    (signal-invalid)))))
                                          (t
                                           (signal-invalid)))))
                                 (t
                                  (signal-invalid))))))))
    (let* ((*lambda-list* store-lambda-list)
           (*lambda-list-description* "store-lambda-list")
           (*parse-lambda-list-error-class* 'parse-store-lambda-list-error)
           (rv (parse-ordinary-lambda-list 'store-parameters #'process store-lambda-list))
           (duplicate-keywords (duplicate-keywords-p rv))
           (duplicate-variables (duplicate-variables-p rv)))
      (when duplicate-keywords
        (signal-parse-lambda-list-error "The keywords ~W are used more than once in the store lambda list." duplicate-keywords))
      (when duplicate-variables
        (signal-parse-lambda-list-error "The variables ~W are used more than once in the store lambda list." duplicate-variables))
      rv)))

;;;; Specialization Lambda Lists

(define-condition parse-specialization-lambda-list-error (parse-lambda-list-error)
  ())

(defgeneric parameter-type (object))

(defclass specialized-required-parameter (required-parameter)
  ((type :initarg :type)))

(defclass specialized-optional-parameter (optional-parameter)
  ())

(defclass specialized-rest-parameter (rest-parameter)
  ())

(defclass specialized-keyword-parameter (keyword-parameter)
  ())

(defun make-specialized-required-parameter (var type)
  (check-type var (and (not null) symbol))
  (make-instance 'specialized-required-parameter :var var :type type))

(defun make-specialized-optional-parameter (dependencies var &optional init-form-or-type varp)
  (check-type dependencies parameter-dependencies)
  (check-type var (and (not null) symbol))
  (check-type varp symbol)
  (make-instance 'specialized-optional-parameter :dependencies dependencies
                                                 :var var
                                                 :init-form init-form-or-type
                                                 :varp varp))

(defun make-specialized-rest-parameter (var)
  (check-type var (and (not null) symbol))
  (make-instance 'specialized-rest-parameter :var var))

(defun make-specialized-keyword-parameter (dependencies var &optional init-form-or-type varp (keyword nil keywordp))
  (check-type dependencies parameter-dependencies)
  (check-type var (and (not null) symbol))
  (check-type varp symbol)
  (let* ((keyword (if keywordp
                      keyword
                      (alexandria:make-keyword (symbol-name var)))))
    (check-type keyword keyword)
    (make-instance 'specialized-keyword-parameter :dependencies dependencies
                                                  :var var
                                                  :init-form init-form-or-type
                                                  :varp varp
                                                  :keyword keyword)))

(defmethod parameter-lambda-list-specification ((parameter specialized-required-parameter))
  (list (parameter-var parameter)
        (parameter-type parameter)))

(defmethod parameter-type ((object specialized-required-parameter))
  (slot-value object 'type))

(defmethod parameter-type ((object specialized-optional-parameter))
  (error "Specialized lambda lists are unable to specify the type of optional parameters."))

(defmethod parameter-type ((object specialized-rest-parameter))
  'list)

(defmethod parameter-type ((object specialized-keyword-parameter))
  (or (parameter-init-form object)
      t))

(defclass specialization-parameters (parameters)
  ())

(defun parse-specialization-lambda-list (specialization-lambda-list)
  (labels ((process (what value &optional dependencies)
             (ecase what
               (:required (cond ((symbolp value)
                                 (make-specialized-required-parameter value t))
                                ((and (listp value) (<= 1 (length value) 2))
                                 (destructuring-bind (name &optional (type t)) value
                                   (make-specialized-required-parameter name type)))
                                (t
                                 (signal-parse-lambda-list-error "Invalid required parameter specification ~W." value))))
               (:optional (flet ((signal-invalid ()
                                   (signal-parse-lambda-list-error "Invalid optional parameter specification ~W." value)))
                            (cond ((symbolp value)
                                   (make-specialized-optional-parameter dependencies value))
                                  ((and (listp value) (<= 1 (length value) 3))
                                   (destructuring-bind (var &optional init-form (supplied-p-var nil supplied-p-var?)) value
                                     (cond ((not var)
                                            (signal-invalid))
                                           ((and supplied-p-var? (not supplied-p-var))
                                            (signal-invalid))
                                           (supplied-p-var?
                                            (make-specialized-optional-parameter dependencies var init-form supplied-p-var))
                                           (t
                                            (make-specialized-optional-parameter dependencies var init-form)))))
                                  (t
                                   (signal-invalid)))))
               (:rest (if value
                          (make-specialized-rest-parameter value)
                          (signal-parse-lambda-list-error "Invalid rest parameter specification ~W." value)))
               (:keyword (flet ((signal-invalid ()
                                  (signal-parse-lambda-list-error "Invalid keyword parameter specification ~W." value)))
                           (cond ((symbolp value)
                                  (make-specialized-keyword-parameter dependencies value))
                                 ((and (listp value) (<= 1 (length value) 3))
                                  (destructuring-bind (var &optional init-form (supplied-p-var nil supplied-p-var?)) value
                                    (cond ((not var)
                                           (signal-invalid))
                                          ((and supplied-p-var? (not supplied-p-var))
                                           (signal-invalid))
                                          ((and (listp var) (= (length var) 2))
                                           (destructuring-bind (keyword name) var
                                             (when (or (not (keywordp keyword))
                                                       (null name)
                                                       (not (symbolp keyword)))
                                               (signal-invalid))
                                             (make-specialized-keyword-parameter dependencies name init-form supplied-p-var keyword)))
                                          ((and (not (null var)) (symbolp var))
                                           (make-specialized-keyword-parameter dependencies var init-form supplied-p-var))
                                          (t
                                           (signal-invalid)))))
                                 (t
                                  (signal-invalid))))))))
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
              (and (keyword-parameters-p specialization)
                   ;; All keyword parameters in the store must be
                   ;; present in the specialization, they must be in
                   ;; the same order and they must in type form.
                   (<= (length (keyword-parameters store))
                       (length (keyword-parameters specialization)))
                   (every #'(lambda (store-keyword-parameter specialization-keyword-parameter)
                              (eql (parameter-keyword store-keyword-parameter)
                                   (parameter-keyword specialization-keyword-parameter)))
                          (keyword-parameters store)
                          (keyword-parameters specialization))))
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
                               :key #'parameter-keyword
                               :test #'eql))))

;;;; Lambda list conversions
(defgeneric parameter-init-forms-as-global-functions (parameters environment))
(defgeneric ordinary-lambda-list (store-parameters specialization-parameters))
(defgeneric type-declarations (store-parameters specialization-parameters))
(defgeneric make-value-completion-lambda-form (parameters))
(defgeneric make-type-completion-lambda-form (parameters environment))

(defmethod parameter-init-forms-as-global-functions ((parameters store-parameters) environment)
  (let* ((globals nil))
    (flet ((generate-init-form (parameter)
             (let* ((function-name (specialization-store:generate-interned-symbol (parameter-var parameter)
                                                                                  "INIT-FUNCTION"))
                    (vars (reduce #'append (parameter-dependencies parameter)
                                  :key #'parameter-vars
                                  :initial-value nil))
                    (function `(defun ,function-name ,vars
                                 (declare (ignorable ,@vars))
                                 ,(parameter-init-form parameter))))
               (push function globals)
               `(,function-name ,@vars))))
      (let* ((required (required-parameters parameters))
             (optional (loop
                         with dependencies = required
                         for parameter in (optional-parameters parameters)
                         for init-form = (parameter-init-form parameter)
                         for new-parameter = (cond ((constantp init-form environment)
                                                    parameter)
                                                   (t
                                                    (make-optional-parameter dependencies
                                                                             (parameter-var parameter)
                                                                             (generate-init-form parameter)
                                                                             (parameter-varp parameter))))
                         do
                            (alexandria:appendf dependencies (list new-parameter))
                         collect
                         new-parameter))
             (rest (rest-parameter parameters))
             (keywords (loop
                         with dependencies = (append required optional (when rest (list rest)))
                         for parameter in (keyword-parameters parameters)
                         for init-form = (parameter-init-form parameter)
                         for new-parameter = (cond ((constantp init-form environment)
                                                    parameter)
                                                   (t
                                                    (make-keyword-parameter dependencies
                                                                            (parameter-var parameter)
                                                                            (generate-init-form parameter)
                                                                            (parameter-varp parameter)
                                                                            (parameter-keyword parameter))))
                         do
                            (alexandria:appendf dependencies (list new-parameter))
                         collect
                         new-parameter))
             (new-lambda-list (append (mapcar #'parameter-lambda-list-specification required)
                                      (when optional
                                        (cons '&optional (mapcar #'parameter-lambda-list-specification optional)))
                                      (when rest
                                        (list '&rest (parameter-var rest)))
                                      (when keywords
                                        (list '&key (mapcar #'parameter-lambda-list-specification keywords)))
                                      (when (allow-other-keys-p parameters)
                                        '(&allow-other-keys)))))
        (list (make-instance 'store-parameters
                             :original-lambda-list new-lambda-list
                             :all-parameters (append required optional (when rest (list rest)) keywords)
                             :required-parameters required
                             :optional-parameters optional
                             :rest-parameter rest
                             :keyword-parameters-p (keyword-parameters-p parameters)
                             :keyword-parameters keywords
                             :allow-other-keys-p (allow-other-keys-p parameters))
              (nreverse globals))))))

(defmethod ordinary-lambda-list ((store-parameters store-parameters) (specialization-parameters specialization-parameters))
  (append (mapcar #'parameter-var (required-parameters specialization-parameters))
          (when (optional-parameters-p specialization-parameters)
            `(&optional ,@(mapcar #'parameter-lambda-list-specification (optional-parameters specialization-parameters))))
          (when (rest-parameter specialization-parameters)
            `(&rest ,(parameter-lambda-list-specification (rest-parameter specialization-parameters))))
          (when (keyword-parameters-p specialization-parameters)
            `(&key ,@(loop
                       with store-keyword-parameters = (keyword-parameters store-parameters)
                       for parameter in (keyword-parameters specialization-parameters)
                       for keyword = (parameter-keyword parameter)
                       for var = (parameter-var parameter)
                       for form = (parameter-init-form parameter)
                       for supplied-p-var = (parameter-varp parameter)
                       for init-form = (if (find keyword store-keyword-parameters :key #'parameter-keyword)
                                           nil ;; The value completion function takes care of this.
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
            for parameter in (required-parameters specialization-parameters)
            for var = (parameter-var parameter)
            for type = (parameter-type parameter)
            unless (eql type t)
              collect `(type ,type ,var))
          (loop
            with store-keyword-parameters = (keyword-parameters store-parameters)
            for parameter in (keyword-parameters specialization-parameters)
            for keyword = (parameter-keyword parameter)
            for var = (parameter-var parameter)
            for form = (parameter-init-form parameter)
            for supplied-p-var = (parameter-varp parameter)
            when (find keyword store-keyword-parameters :key #'parameter-keyword)
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
         (input-types (append (mapcar #'parameter-type required)
                              (when optional
                                (cons '&optional (mapcar (constantly t) optional)))
                              (when (and rest (not keywordsp))
                                '(&rest t))
                              (when keywordsp
                                (cons '&key
                                      (loop
                                        with store-keyword-parameters = (keyword-parameters store-parameters)
                                        for parameter in keywords
                                        for keyword = (parameter-keyword parameter)
                                        for form = (parameter-init-form parameter)
                                        when (find keyword store-keyword-parameters :key #'parameter-keyword)
                                          append (list keyword form)))))))
    `(function ,input-types ,value-type)))

(defmethod make-value-completion-lambda-form ((parameters store-parameters))
  (let* ((required (mapcar #'parameter-lambda-list-specification (required-parameters parameters)))
         (optional (mapcar #'parameter-lambda-list-specification (optional-parameters parameters)))
         (positional (append required (when optional (append '(&optional) optional))))
         (rest (when (rest-parameter-p parameters)
                 (parameter-var (rest-parameter parameters))))
         (keywordsp (keyword-parameters-p parameters))
         (keywords (mapcar #'parameter-lambda-list-specification (keyword-parameters parameters)))
         (required-forms (mapcar #'parameter-var (required-parameters parameters)))
         (optional-forms (mapcar #'parameter-var (optional-parameters parameters)))
         (positional-forms (append required-forms optional-forms))
         (keyword-forms (loop
                          for parameter in (keyword-parameters parameters)
                          append (list (parameter-keyword parameter)
                                       (parameter-var parameter))))
         (allow-other-keys (when (allow-other-keys-p parameters)
                             '(&allow-other-keys)))
         (continuation (gensym "CONTINUATION"))
         (ignorable-vars (parameter-vars parameters)))
    (cond (keywordsp
           (let* ((rest (or rest (gensym "REST"))))
             `(lambda (,continuation)
                (lambda (,@positional &rest ,rest &key ,@keywords ,@allow-other-keys)
                  (declare (ignorable ,@ignorable-vars))
                  (apply ,continuation ,@positional-forms ,@keyword-forms ,rest)))))
          (rest
           `(lambda (,continuation)
              (lambda (,@positional &rest ,rest)
                (declare (ignorable-vars ,@ignorable-vars))
                (apply ,continuation ,@positional-forms ,rest))))
          (t
           `(lambda (,continuation)
              (lambda (,@positional)
                (declare (ignorable-vars ,@ignorable-vars))
                (funcall ,continuation ,@positional-forms)))))))

(defmethod make-type-completion-lambda-form ((parameters store-parameters) environment)
  (let* ((continuation (gensym "CONTINUATION"))
         (lambda-form (gensym "FORM"))
         (lambda-environment (gensym "ENVIRONMENT"))
         (required (mapcar #'parameter-var (required-parameters parameters)))
         (required-vars required)
         (required-let*-forms (loop
                                for var in required
                                collect `(,var (determine-form-value-type ,var ,lambda-environment))))
         (required-forms required-vars)
         (optional (loop
                     for parameter in (optional-parameters parameters)
                     for var = (parameter-var parameter)
                     for init-form = (parameter-init-form parameter)
                     collect (list var nil (gensym "SUPPLIEDP"))))
         (optional-vars (mapcar #'parameter-var (optional-parameters parameters)))
         (optional-let*-forms (loop
                                for parameter in (optional-parameters parameters)
                                for init-form = (parameter-init-form parameter)
                                for init-form-type = (determine-form-value-type init-form environment)
                                for (var nil suppliedp) in optional
                                collect `(,var (if ,suppliedp
                                                   (determine-form-value-type ,var ,lambda-environment)
                                                   (quote ,init-form-type)))))
         (optional-forms optional-vars)
         (positional (append required (when optional (append '(&optional) optional))))
         (positional-let*-forms (append required-let*-forms optional-let*-forms))
         (positional-forms (append required-forms optional-forms))
         (rest (when (rest-parameter-p parameters)
                 (parameter-var (rest-parameter parameters))))
         (keywordsp (keyword-parameters-p parameters))
         (keywords (loop
                     for parameter in (keyword-parameters parameters)
                     for keyword = (parameter-keyword parameter)
                     for var = (parameter-var parameter)
                     collect (list (list keyword var) nil (gensym "SUPPLIEDP"))))
         (keyword-let*-forms (loop
                               for parameter in (keyword-parameters parameters)
                               for init-form = (parameter-init-form parameter)
                               for init-form-type = (determine-form-value-type init-form environment)
                               for ((nil var) nil suppliedp) in keywords
                               collect (list var `(if ,suppliedp
                                                      (determine-form-value-type ,var ,lambda-environment)
                                                      ',init-form-type))))
         (keyword-forms (loop
                          for parameter in (keyword-parameters parameters)
                          append (list (parameter-keyword parameter)
                                       (parameter-var parameter))))
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
