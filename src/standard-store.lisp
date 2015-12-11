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

(defgeneric runtime-discriminating-function (standard-store))
(defgeneric compile-time-discriminating-function (standard-store))
(defgeneric store-parameters (standard-store))

(defclass standard-store ()
  ((name :initarg :name
         :reader store-name)
   (lambda-list :initarg :lambda-list
                :reader store-lambda-list)
   (parameters :initarg :parameters
               :reader store-parameters)
   (documentation :initarg :documentation
                  :accessor store-documentation)
   (specializations :initarg :specializations
                    :accessor store-specializations)
   (specialization-class :initarg :specialization-class
                         :reader store-specialization-class)
   (completion-function :initarg :completion-function)
   (form-type-completion-function :initarg :form-type-completion-function)
   (runtime-discriminating-function :initarg :runtime-discriminating-function
                                    :reader runtime-discriminating-function)
   (compile-time-discriminating-function :initarg :compile-time-discriminating-function
                                         :reader compile-time-discriminating-function)
   (runtime-function :initarg :runtime-function)
   (compile-time-function :initarg :compile-time-function))
  (:default-initargs
   :name nil
   :documentation nil    
   :specializations nil
   :form-type-completion-function nil))

(defmethod print-object ((object standard-store) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (store-lambda-list object) stream)))

(defgeneric specialization-parameters (standard-specialization))

(defclass standard-specialization ()
  ((name :initarg :name
         :reader specialization-name)
   (lambda-list :initarg :lambda-list
                :reader specialization-lambda-list)
   (parameters :initarg :parameters
               :reader specialization-parameters)
   (documentation :initarg :documentation
                  :accessor specialization-documentation)
   (function :initarg :function
             :reader specialization-function)
   (expand-function :initarg :expand-function
                    :reader specialization-expand-function)
   (weight :initarg :weight
           :reader specialization-weight))
  (:default-initargs
   :name nil
   :expand-function nil
   :documentation nil
   :weight 1))

(defmethod print-object ((object standard-specialization) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (specialization-lambda-list object) stream)))

;;;; Standard Store Implementation (Object Layer)

(defun compute-discriminating-functions (store specializations)
  (let* ((store-parameters (parse-store-lambda-list (store-lambda-list store)))
         (all-specialization-parameters (loop
                                           for specialization in specializations
                                           collect (parse-specialization-lambda-list
                                                    (specialization-lambda-list specialization))))
         (weights (mapcar #'specialization-weight specializations))
         (dispatch-tree (make-dispatch-tree store-parameters all-specialization-parameters weights)))
    (list (funcall (compile nil (dispatch-tree-to-lambda-form store specializations dispatch-tree :objects)))
          (funcall (compile nil (dispatch-tree-to-lambda-form store specializations dispatch-tree :types))))))

(defun update-discriminating-functions (store)
  (check-type store standard-store)
  (with-slots (runtime-discriminating-function compile-time-discriminating-function) store
    (let* ((specializations (store-specializations store)))
      (destructuring-bind (runtime compile-time) (if (alexandria:emptyp specializations)
                                                     (list (lambda (&rest args)
                                                             (signal-no-applicable-specialization-error store args))
                                                           (lambda (&rest args)
                                                             (declare (ignore args))
                                                             nil))
                                                     (compute-discriminating-functions store specializations))
        (setf runtime-discriminating-function runtime
              compile-time-discriminating-function compile-time)))))

(defmethod shared-initialize :after ((instance standard-store) slot-names
                                     &key
                                       (lambda-list nil lambda-list-p)
                                       (specializations nil specializationsp)
                                       (completion-function nil completion-function-p)
                                       (form-type-completion-function nil form-type-completion-function-p)
                                       &allow-other-keys)
  (labels ((initialise/runtime-function ()
             (with-slots (runtime-function) instance
               (setf runtime-function (funcall completion-function
                                               (lambda (&rest args)
                                                 (let* ((fn (runtime-discriminating-function instance))
                                                        (specialization (funcall fn args)))
                                                   (if specialization
                                                       (apply (specialization-function specialization) args)
                                                       (signal-no-applicable-specialization-error instance args))))))))
           (initialise/compile-time-function ()
             (with-slots (compile-time-function) instance
               (setf compile-time-function (if form-type-completion-function                                      
                                               (funcall form-type-completion-function
                                                        (lambda (form env &rest arg-types)
                                                          (let* ((fn (compile-time-discriminating-function instance))
                                                                 (specialization (funcall fn arg-types)))
                                                            (cond (specialization
                                                                   (let* ((fn (specialization-expand-function specialization)))
                                                                     (if fn
                                                                         (funcall fn form env)
                                                                         form)))
                                                                  (t
                                                                   form)))))
                                               (lambda (form env &rest args)
                                                 (declare (ignore env args))
                                                 form))))))

    (let* ((initialisingp (eql slot-names t))
           (reinitialisingp (null slot-names)))
      (when initialisingp
        (unless (and lambda-list-p completion-function-p)
          (error 'store-error
                 :message (format nil "Store functions must be initialised with a lambda list and a completion function.")
                 :store instance)))
      
      (when initialisingp
        (with-slots (parameters) instance
          (setf parameters (parse-store-lambda-list lambda-list))))
      
      (when (and lambda-list-p reinitialisingp)
        (with-slots (parameters) instance
          (let ((new-parameters (parse-store-lambda-list lambda-list)))
            (unless (congruent-parameters-p parameters new-parameters)
              (error 'store-error
                     :message (format nil "New lambda list ~W is not congruent with the old lambda list ~W."
                                      lambda-list
                                      (store-lambda-list instance))
                     :store instance))
            (setf parameters new-parameters))))
      
      (when (or initialisingp completion-function-p)        
        (initialise/runtime-function))

      (when (or initialisingp form-type-completion-function-p)
        (initialise/compile-time-function))

      (when specializationsp
        (dolist (specialization specializations)
          (unless (congruent-parameters-p (store-parameters instance) (specialization-parameters specialization))
            (error 'store-error
                   :message (format nil "Specialization ~W is not congruent with store ~W."
                                    specialization instance)
                   :store instance))))

      (when (or initialisingp specializationsp)
        (update-discriminating-functions instance)))))

(defmethod funcall-store ((store standard-store) &rest args)
  (with-slots (runtime-function) store
    (apply runtime-function args)))

(defmethod apply-store ((store standard-store) &rest args)
  (with-slots (runtime-function) store
    (apply #'apply runtime-function args)))

(defmethod expand-store ((store standard-store) form &optional env)
  (with-slots (compile-time-function) store
    (funcall compile-time-function form env)))

(defmethod add-specialization ((store standard-store) (specialization standard-specialization))
  (unless (congruent-parameters-p (store-parameters store) (specialization-parameters specialization))
    (error 'store-error :store store :message (format nil "Specialization ~W is not congruent with store ~W."
                                                      specialization store)))

  (loop
     for sublist on (store-specializations store)
     for existing-specialization = (car sublist)
     when (specialization-equal store specialization existing-specialization)
     return (progn (setf (car sublist) specialization)
                   nil)
     finally
       (alexandria:appendf (store-specializations store) (list specialization)))
  (update-discriminating-functions store)
  store)

(defmethod remove-specialization ((store standard-store) (specialization standard-specialization))
  (alexandria:deletef (store-specializations store) specialization
                      :test #'(lambda (a b)
                                (specialization-equal store a b)))
  (update-discriminating-functions store)
  store)

(defmethod specialization-equal ((store standard-store) (a standard-specialization) (b standard-specialization))
  (let* ((parameters-a (specialization-parameters a))
         (parameters-b (specialization-parameters b))
         (parameters (store-parameters store)))
    (labels ((compare/value (keys object)
               (reduce #'funcall keys :initial-value object :from-end t))
             (compare (test &rest keys)
               (funcall test (compare/value keys parameters-a) (compare/value keys parameters-b)))
             (ensure-key (match keyword specialization)
               (assert match nil "Unable to find keyword argument specification ~W in specialization ~W." keyword specialization)))
      (and (compare #'= #'length #'required-parameters)
           (compare #'= #'length #'optional-parameters)
           (compare #'eql #'null #'rest-parameter)
           (compare #'eql #'keyword-parameters-p)
           (loop
              for (nil type-a) in (required-parameters parameters-a)
              for (nil type-b) in (required-parameters parameters-b)
              always
                (alexandria:type= type-a type-b))
           (loop
              with keys-a = (keyword-parameters parameters-a)
              with keys-b = (keyword-parameters parameters-b)
              for (keyword nil) in (keyword-parameters parameters)
              for key-a = (find keyword keys-a :key #'first)
              for key-b = (find keyword keys-b :key #'first)
              for type-a = (or (third key-a) t)
              for type-b = (or (third key-b) t)
              do
                (ensure-key key-a keyword a)
                (ensure-key key-b keyword b)
              always
                (alexandria:type= type-a type-b))))))

;;;; Standard Specialization Implementation (Object Layer)

(defmethod initialize-instance :after ((instance standard-specialization) &key)
  (setf (slot-value instance 'parameters) (parse-specialization-lambda-list (specialization-lambda-list instance))))

;;;; Standard Store Implementation (Glue Layer)

(defmethod ensure-store-using-class ((instance standard-store) store-name lambda-list completion-function form-type-completion-function
                                     &rest args &key store-class specialization-class documentation &allow-other-keys)
  (declare (ignore store-class))
  (apply #'reinitialize-instance instance
         :name store-name
         :lambda-list lambda-list
         :completion-function completion-function
         :form-type-completion-function form-type-completion-function
         :specialization-class specialization-class
         args)

  (with-slots (runtime-function compile-time-function) instance    
    (setf (fdefinition store-name) runtime-function
          (compiler-macro-function store-name) (compiler-macro-lambda (&whole form &rest args &environment env)
                                                 (apply compile-time-function form env args))
          (documentation store-name 'function) documentation))  
    
  instance)


;;;; Standard Specialization Implementation (Glue Layer)

(defmethod ensure-specialization-using-class ((store standard-store) lambda-list function &key expand-function name documentation &allow-other-keys)
  (let* ((specialization (make-instance 'standard-specialization
                                        :name name
                                        :lambda-list lambda-list
                                        :documentation documentation
                                        :function function
                                        :expand-function expand-function)))    
    (when name
      (setf (fdefinition name) function))
    (when (and name expand-function)
      (setf (compiler-macro-function name) (lambda (form env)
                                             (let ((rv (funcall expand-function form env)))
                                               ;; Need to check this
                                               ;; to get a termination
                                               ;; in situations where
                                               ;; the expand function
                                               ;; is generated by
                                               ;; define-specialization.
                                               (if (equal rv form)
                                                   form
                                                   rv)))))
    (add-specialization store specialization)
    specialization))


;;;; Helpers

(defun convert-optional-to-required (parameters)
  (check-type parameters specialization-store.lambda-lists:store-parameters)
  (let* ((required (specialization-store.lambda-lists:required-parameters parameters))
         (optional (specialization-store.lambda-lists:optional-parameters parameters))
         (rest (specialization-store.lambda-lists:rest-parameter parameters))
         (keywordsp (specialization-store.lambda-lists:keyword-parameters-p parameters))
         (keywords (specialization-store.lambda-lists:keyword-parameters parameters))
         (allow-other-keys (specialization-store.lambda-lists:allow-other-keys-p parameters))
         (positional-lambda-list (append required (mapcar #'first optional)))         
         (keyword-lambda-list (when keywordsp
                                `(&key ,@(loop
                                            for (keyword var init-form) in keywords
                                            collect `((,keyword ,var) ,init-form))
                                       ,@(when allow-other-keys
                                            '(&allow-other-keys)))))
         (rest-lambda-list (when rest
                             `(&rest ,rest)))
         (lambda-list `(,@positional-lambda-list ,@rest-lambda-list ,@keyword-lambda-list)))
    (specialization-store.lambda-lists:parse-store-lambda-list lambda-list)))

(defun make-runtime-type-of-lambda-form (parameters)
  (check-type parameters specialization-store.lambda-lists:store-parameters)
  (let* ((parameters (convert-optional-to-required parameters))
         (original-lambda-list (specialization-store.lambda-lists:original-lambda-list parameters))
         (required (specialization-store.lambda-lists:required-parameters parameters))
         (optional (specialization-store.lambda-lists:optional-parameters parameters))
         (rest (specialization-store.lambda-lists:rest-parameter parameters))
         (keywordsp (specialization-store.lambda-lists:keyword-parameters-p parameters))
         (keywords (specialization-store.lambda-lists:keyword-parameters parameters))
         (continuation (gensym "CONTINUATION"))
         (positional-vars (loop
                             for var in required
                             collect `(type-of ,var)))
         (keywords-vars (loop
                           for (keyword-name var nil) in keywords
                           append `(,keyword-name (type-of ,var)))))
    (assert (null optional))
    (cond
      (keywordsp
       (let* ((rest (or rest (gensym "REST")))
              (lambda-list `(,@required &rest ,rest &key ,@keywords &allow-other-keys)))
         `(lambda (,continuation)
            (lambda ,lambda-list
              (apply ,continuation ,@positional-vars ,keywords-vars ,rest)))))
      (rest
       `(lambda (,continuation)
          (lambda ,original-lambda-list
            (apply ,continuation ,@positional-vars (mapcar #'type-of ,rest)))))
      (t
       `(lambda (,continuation)
          (lambda ,original-lambda-list
            (funcall ,continuation ,@positional-vars)))))))


;;;; Dispatch Tree Compiler

;; The code in this section converts the tree computed by
;; MAKE-DISPATCH-TREE in to code.
;;
;; Each dispatch rule queries one or more of the following information
;; constructed from the input arguments.
;; 1. The number of positional arguments.
;; 2. The positional arguments themselves.
;; 3. Keyword arguments.
;;
;; The values of the positional and keyword arguments are types.

(defclass dispatch-tree-symbols ()
  ((all-arguments :initarg :all-arguments)
   (argument-count :initarg :argument-count)
   (positional-arguments :initarg :positional-arguments)
   (keywords-plist :initarg :keywords-plist))
  (:default-initargs
   :all-arguments (gensym "ALL-ARGUMENTS")
   :argument-count (gensym "ARGUMENT-COUNT")
   :positional-arguments (gensym "POSITIONAL-ARGUMENTS")
   :keywords-plist (gensym "KEYWORDS-PLIST")))

(defgeneric predicate-code-for-type (dispatch-rule dispatch-tree-symbols))
(defgeneric predicate-code-for-object (dispatch-rule dispatch-tree-symbols))

(defun dispatch-tree-to-lambda-form/build (store-parameters specializations dispatch-tree code-function dispatch-tree-symbols)
  (check-type dispatch-tree node)
  (check-type dispatch-tree-symbols dispatch-tree-symbols)
  (labels ((process (node knowledge specializations)
             (assert (not (alexandria:emptyp specializations)))
             (cond
               ((leafp node)
                (assert (= 1 (length specializations)))
                (let* ((specialization-parameters (specialization-parameters (first specializations)))
                       (conjoined-rule (make-conjoined-dispatch-rule
                                        (dispatch-rules-for-specialization-parameters store-parameters specialization-parameters)))
                       (rule (remove-rule-tautologies conjoined-rule knowledge)))
                  `(if ,(funcall code-function rule dispatch-tree-symbols)
                       ,(first specializations)
                       nil)))
               (t
                (loop
                   with rule = (node-value node)
                   for specialization in specializations
                   for specialization-parameters = (specialization-parameters specialization)
                   for result = (evaluate-rule rule specialization-parameters)
                   if result
                   collect specialization into left-specializations
                   else
                   collect specialization into right-specializations
                   finally
                     (return (list 'if
                                   (funcall code-function rule dispatch-tree-symbols)
                                   (process (node-left node) (cons rule knowledge) left-specializations)
                                   (process (node-right node) knowledge right-specializations))))))))
    (process dispatch-tree nil specializations)))

(defun dispatch-tree-to-lambda-form (store specializations dispatch-tree lambda-form-type)
  (check-type lambda-form-type (member :types :objects))
  (let* ((code-function (ecase lambda-form-type
                          (:types #'predicate-code-for-type)
                          (:objects #'predicate-code-for-object)))
         (store-parameters (store-parameters store))
         (maximum-required-count (loop
                                    for specialization in specializations
                                    maximizing (length (required-parameters (specialization-parameters specialization)))))
         (keywordsp (keyword-parameters-p store-parameters))
         (keywords-position-diff (if keywordsp
                                     (- (+ (length (required-parameters store-parameters))
                                           (length (optional-parameters store-parameters)))
                                        maximum-required-count)
                                     0))
         (symbols (make-instance 'dispatch-tree-symbols)))
    (with-slots (all-arguments argument-count positional-arguments keywords-plist) symbols
      (cond
        ((zerop maximum-required-count)
         `(lambda ()
            (lambda (,all-arguments)
              (let* ((,argument-count (length ,all-arguments))
                     (,keywords-plist ,all-arguments))
                (declare (ignorable ,argument-count ,keywords-plist))
                ,(dispatch-tree-to-lambda-form/build store-parameters specializations dispatch-tree code-function symbols)))))
        (t
         (let ((arg (gensym "ARG"))
               (index (gensym "INDEX")))
           `(lambda ()
              (lambda (,all-arguments)
                (let* ((,argument-count 0)
                       (,keywords-plist nil)
                       (,positional-arguments (make-array ,maximum-required-count)))
                  (declare (ignorable ,argument-count ,keywords-plist ,positional-arguments))
                  (loop
                     for ,arg on ,all-arguments
                     for ,index from 0 below ,maximum-required-count
                     do
                       (setf (aref ,positional-arguments ,index) (car ,arg))
                       (incf ,argument-count)
                     finally
                       (setf ,keywords-plist (nthcdr ,keywords-position-diff ,arg)))
                  (incf ,argument-count (length ,keywords-plist))
                  ,(dispatch-tree-to-lambda-form/build store-parameters specializations dispatch-tree code-function symbols))))))))))

;;;; Predicate code for object implementations
(defmethod predicate-code-for-object ((rule parameter-count-bound-rule) dispatch-tree-symbols)
  (destructuring-bind (lower upper) (parameter-count-bound rule)
    (let ((symbol (slot-value dispatch-tree-symbols 'argument-count)))
      `(<= ,lower ,symbol ,upper))))

(defmethod predicate-code-for-object ((rule positional-parameter-type-rule) dispatch-tree-symbols)
  (with-slots (positional-arguments) dispatch-tree-symbols
    (let* ((position (parameter-position rule))
           (type (parameter-type rule)))
      `(typep (elt ,positional-arguments ,position) ',type))))

(defmethod predicate-code-for-object ((rule keyword-parameter-type-rule) dispatch-tree-symbols)
  (let* ((keywords-plist (slot-value dispatch-tree-symbols 'keywords-plist))
         (keyword (parameter-keyword rule))
         (type (parameter-type rule))
         (value (gensym "VALUE"))
         (found? (gensym "FOUND?")))
    `(multiple-value-bind (,value ,found?) (getf2 ,keywords-plist ,keyword)
       (unless ,found?
         (error "No keyword argument present for ~W." ,keyword))
       (typep ,value ',type))))

(defmethod predicate-code-for-object ((rule conjoined-dispatch-rule) dispatch-tree-symbols)
  `(and ,@(loop
             for subrule in (rules rule)
             collect (predicate-code-for-object subrule dispatch-tree-symbols))))

(defmethod predicate-code-for-object ((rule constantly-rule) dispatch-tree-symbol)
  (declare (ignore dispatch-tree-symbol))
  (constantly-rule-value rule))

;;;; Predicate code for type implementations.
(defmethod predicate-code-for-type ((rule parameter-count-bound-rule) dispatch-tree-symbols)
  (destructuring-bind (lower upper) (parameter-count-bound rule)
    (let ((symbol (slot-value dispatch-tree-symbols 'argument-count)))
      `(<= ,lower ,symbol ,upper))))

(defmethod predicate-code-for-type ((rule positional-parameter-type-rule) dispatch-tree-symbols)
  (with-slots (positional-arguments) dispatch-tree-symbols
    (let* ((position (parameter-position rule))
           (type (parameter-type rule)))
      `(subtypep (elt ,positional-arguments ,position) ',type))))

(defmethod predicate-code-for-type ((rule keyword-parameter-type-rule) dispatch-tree-symbols)
  (let* ((keywords-plist (slot-value dispatch-tree-symbols 'keywords-plist))
         (keyword (parameter-keyword rule))
         (type (parameter-type rule))
         (value (gensym "VALUE"))
         (found? (gensym "FOUND?")))
    `(multiple-value-bind (,value ,found?) (getf2 ,keywords-plist ,keyword)
       (unless ,found?
         (error "No keyword argument present for ~W." ,keyword))
       (subtypep ,value ',type))))

(defmethod predicate-code-for-type ((rule conjoined-dispatch-rule) dispatch-tree-symbols)
  `(and ,@(loop
             for subrule in (rules rule)
             collect (predicate-code-for-type subrule dispatch-tree-symbols))))

(defmethod predicate-code-for-type ((rule constantly-rule) dispatch-tree-symbol)
  (declare (ignore dispatch-tree-symbol))
  (constantly-rule-value rule))

(defun getf2 (plist indicator &optional default)
  (loop
     for arg on plist by #'cddr
     when (eql (car arg) indicator)
     return (values (cadr arg) t)
     finally (return (values default nil))))
