(in-package "SPECIALIZATION-STORE.DISPATCH")


;;;; Binary Tree

(defstruct (node (:constructor %make-node))
  (value nil)
  (left nil)
  (right nil))

(defun make-node (&optional value left right)
  (check-type left (or null node))
  (check-type right (or null node))
  (%make-node :left left :right right :value value))

(defun leafp (node)
  (and (null (node-left node))
       (null (node-right node))))

(defun split-leaf (node splitting-function)
  (assert (leafp node))
  (let ((split? (funcall splitting-function (node-value node))))
    (cond (split? (destructuring-bind (new-value left-value right-value) split?
		    (make-node new-value
			       (make-node left-value)
			       (make-node right-value))))
	  (t node))))

(defun deepen-tree (node test-function splitting-function)
  ;; Implement this the easy way. The trees will be short.
  (labels ((process (node)
	     (cond ((leafp node)
		    (if (funcall test-function (node-value node))
			(values (split-leaf node splitting-function) t)
			(values node nil)))
		   (t
		    (multiple-value-bind (left-node left?) (process (node-left node))
		      (multiple-value-bind (right-node right?) (process (node-right node))
			(values (make-node (node-value node) left-node right-node)
				(or left? right?))))))))
    (process node)))

;;;; Rules 
;;
;; A rule is something that must be satisfied in order for a
;; specialization to be invoked.
;;
;; A dispatch rule is a rule that can be used to discriminate between
;; two or specializations.

(defgeneric rule-equal (rule-a rule-b))
(defgeneric evaluate-rule (rule specialization-parameters))
(defgeneric remove-rule-tautologies (rule known-rule))

(defclass rule ()
  ())

(defclass dispatch-rule (rule)
  ())

;;;; Fixed argument count bound rule
(deftype lambda-parameter-count ()
  `(integer 0 ,lambda-parameters-limit))

(defgeneric argument-count (dispatch-rule))

(defclass fixed-argument-count-rule (dispatch-rule)
  ((count :initarg :count
          :reader argument-count))
  (:documentation "Ensure the argument count x is equal to count."))

(defmethod print-object ((object fixed-argument-count-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~d" (argument-count object))))

;;;; Positional Parameter Type Rule
(defgeneric parameter-position (dispatch-rule))
(defgeneric parameter-type (dispatch-rule))

(defclass positional-parameter-type-rule (dispatch-rule)
  ((position :initarg :position
             :reader parameter-position)
   (type :initarg :type
         :reader parameter-type)))

(defmethod print-object ((object positional-parameter-type-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (position type) object
      (format stream "~d ~W" position type))))

;;;; Keyword Parameter Type Rule

(defgeneric parameter-keyword (dispatch-rule))
;; (defgeneric parameter-type (dispatch-rule)) ;; This is defined earlier.

(defclass keyword-parameter-type-rule (dispatch-rule)
  ((keyword :initarg :keyword
            :reader parameter-keyword)
   (type :initarg :type
         :reader parameter-type)))

(defmethod print-object ((object keyword-parameter-type-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (keyword type) object
      (format stream "~W ~W" keyword type))))

;;;; Conjoined Dispatch Rule

(defgeneric rules (dispatch-rule))

(defclass conjoined-dispatch-rule (dispatch-rule)
  ((rules :initarg :rules
          :reader rules)))

(defmethod print-object ((object conjoined-dispatch-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (rules) object
      (write rules :stream stream))))

;;;; Consantly rule
(defgeneric constantly-rule-value (dispatch-rule))

(defclass constantly-rule (dispatch-rule)
  ((value :initarg :value
          :reader constantly-rule-value)))

(defmethod print-object ((object constantly-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (value) object
      (write value :stream stream))))

;;;; Constructors
(defun make-fixed-argument-count-rule (count)
  (check-type count lambda-parameter-count)
  (make-instance 'fixed-argument-count-rule :count count))

(defun make-positional-parameter-type-rule (position type)
  (make-instance 'positional-parameter-type-rule :position position :type type))

(defun make-keyword-parameter-type-rule (keyword type)
  (make-instance 'keyword-parameter-type-rule :keyword keyword :type type))

(defun conjoin-dispatch-rules (&rest rules)
  (make-conjoined-dispatch-rule rules))

(defun make-conjoined-dispatch-rule (rules)
  (make-instance 'conjoined-dispatch-rule :rules rules))

(defun make-constantly-rule (value)
  (make-instance 'constantly-rule :value value))

;;;; Functions

(defun other-keys-p (store-parameters specialization-parameters)
  (and (keyword-parameters-p store-parameters)
       (keyword-parameters-p specialization-parameters)       
       (> (length (keyword-parameters specialization-parameters))
	  (length (keyword-parameters store-parameters)))))

(defun specialization-parameters-lower-bound (specialization-parameters)
  (length (required-parameters specialization-parameters)))

(defun specialization-parameters-upper-bound (specialization-parameters)
  (cond
    ((or (rest-parameter specialization-parameters)
	 (keyword-parameters-p specialization-parameters))
     lambda-parameters-limit)
    (t
     (+ (length (required-parameters specialization-parameters))
	(length (optional-parameters specialization-parameters))))))

;;;; Training
(defun fixed-arity-store-parameters-p (store-parameters)
  (or (keyword-parameters-p store-parameters)
      (null (rest-parameter store-parameters))))

(defun variable-arity-store-parameters-p (store-parameters)
  (and (rest-parameter store-parameters)
       (not (keyword-parameters-p store-parameters))))

(defun make-initial-dispatch-tree (store-parameters all-specialization-parameters all-weights)
  (cond ((null all-specialization-parameters)
         (make-constantly-rule nil))        
        ((fixed-arity-store-parameters-p store-parameters)
         (specialization-store.dispatch.fixed-arity:make-initial-dispatch-tree store-parameters all-specialization-parameters all-weights))
        (t
         (error "Not implemented yet."))))

(defmethod remove-rule-tautologies ((rule t) (known-rules list))
  (reduce #'(lambda (current-rule known-rule)
	      (remove-rule-tautologies current-rule known-rule))
	  known-rules
	  :initial-value rule))

(defmethod remove-rule-tautologies ((rule t) (known-rule t))
  (if (rule-equal rule known-rule)
      (make-constantly-rule t)      
      rule))

(defun remove-dispatch-tree-tautologies (tree)
  (labels ((process (node knowledge)
	     (cond ((null node)
                    nil)
                   ((leafp node)
                    node)
                   (t
                    (let* ((rule (node-value node))
                           (new-rule (remove-rule-tautologies rule knowledge))
                           (new-knowledge (cons new-rule knowledge)))
                      (make-node new-rule
                                 (process (node-left node) new-knowledge)
                                 (process (node-right node) knowledge)))))))
    (process tree nil)))

(defun remove-dispatch-tree-constant-rules (tree)
  (labels ((process (node)
             (when node
               (let ((value (node-value node)))
                 (cond ((leafp node)
                        node)
                       ((typep value 'constantly-rule)
                        (if (constantly-rule-value value)
                            (process (node-left node))
                            (process (node-right node))))
                       (t
                        (make-node value
                                   (process (node-left node))
                                   (process (node-right node)))))))))
    (process tree)))

(defun make-dispatch-tree (store-parameters all-specialization-parameters all-weights)
  (remove-dispatch-tree-constant-rules
   (remove-dispatch-tree-tautologies
    (make-initial-dispatch-tree store-parameters all-specialization-parameters all-weights))))

;;;; Rule Implementation
(defun compare-slot-values (slot-name test-fn &rest objects)
  (apply test-fn (mapcar #'(lambda (object)
			     (slot-value object slot-name))
			 objects)))

(defmethod rule-equal ((rule-a t) (rule-b t))
  nil)

(defmethod rule-equal ((rule-a positional-parameter-type-rule) (rule-b positional-parameter-type-rule))
  (and (compare-slot-values 'position #'= rule-a rule-b)
       (compare-slot-values 'type #'alexandria:type= rule-a rule-b)))

(defmethod rule-equal ((rule-a keyword-parameter-type-rule) (rule-b keyword-parameter-type-rule))
  (and (compare-slot-values 'keyword #'eql rule-a rule-b)
       (compare-slot-values 'type #'alexandria:type= rule-a rule-b)))

(defmethod rule-equal ((rule-a conjoined-dispatch-rule) (rule-b conjoined-dispatch-rule))
  (every #'rule-equal (slot-value rule-a 'rules) (slot-value rule-b 'rules)))

(defmethod evaluate-rule ((rule fixed-argument-count-rule) (specialization-parameters specialization-parameters))
  (= (argument-count rule)
     (specialization-parameters-lower-bound specialization-parameters)
     (specialization-parameters-upper-bound specialization-parameters)))

(defmethod evaluate-rule ((rule positional-parameter-type-rule) (specialization-parameters specialization-parameters))
  (with-slots (position type) rule
    (let ((required-parameter (nth position (required-parameters specialization-parameters))))
      (cond ((and (rest-parameter specialization-parameters)
                  (null (keyword-parameters specialization-parameters))
                  (null required-parameter))
             (subtypep t type))
            (required-parameter
             (destructuring-bind (var var-type) required-parameter
               (declare (ignore var))
               (subtypep var-type type)))))))

(defmethod evaluate-rule ((rule keyword-parameter-type-rule) (specialization-parameters specialization-parameters))
  (with-slots (keyword type) rule
    (let ((keyword-parameter (find keyword (keyword-parameters specialization-parameters) :key #'first)))
      (assert keyword-parameter nil "Unable to find keyword parameter ~W in specialization parameters ~W."
	      keyword specialization-parameters)
      (destructuring-bind (keyword var var-type supplied-p-var) keyword-parameter
	(declare (ignore keyword var supplied-p-var))
	(subtypep (or var-type t) type)))))

(defmethod evaluate-rule ((rule conjoined-dispatch-rule) (specialization-parameters specialization-parameters))
  (loop
     for r in (slot-value rule 'rules)
     always (evaluate-rule r specialization-parameters)))

(defmethod evaluate-rule ((rule constantly-rule) specialization-parameters)
  (declare (ignore specialization-parameters))
  (constantly-rule-value rule))

(defmethod remove-rule-tautologies ((rule conjoined-dispatch-rule) known-rule)
  (with-slots (rules) rule
    (cond
      ((find-if #'(lambda (x)
		    (and (typep x 'constantly-rule)
			 (null (constantly-rule-value x))))
		rules)
       (make-constantly-rule nil))
      (t
       (let* ((known-rules (list known-rule))
              (new-rules (loop
                            for rule in rules
                            for new-rule = (remove-rule-tautologies rule known-rules)
                            when (and (typep new-rule 'constantly-rule)
                                      (null (constantly-rule-value new-rule)))
                            return (list new-rule)
                            unless (and (typep new-rule 'constantly-rule)
                                        (constantly-rule-value new-rule))
                            do
                              (push new-rule known-rules)
                            and collect new-rule)))
	 (cond
	   ((null new-rules)
	    (make-constantly-rule t))
	   ((null (rest new-rules))
	    (first new-rules))
	   (t
	    (make-instance 'conjoined-dispatch-rule :rules new-rules))))))))

(defmethod remove-rule-tautologies ((rule t) (known-rule conjoined-dispatch-rule))
  (remove-rule-tautologies rule (slot-value known-rule 'rules)))

(defmethod remove-rule-tautologies ((rule positional-parameter-type-rule) (known-rule fixed-argument-count-rule))
  (let* ((count (argument-count known-rule))
         (applicable? (< (parameter-position rule) count)))
    (cond ((and applicable? (alexandria:type= (parameter-type rule) t))
           (make-constantly-rule t))
          ((not applicable?)
           (make-constantly-rule nil))
          (t
           rule))))
