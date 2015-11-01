(in-package "SPECIALIZATION-STORE.DISPATCH")


;;;; Binary Tree

(defstruct (node (:constructor %make-node))
  (value nil)
  (left nil)
  (right nil))

(defun make-node (&optional value left right)
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

(defclass rule ()
  ())

(defclass dispatch-rule (rule)
  ())

(defclass parameter-count-bound-rule (dispatch-rule)
  ((lower-bound :initarg :lower-bound)
   (upper-bound :initarg :upper-bound)))

(defmethod print-object ((object parameter-count-bound-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (lower-bound upper-bound) object
      (format stream "[~W, ~W]" lower-bound upper-bound))))

(defclass positional-parameter-type-rule (dispatch-rule)
  ((position :initarg :position)
   (type :initarg :type)))

(defmethod print-object ((object positional-parameter-type-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (position type) object
      (format stream "~d ~W" position type))))

(defclass keyword-parameter-type-rule (dispatch-rule)
  ((keyword :initarg :keyword)
   (type :initarg :type)))

(defmethod print-object ((object keyword-parameter-type-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (keyword type) object
      (format stream "~W ~W" keyword type))))

(defclass conjoined-dispatch-rule (dispatch-rule)
  ((rules :initarg :rules)))

(defmethod print-object ((object conjoined-dispatch-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (rules) object
      (write rules :stream stream))))

;;;; Constructors
(defun make-parameter-count-bound-rule (lower-bound upper-bound)
  (make-instance 'parameter-count-bound-rule :lower-bound lower-bound :upper-bound upper-bound))

(defun make-positional-parameter-type-rule (position type)
  (make-instance 'positional-parameter-type-rule :position position :type type))

(defun make-keyword-parameter-type-rule (keyword type)
  (make-instance 'keyword-parameter-type-rule :keyword keyword :type type))

(defun conjoin-dispatch-rules (&rest rules)
  (make-instance 'conjoined-dispatch-rule :rules rules))

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

(defun dispatch-rules-for-specialization-parameters (store-parameters specialization-parameters)
  (let* ((lower-bound (specialization-parameters-lower-bound specialization-parameters))
	 (upper-bound (specialization-parameters-upper-bound specialization-parameters))
	 (parameter-count-rule (make-parameter-count-bound-rule lower-bound upper-bound))
	 (parameter-rules (append (loop
				     for (nil type) in (required-parameters specialization-parameters)
				     for position from 0
				     collect (conjoin-dispatch-rules (make-positional-parameter-type-rule position type)
								     parameter-count-rule))
				  (loop
				     with sp-keys = (keyword-parameters specialization-parameters)
				     for (st-keyword nil) in (keyword-parameters store-parameters)
				     for (keyword nil type nil) = (find st-keyword sp-keys :key #'first)
				     collect (conjoin-dispatch-rules (make-keyword-parameter-type-rule keyword type)
								     parameter-count-rule)))))
    parameter-rules))

;;;; Training

(defclass data ()
  ((specializations :initarg :specializations)
   (weights :initarg :weights)
   (remaining-dispatch-rules :initarg :remaining-dispatch-rules)))

(defmethod print-object ((object data) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (specializations remaining-dispatch-rules) object
      (format stream "~d rule~:P remaining for ~d specialization~:P"
	      (length remaining-dispatch-rules)
	      (length specializations)))))

(defun make-data (specializations weights remaining-dispatch-rules)
  (make-instance 'data
		 :specializations specializations
		 :weights weights
		 :remaining-dispatch-rules remaining-dispatch-rules))

(defun make-root-data (store-parameters all-specialization-parameters all-weights)
  (let ((dispatch-rules (loop
			   for specialization-parameters in all-specialization-parameters
			   append (dispatch-rules-for-specialization-parameters store-parameters specialization-parameters))))
    (make-data all-specialization-parameters
	       all-weights
	       (remove-duplicates dispatch-rules :test #'rule-equal))))

(defun split-data (data)
  (flet ((partition-weight (weights)
	   (reduce #'+ weights :initial-value 0))
	 (trial< (trial-a trial-b)
	   (let ((difference-a (first trial-a))
		 (difference-b (first trial-b)))
	     (cond
	       ((< difference-a difference-b)
		t)
	       (t
		nil)))))
    (with-slots (specializations weights remaining-dispatch-rules) data
      (cond
	((null remaining-dispatch-rules)
	 (error "Unable to discimrinate between specializations ~W." specializations))
	(t
	 (let ((trials (loop
			  for dispatch-rule in remaining-dispatch-rules
			  for other-rules = (remove dispatch-rule remaining-dispatch-rules)
			  collect
			    (let (a-weights a-specializations b-weights b-specializations)
			      (loop
				 for specialization in specializations
				 for weight in weights
				 do
				   (cond
				     ((evaluate-rule dispatch-rule specialization)
				      (push weight a-weights)
				      (push specialization a-specializations))
				     (t
				      (push weight b-weights)
				      (push specialization b-specializations))))
			      (list (abs (- (partition-weight a-weights)
					    (partition-weight b-weights)))
				    ;; The order here must align with the
				    ;; splitting function protocol in
				    ;; split-leaf.
				    dispatch-rule
				    (make-data a-specializations a-weights other-rules)
				    (make-data b-specializations b-weights other-rules))))))
	   (rest (reduce #'(lambda (current next)
			     (if (trial< current next)
				 current
				 next))
			 (rest trials)
			 :initial-value (first trials)))))))))

(defun split-data-p (data)
  (with-slots (specializations) data
    (> (length specializations) 1)))

(defun make-dispatch-tree (store-parameters all-specialization-parameters all-weights)
  (labels ((process (tree)
	     (multiple-value-bind (new-tree split?) (deepen-tree tree #'split-data-p #'split-data)
	       (if split?
		   (process new-tree)
		   new-tree))))
    (process (make-node (make-root-data store-parameters all-specialization-parameters all-weights)))))

;;;; Rule Implementation
(defun compare-slot-values (slot-name test-fn &rest objects)
  (apply test-fn (mapcar #'(lambda (object)
			     (slot-value object slot-name))
			 objects)))

(defmethod rule-equal ((rule-a t) (rule-b t))
  nil)

(defmethod rule-equal ((rule-a parameter-count-bound-rule) (rule-b parameter-count-bound-rule))
  (and (compare-slot-values 'lower-bound #'= rule-a rule-b)
       (compare-slot-values 'upper-bound #'= rule-a rule-b)))

(defmethod rule-equal ((rule-a positional-parameter-type-rule) (rule-b positional-parameter-type-rule))
  (and (compare-slot-values 'position #'= rule-a rule-b)
       (compare-slot-values 'type #'alexandria:type= rule-a rule-b)))

(defmethod rule-equal ((rule-a keyword-parameter-type-rule) (rule-b keyword-parameter-type-rule))
  (and (compare-slot-values 'keyword #'eql rule-a rule-b)
       (compare-slot-values 'type #'alexandria:type= rule-a rule-b)))

(defmethod rule-equal ((rule-a conjoined-dispatch-rule) (rule-b conjoined-dispatch-rule))
  (every #'rule-equal (slot-value rule-a 'rules) (slot-value rule-b 'rules)))

(defmethod evaluate-rule ((rule parameter-count-bound-rule) (specialization-parameters specialization-parameters))
  (with-slots (lower-bound upper-bound) rule
    (<= lower-bound
	(specialization-parameters-lower-bound specialization-parameters)       
	(specialization-parameters-upper-bound specialization-parameters)
	upper-bound)))

(defmethod evaluate-rule ((rule positional-parameter-type-rule) (specialization-parameters specialization-parameters))
  (with-slots (position type) rule
    (let ((required-parameter (nth position (required-parameters specialization-parameters))))
      (when required-parameter
	(destructuring-bind (var var-type) required-parameter
	  (declare (ignore var))
	  (subtypep var-type type))))))

(defmethod evaluate-rule ((rule keyword-parameter-type-rule) (specialization-parameters specialization-parameters))
  (with-slots (keyword type) rule
    (let ((keyword-parameter (find keyword (keyword-parameters specialization-parameters) :key #'first)))
      (assert keyword-parameter nil "Unable to find keyword parameter ~W in specialization parameters ~W."
	      keyword specialization-parameters)
      (destructuring-bind (keyword var var-type supplied-p-var) keyword-parameter
	(declare (ignore keyword var supplied-p-var))
	(subtypep var-type type)))))

(defmethod evaluate-rule ((rule conjoined-dispatch-rule) (specialization-parameters specialization-parameters))
  (loop
     for r in (slot-value rule 'rules)
     always (evaluate-rule r specialization-parameters)))
