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
(defgeneric remove-rule-tautologies (rule known-rule))
(defgeneric remove-constant-rules (rule specializations))

(defclass rule ()
  ())

(defclass dispatch-rule (rule)
  ())

;;;; Parameter Count Bound Rule
(defgeneric parameter-count-upper-bound (dispatch-rule))
(defgeneric parameter-count-lower-bound (dispatch-rule))
(defgeneric parameter-count-bound (dispatch-rule))

(defclass parameter-count-bound-rule (dispatch-rule)
  ((lower-bound :initarg :lower-bound
                :reader parameter-count-lower-bound)
   (upper-bound :initarg :upper-bound
                :reader parameter-count-upper-bound)))

(defmethod print-object ((object parameter-count-bound-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (lower-bound upper-bound) object
      (format stream "[~W, ~W]" lower-bound upper-bound))))

(defmethod parameter-count-bound ((rule parameter-count-bound-rule))
  (list (parameter-count-lower-bound rule)
        (parameter-count-upper-bound rule)))

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
(defun make-parameter-count-bound-rule (lower-bound upper-bound)
  (make-instance 'parameter-count-bound-rule :lower-bound lower-bound :upper-bound upper-bound))

(defun make-positional-parameter-type-rule (position type)
  (make-instance 'positional-parameter-type-rule :position position :type type))

(defun make-keyword-parameter-type-rule (keyword type)
  (make-instance 'keyword-parameter-type-rule :keyword keyword :type type))

(defun conjoin-dispatch-rules (&rest rules)
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
  ((store-parameters :initarg :store-parameters)
   (all-specialization-parameters :initarg :all-specialization-parameters)
   (weights :initarg :weights)))

(defmethod print-object ((object data) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (all-specialization-parameters) object
      (format stream "~d specialization~:P" (length all-specialization-parameters)))))

(defun make-data (store-parameters all-specialization-parameters weights)
  (make-instance 'data
		 :store-parameters store-parameters
		 :all-specialization-parameters all-specialization-parameters
		 :weights weights))

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
    (with-slots (store-parameters all-specialization-parameters weights) data
      (let* ((dispatch-rules (loop
				for specialization-parameters in all-specialization-parameters
				append (dispatch-rules-for-specialization-parameters store-parameters specialization-parameters)))
	     (trials (loop
			for dispatch-rule in dispatch-rules
			for trial = (let (a-weights a-specializations b-weights b-specializations)
				      (loop
					 for specialization in all-specialization-parameters
					 for weight in weights
					 do
					   (cond
					     ((evaluate-rule dispatch-rule specialization)
					      (push weight a-weights)
					      (push specialization a-specializations))
					     (t
					      (push weight b-weights)
					      (push specialization b-specializations))))
				      (list (or (zerop (length a-weights))
						(zerop (length b-weights)))
					    (abs (- (partition-weight a-weights)
						    (partition-weight b-weights)))
					    ;; The order here must align with the
					    ;; splitting function protocol in
					    ;; split-leaf.
					    dispatch-rule
					    (make-data store-parameters a-specializations a-weights)
					    (make-data store-parameters b-specializations b-weights)))
			unless (first trial)
			collect (rest trial))))
	(unless trials
	  (error "Unable to split the specializations ~W." all-specialization-parameters))
	(rest (reduce #'(lambda (current next)
			  (if (trial< current next)
			      current
			      next))
		      (rest trials)
		      :initial-value (first trials)))))))

(defun split-data-p (data)
  (with-slots (all-specialization-parameters) data
    (> (length all-specialization-parameters) 1)))

(defun make-initial-dispatch-tree (store-parameters all-specialization-parameters all-weights)
  (labels ((process (tree)
	     (multiple-value-bind (new-tree split?) (deepen-tree tree #'split-data-p #'split-data)
	       (if split?
		   (process new-tree)
		   new-tree))))
    (process (make-node (make-data store-parameters all-specialization-parameters all-weights)))))

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
	     (cond
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

(defmethod remove-constant-rules ((rule constantly-rule) (specializations t))
  rule)

(defmethod remove-constant-rules ((rule t) (all-specialization-parameters list))
  (let* ((results (mapcar #'(lambda (specialization-parameters)
			      (evaluate-rule rule specialization-parameters))
			  all-specialization-parameters))
	 (unique-results (remove-duplicates results))
	 (unique-result-count (length unique-results)))
    (case unique-result-count
      (0 (make-constantly-rule t))
      (1 (make-constantly-rule (first unique-results)))
      (otherwise rule))))

(defun remove-dispatch-tree-constant-rules (tree)
  (labels ((node-specialization-parameters (node)
	     (cond
	       ((leafp node)
		(slot-value (node-value node) 'all-specialization-parameters))
	       (t
		(append (node-specialization-parameters (node-left node))
			(node-specialization-parameters (node-right node))))))
	   (process (node)
	     (cond
	       ((leafp node)
		node)
	       (t
		(let* ((current-rule (node-value node))
		       (new-rule (remove-constant-rules current-rule (node-specialization-parameters node))))
		  (make-node new-rule
			     (process (node-left node))
			     (process (node-right node))))))))
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
       (let ((new-rules (loop
			   for rule in rules
			   for new-rule = (remove-rule-tautologies rule known-rule)
			   when (and (typep new-rule 'constantly-rule)
				     (null (constantly-rule-value new-rule)))
			   return (list new-rule)
			   unless (and (typep new-rule 'constantly-rule)
				       (constantly-rule-value new-rule))
			   collect new-rule)))
	 (cond
	   ((null new-rules)
	    (make-constantly-rule t))
	   ((null (rest new-rules))
	    (first new-rules))
	   (t
	    (make-instance 'conjoined-dispatch-rule :rules new-rules))))))))

(defmethod remove-rule-tautologies ((rule t) (known-rule conjoined-dispatch-rule))
  (remove-rule-tautologies rule (slot-value known-rule 'rules)))

(defmethod remove-constant-rules ((rule conjoined-dispatch-rule) (all-specialization-parameters t))
  (with-slots (rules) rule
    (let ((new-rules (loop
			for rule in rules
			for new-rule = (remove-constant-rules rule all-specialization-parameters)
			when (and (typep new-rule 'constantly-rule)
				  (null (constantly-rule-value new-rule)))
			return (list new-rule)
			unless (and (typep new-rule 'constantly-rule)
				    (constantly-rule-value new-rule))
			collect new-rule)))
      (cond
	((null new-rules)
	 (make-constantly-rule t))
	((null (rest new-rules))
	 (first new-rules))
	(t
	 (make-instance 'conjoined-dispatch-rule :rules new-rules))))))
