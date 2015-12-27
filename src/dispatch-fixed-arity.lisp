(in-package "SPECIALIZATION-STORE.DISPATCH.FIXED-ARITY")

(defgeneric set-specialization-parameters (set-specialization))
(defgeneric set-specialization-weight (set-specialization))

(defclass set-specialization ()
  ((specialization-parameters
    :initarg :parameters
    :reader set-specialization-parameters)
   (weight
    :initarg :weight
    :reader set-specialization-weight)))

(defmethod print-object ((object set-specialization) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~W :weight ~W"
            (set-specialization-parameters object)
            (set-specialization-weight object))))

(defun make-set-specialization (specialization-parameters weight)
  (make-instance 'set-specialization
                 :parameters specialization-parameters
                 :weight weight))

(defmethod evaluate-rule (rule (set-specialization set-specialization))
  (evaluate-rule rule (set-specialization-parameters set-specialization)))

(defgeneric set-store-parameters (set))
(defgeneric set-specializations (set))

(defclass set ()
  ((store-parameters
    :initarg :store-parameters
    :reader set-store-parameters)
   (set-specializations
    :initarg :set-specializations
    :reader set-specializations)))

(defmethod print-object ((object set) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~d specialization~:P" (length (set-specializations object)))))

(defun make-set (store-parameters set-specializations)
  (make-instance 'set
                 :store-parameters store-parameters
                 :set-specializations set-specializations))

(defun split-node-value-p (value)
  (cond ((typep value 'set)         
         (> (length (set-specializations value)) 1))
        ((typep value 'specialization-parameters)
         nil)
        ((null value)
         nil)
        (t
         (error "Invalid node value."))))

(defun dispatch-rules (store-parameters specialization-parameters)
  (assert (fixed-arity-store-parameters-p store-parameters))
  (append (loop
             for (nil type) in (required-parameters specialization-parameters)
             for index from 0
             collect (make-positional-parameter-type-rule index type))
          (loop
             with store-keywords = (mapcar #'first (keyword-parameters store-parameters))
             for (keyword nil type) in (keyword-parameters specialization-parameters)
             when (member keyword store-keywords)
             collect (make-keyword-parameter-type-rule keyword type))))

(defun split-set (set)
  (labels ((all-dispatch-rules ()
             (let* ((store-parameters (set-store-parameters set)))
               (remove-duplicates (reduce #'append (set-specializations set)
                                          :key #'(lambda (set-specialization)
                                                   (dispatch-rules store-parameters
                                                                   (set-specialization-parameters set-specialization))))
                                  :test #'rule-equal)))
           (trial-rule (rule)
             (loop
                with set-X = nil
                with set-Y = nil
                for set-specialization in (set-specializations set)
                do
                  (cond ((evaluate-rule rule set-specialization)
                         (push set-specialization set-X))
                        (t
                         (push set-specialization set-Y)))
                finally
                  (return (list (abs (- (reduce #'+ set-X :key #'set-specialization-weight :initial-value 0)
                                        (reduce #'+ set-Y :key #'set-specialization-weight :initial-value 0)))
                                rule set-X set-Y))))
           (trial< (trial-a trial-b)
             (if (< (first trial-a) (first trial-b))
                 trial-a
                 trial-b))
           (make-leaf (set-specializations)
             (if (rest set-specializations)
                 (make-set (set-store-parameters set) set-specializations)
                 (set-specialization-parameters (first set-specializations)))))
    (let* ((trials (loop
                      for dispatch-rule in (all-dispatch-rules)
                      collect
                        (trial-rule dispatch-rule)))
           (best (reduce #'trial< (rest trials) :initial-value (first trials))))
      (destructuring-bind (value rule set-X set-Y) best
        (declare (ignore value))
        (unless set-Y
          (error "Unable to split the specializations in the set ~A." set))
        (list rule (make-leaf set-X) (make-leaf set-Y))))))

(defun ensure-applicable (store-parameters tree)
  (labels ((process-chain (dispatch-rules specialization-parameters)
             (cond (dispatch-rules
                    (destructuring-bind (dispatch-rule &rest others) dispatch-rules
                      (make-node dispatch-rule
                                 (process-chain others specialization-parameters)
                                 (make-node))))
                   (t
                    (make-node specialization-parameters))))
           (process (node)
             (let ((value (node-value node)))
               (cond ((leafp node)
                      (if value
                          (let* ((specialization-parameters (node-value node))
                                 (dispatch-rules (dispatch-rules store-parameters specialization-parameters)))
                            (process-chain dispatch-rules specialization-parameters))
                          nil))
                     (t
                      (make-node value
                                 (process (node-left node))
                                 (process (node-right node))))))))
    (process tree)))

(defun make-initial-dispatch-tree (store-parameters all-specialization-parameters all-weights)
  (assert (fixed-arity-store-parameters-p store-parameters))
  (labels ((process (tree)
             (multiple-value-bind (new-tree split?) (deepen-tree tree #'split-node-value-p #'split-set)
               (if split?
                   (process new-tree)
                   new-tree))))
    (let* ((arity (specialization-parameters-upper-bound (first all-specialization-parameters)))
           (set-specializations (mapcar #'make-set-specialization all-specialization-parameters all-weights))
           (node (make-node (cond ((rest set-specializations)
                                   (make-set store-parameters set-specializations))
                                  ((= 1 (length set-specializations))
                                   (first all-specialization-parameters))
                                  (t
                                   (make-constantly-rule nil)))))
           (tree (if (< arity lambda-parameters-limit)
                     (make-node (make-fixed-argument-count-rule arity)
                                node
                                (make-node))
                     node)))
      (ensure-applicable store-parameters
                         (process tree)))))
