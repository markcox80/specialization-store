(in-package "SPECIALIZATION-STORE.DISPATCH.FIXED-ARITY")


;;;; Knowledge

(defclass knowledge ()
  ((set :initarg :set
        :type simple-vector)))

(defun make-knowledge (arity)
  (make-instance 'knowledge :set (make-array arity :initial-element nil)))

(defmethod print-object ((object knowledge) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (set) object
      (print set stream))))

(defun known-type (knowledge index)
  "What is the known type of the INDEX'th argument?"
  (check-type knowledge knowledge)
  (with-slots (set) knowledge
    (declare (type simple-vector set))
    (assert (and (<= 0 index) (< index (length set))))
    (elt set index)))

(defun knownp (knowledge index)
  (not (null (known-type knowledge index))))

(defun append-knowledge (knowledge type index)
  (assert (not (knownp knowledge index)))
  (let* ((new-set (copy-seq (slot-value knowledge 'set))))
    (setf (elt new-set index) type)
    (make-instance 'knowledge :set new-set)))


;;;; Specializations

(defclass set ()
  ((specializations :initarg :specializations)
   (arity :initarg :arity)))

(defmethod print-object ((object set) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let* ((specializations (set-specializations object))
           (length (length specializations)))
      (if (= 1 length)
          (format stream "~W" (first specializations))
          (format stream "~d specializations" length)))))

(defun make-set (specializations)
  (check-type specializations sequence)
  (assert (every #'listp specializations))
  (cond ((alexandria:emptyp specializations)
         (make-instance 'set :specializations nil :arity 0))
        (t
         (let* ((arity (length (elt specializations 0))))
           (make-instance 'set
                          :specializations (map 'list #'identity specializations)
                          :arity arity)))))

(defun set-specializations (set)
  (slot-value set 'specializations))

(defun set-arity (set)
  (slot-value set 'arity))

(defun set-count (set)
  (length (set-specializations set)))


;;;; Fixed Arity Rule
(defstruct (fixed-arity-rule (:constructor make-fixed-arity-rule (type index)))
  type
  index)

(defmethod print-object ((object fixed-arity-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (type index) object
      (format stream "~W ~W" index type))))


;;;; Building

(defun strict-subtype-p (a b)
  (and (subtypep a b)
       (not (alexandria:type= a b))))

(defun fixed-arity-rules-for-index (set index knowledge)
  (let* ((types (loop
                   for specialization in (set-specializations set)
                   collect (elt specialization index)))
         (filtered-types (loop
                            for type in types
                            for remove? = (or (knownp knowledge index)
                                              (loop
                                                 for other-type in types
                                                 thereis (strict-subtype-p other-type type)))
                            unless remove?
                            collect type))
         (no-duplicates (remove-duplicates filtered-types :test #'alexandria:type=)))
    (loop
       for type in no-duplicates
       collect (make-fixed-arity-rule type index))))

(defun fixed-arity-rules (set knowledge)
  (loop
     for index from 0 below (set-arity set)
     append (fixed-arity-rules-for-index set index knowledge)))

(defun split-set (set fixed-arity-rule)
  (loop
     with type = (slot-value fixed-arity-rule 'type)
     with index = (slot-value fixed-arity-rule 'index)
     for specialization in (set-specializations set)
     for specialization-type = (elt specialization index)
     for subtype? = (subtypep type specialization-type)
     for strict-subtype? = (strict-subtype-p type specialization-type)
     when subtype?
     collect specialization into set-X
     when (or (not subtype?) strict-subtype?)
     collect specialization into set-Y
     finally
       (return (list (make-set set-X)
                     (make-set set-Y)))))

(defun select-specialization (specializations)
  (let ((specializations (copy-list specializations)))
    (first (sort specializations
                 #'(lambda (a b)
                     (loop
                        for a-type in a
                        for b-type in b
                        always
                          (subtypep a-type b-type)))))))

(defun build-tree (Z &optional (knowledge (make-knowledge (set-arity Z))))
  (flet ((best-split (rules)
           (loop
              with maximum = most-negative-fixnum
              with rv-rule = nil
              with rv-X = nil
              with rv-Y = nil
              for rule in rules
              for (set-X set-Y) = (split-set Z rule)
              for quality = (- (set-count set-X) (set-count set-Y))
              when (> quality maximum)
              do (setf maximum quality
                       rv-rule rule
                       rv-X set-X
                       rv-Y set-Y)
              finally (return (list rv-rule rv-X rv-Y)))))
    (let* ((rules (fixed-arity-rules Z knowledge)))
      (cond (rules
             (destructuring-bind (rule set-X set-Y) (best-split rules)
               (make-node rule
                          (build-tree set-X (append-knowledge knowledge
                                                              (fixed-arity-rule-type rule)
                                                              (fixed-arity-rule-index rule)))
                          (build-tree set-Y knowledge))))
            (t
             (make-node (if (zerop (set-count Z))
                            Z
                            (make-set (list (select-specialization (set-specializations Z)))))))))))

;;;; Dispatch Tree

(defun fixed-arity-specialization-types (store-parameters specialization-parameters)
  (assert (congruent-parameters-p store-parameters specialization-parameters))
  (append (mapcar #'parameter-type (required-parameters specialization-parameters))
          (loop
            for st-parameter in (keyword-parameters store-parameters)
            for keyword = (parameter-keyword st-parameter)
            for sp-parameter = (find keyword (keyword-parameters specialization-parameters) :key #'parameter-keyword)
            collect (if sp-parameter
                        (parameter-type sp-parameter)
                        t))))

(defun map-to-problem (store-parameters all-specialization-parameters)
  (loop
     for specialization-parameters in all-specialization-parameters
     collect
       (fixed-arity-specialization-types store-parameters specialization-parameters)))

(defun map-from-problem (store-parameters all-specialization-parameters tree)
  (let* ((index-function-table (map 'vector #'identity (append (loop
                                                                  for nil in (required-parameters store-parameters)
                                                                  for index from 0
                                                                  collect (let ((index index))
                                                                            #'(lambda (type)
                                                                                (make-positional-parameter-type-rule index type))))
                                                               (loop
                                                                  for nil in (optional-parameters store-parameters)
                                                                  for index from (length (required-parameters store-parameters))
                                                                  collect (let ((index index))
                                                                            #'(lambda (type)
                                                                                (make-positional-parameter-type-rule index type))))
                                                               (loop
                                                                  for parameter in (keyword-parameters store-parameters)
                                                                  collect (let ((keyword (parameter-keyword parameter)))
                                                                            #'(lambda (type)
                                                                                (make-keyword-parameter-type-rule keyword type))))))))
    (labels ((process (node)
               (cond ((leafp node)
                      (let ((set (node-value node)))
                        (check-type set set)
                        (make-node (ecase (set-count set)
                                     (0 nil)
                                     (1 (loop
                                           with tuple = (first (set-specializations set))
                                           for specialization-parameters in all-specialization-parameters
                                           for specialization-tuple = (fixed-arity-specialization-types store-parameters specialization-parameters)
                                           when (equal tuple specialization-tuple)
                                           return specialization-parameters
                                           finally (error "Unable to find specialization parameters with tuple ~W." tuple)))))))
                     (t
                      (let ((rule (node-value node)))
                        (check-type rule fixed-arity-rule)
                        (make-node (funcall (elt index-function-table (fixed-arity-rule-index rule))
                                            (fixed-arity-rule-type rule))
                                   (process (node-pass node))
                                   (process (node-fail node))))))))
      (process tree))))

(defun make-initial-dispatch-tree (store-parameters all-specialization-parameters)
  (let* ((fixed-arity-specializations (map-to-problem store-parameters all-specialization-parameters))
         (set (make-set fixed-arity-specializations))
         (fixed-arity-tree (build-tree set))
         (tree (map-from-problem store-parameters all-specialization-parameters fixed-arity-tree)))
    (cond ((keyword-parameters-p store-parameters)
           (make-node (make-accepts-argument-count-rule (+ (length (required-parameters store-parameters))
                                                           (length (optional-parameters store-parameters))))
                      tree))
          (t
           (make-node (make-fixed-argument-count-rule (set-arity set))
                      tree)))))
