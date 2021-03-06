(in-package "SPECIALIZATION-STORE.DISPATCH")


;;;; Binary Decision Tree

(defstruct (node (:constructor %make-node))
  (value nil)
  (pass nil)
  (fail nil))

(defun make-node (&optional value pass fail)
  (check-type pass (or null node))
  (check-type fail (or null node))
  (%make-node :pass pass :fail fail :value value))

(defun leafp (node)
  (and (null (node-pass node))
       (null (node-fail node))))

(defun split-leaf (node splitting-function)
  (assert (leafp node))
  (let ((split? (funcall splitting-function (node-value node))))
    (cond (split? (destructuring-bind (new-value pass-value fail-value) split?
                    (make-node new-value
                               (make-node pass-value)
                               (make-node fail-value))))
          (t node))))

(defun deepen-tree (node test-function splitting-function)
  ;; Implement this the easy way. The trees will be short.
  (labels ((process (node)
             (cond ((leafp node)
                    (if (funcall test-function (node-value node))
                        (values (split-leaf node splitting-function) t)
                        (values node nil)))
                   (t
                    (multiple-value-bind (pass-node pass?) (process (node-pass node))
                      (multiple-value-bind (fail-node fail?) (process (node-fail node))
                        (values (make-node (node-value node) pass-node fail-node)
                                (or pass? fail?))))))))
    (process node)))

;;;; Rules
;;
;; A rule is something that must be satisfied in order for a
;; specialization to be invoked.
;;
;; A dispatch rule is a rule that can be used to discriminate between
;; two or more specializations.

(defgeneric rule-equal (rule-a rule-b))
(defgeneric remove-rule-tautologies (rule known-rule))
(defgeneric negate-rule-if-possible (rule))

(defclass rule ()
  ())

(defclass dispatch-rule (rule)
  ())

;;;; Default negate-rule
(defmethod negate-rule-if-possible ((rule rule))
  nil)

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

;;;; Consumes count arguments rule
(defclass accepts-argument-count-rule (dispatch-rule)
  ((count :initarg :count
          :reader argument-count))
  (:documentation "Accepts count or more arguments?"))

(defmethod print-object ((object accepts-argument-count-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~d" (argument-count object))))

(defmethod negate-rule-if-possible ((rule accepts-argument-count-rule))
  (when (plusp (argument-count rule))
    (make-instance 'argument-count-less-than-rule
                   :count (argument-count rule))))

;;;; Consumes less than count arguments rule
(defclass argument-count-less-than-rule (dispatch-rule)
  ((count :initarg :count
          :reader argument-count))
  (:documentation "The arity of the specialization is less than count."))

(defmethod print-object ((object argument-count-less-than-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~d" (argument-count object))))

(defmethod negate-rule-if-possible ((rule argument-count-less-than-rule))
  (make-instance 'accepts-argument-count-rule
                 :count (argument-count rule)))

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

;;;; Consantly rule
(defgeneric constantly-rule-value (dispatch-rule))

(defclass constantly-rule (dispatch-rule)
  ((value :initarg :value
          :reader constantly-rule-value)))

(defmethod print-object ((object constantly-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (value) object
      (write value :stream stream))))

;;;; Rest objects rule
(defgeneric rest-objects-rule-type (dispatch-rule))
(defgeneric rest-objects-rule-position (dispatch-rule))

(defclass rest-objects-rule (dispatch-rule)
  ((type :initarg :type
         :reader rest-objects-rule-type)
   (position :initarg :position
             :reader rest-objects-rule-position)))

(defmethod print-object ((object rest-objects-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (rest-objects-rule-type object) :stream stream)))

;;;; Constructors
(defun make-fixed-argument-count-rule (count)
  (check-type count lambda-parameter-count)
  (make-instance 'fixed-argument-count-rule :count count))

(defun make-accepts-argument-count-rule (count)
  (check-type count lambda-parameter-count)
  (make-instance 'accepts-argument-count-rule :count count))

(defun make-positional-parameter-type-rule (position type)
  (make-instance 'positional-parameter-type-rule :position position :type type))

(defun make-keyword-parameter-type-rule (keyword type)
  (make-instance 'keyword-parameter-type-rule :keyword keyword :type type))

(defun make-constantly-rule (value)
  (make-instance 'constantly-rule :value value))

(defun make-rest-objects-rule (type starting-at)
  (cond ((alexandria:type= type t)
         (make-constantly-rule t))
        ((alexandria:type= type nil)
         (make-constantly-rule nil))
        (t
         (make-instance 'rest-objects-rule :type type :position starting-at))))

;;;; Functions

(defun other-keys-p (store-parameters specialization-parameters)
  (and (specialization-store.lambda-lists:keyword-parameters-p store-parameters)
       (specialization-store.lambda-lists:keyword-parameters-p specialization-parameters)
       (> (length (specialization-store.lambda-lists:keyword-parameters specialization-parameters))
          (length (specialization-store.lambda-lists:keyword-parameters store-parameters)))))

(defun specialization-parameters-lower-bound (specialization-parameters)
  (length (specialization-store.lambda-lists:required-parameters specialization-parameters)))

(defun specialization-parameters-upper-bound (specialization-parameters)
  (cond
    ((or (specialization-store.lambda-lists:rest-parameter specialization-parameters)
         (specialization-store.lambda-lists:keyword-parameters-p specialization-parameters))
     lambda-parameters-limit)
    (t
     (length (specialization-store.lambda-lists:required-parameters specialization-parameters)))))

;;;; Training
(defun fixed-arity-store-parameters-p (store-parameters)
  (or (specialization-store.lambda-lists:keyword-parameters-p store-parameters)
      (null (specialization-store.lambda-lists:rest-parameter store-parameters))))

(defun variable-arity-store-parameters-p (store-parameters)
  (and (specialization-store.lambda-lists:rest-parameter store-parameters)
       (not (specialization-store.lambda-lists:keyword-parameters-p store-parameters))))

(defun make-initial-dispatch-tree (store-parameters all-specialization-parameters)
  (cond ((null all-specialization-parameters)
         (make-node (make-constantly-rule nil)))
        ((fixed-arity-store-parameters-p store-parameters)
         (specialization-store.dispatch.fixed-arity:make-initial-dispatch-tree store-parameters all-specialization-parameters))
        ((variable-arity-store-parameters-p store-parameters)
         (specialization-store.dispatch.variable-arity:make-initial-dispatch-tree store-parameters all-specialization-parameters))
        (t
         (error "Should not get here."))))

(defmethod remove-rule-tautologies ((rule t) (known-rules list))
  (labels ((process (current-rule changed? known-rules)
             (cond (known-rules
                    (multiple-value-bind (new-rule current-rule-changed?)
                        (remove-rule-tautologies current-rule (first known-rules))
                      (process new-rule (or changed? current-rule-changed?)
                               (rest known-rules))))
                   (t
                    (values current-rule changed?)))))
    (process rule nil known-rules)))

(defmethod remove-rule-tautologies ((rule t) (known-rule t))
  (if (rule-equal rule known-rule)
      (values (make-constantly-rule t)
              t)
      (values rule
              nil)))

(defun remove-dispatch-tree-tautologies (tree &optional initial-knowledge)
  (labels ((node-equal (a b)
             (cond ((and (null a) (null b))
                    t)
                   ((or (null a) (null b))
                    nil)
                   ((and (leafp a) (leafp b))
                    (eql (node-value a) (node-value b)))
                   ((and (not (leafp a)) (not (leafp b)))
                    (and (rule-equal (node-value a) (node-value b))
                         (node-equal (node-pass a) (node-pass b))
                         (node-equal (node-fail a) (node-fail b))))
                   (t
                    nil)))
           (process (node knowledge)
             (cond ((null node)
                    nil)
                   ((leafp node)
                    node)
                   (t
                    (let ((rule (node-value node)))
                      (cond ((node-equal (node-pass node) (node-fail node))
                             (values (node-pass node) t))
                            ((typep rule 'constantly-rule)
                             (values (if (constantly-rule-value rule)
                                         (node-pass node)
                                         (node-fail node))
                                     t))
                            (t
                             (multiple-value-bind (new-rule changed?) (remove-rule-tautologies rule knowledge)
                               (let ((pass-knowledge (cons new-rule knowledge))
                                     (fail-knowledge (let ((negated-rule (negate-rule-if-possible new-rule)))
                                                       (if negated-rule
                                                           (cons (remove-rule-tautologies negated-rule knowledge)
                                                                 knowledge)
                                                           knowledge))))
                                 (multiple-value-bind (new-pass pass-changed?) (process (node-pass node) pass-knowledge)
                                   (multiple-value-bind (new-fail fail-changed?) (process (node-fail node) fail-knowledge)
                                     (values (make-node new-rule new-pass new-fail)
                                             (or changed? pass-changed? fail-changed?)))))))))))))
    (multiple-value-bind (new-tree changed?) (process tree initial-knowledge)
      (if changed?
          (remove-dispatch-tree-tautologies new-tree)
          new-tree))))

(defun make-dispatch-tree (store-parameters all-specialization-parameters)
  (remove-dispatch-tree-tautologies
   (make-initial-dispatch-tree store-parameters all-specialization-parameters)
   (list (make-accepts-argument-count-rule (specialization-parameters-lower-bound store-parameters)))))

(defun pretty-print-dispatch-tree (tree &optional (stream *standard-output*))
  (cond ((null tree)
         (princ nil stream))
        ((leafp tree)
         (princ (node-value tree) stream))
        (t
         (pprint-logical-block (stream nil :prefix "(if " :suffix ")")
           (princ (node-value tree) stream)
           (terpri stream)
           (pprint-logical-block (stream nil :per-line-prefix "   ")
             (pretty-print-dispatch-tree (node-pass tree) stream)
             (terpri stream)
             (pretty-print-dispatch-tree (node-fail tree) stream))))))

;;;; Rule Implementation
(defun compare-slot-values (slot-name test-fn &rest objects)
  (apply test-fn (mapcar #'(lambda (object)
                             (slot-value object slot-name))
                         objects)))

(defmethod rule-equal ((rule-a t) (rule-b t))
  nil)

(defmethod rule-equal ((rule-a fixed-argument-count-rule) (rule-b fixed-argument-count-rule))
  (= (argument-count rule-a)
     (argument-count rule-b)))

(defmethod rule-equal ((rule-a accepts-argument-count-rule) (rule-b accepts-argument-count-rule))
  (= (argument-count rule-a)
     (argument-count rule-b)))

(defmethod rule-equal ((rule-a argument-count-less-than-rule) (rule-b argument-count-less-than-rule))
  (= (argument-count rule-a)
     (argument-count rule-b)))

(defmethod rule-equal ((rule-a positional-parameter-type-rule) (rule-b positional-parameter-type-rule))
  (and (compare-slot-values 'position #'= rule-a rule-b)
       (compare-slot-values 'type #'alexandria:type= rule-a rule-b)))

(defmethod rule-equal ((rule-a keyword-parameter-type-rule) (rule-b keyword-parameter-type-rule))
  (and (compare-slot-values 'keyword #'eql rule-a rule-b)
       (compare-slot-values 'type #'alexandria:type= rule-a rule-b)))

(defmethod rule-equal ((rule-a rest-objects-rule) (rule-b rest-objects-rule))
  (alexandria:type= (rest-objects-rule-type rule-a)
                    (rest-objects-rule-type rule-b)))

(defmethod remove-rule-tautologies ((rule positional-parameter-type-rule) (known-rule fixed-argument-count-rule))
  (let* ((count (argument-count known-rule))
         (applicable? (< (parameter-position rule) count)))
    (cond ((and applicable? (alexandria:type= (parameter-type rule) t))
           (values (make-constantly-rule t)
                   t))
          ((not applicable?)
           (values (make-constantly-rule nil)
                   t))
          (t
           (values rule
                   nil)))))

(defmethod remove-rule-tautologies ((rule positional-parameter-type-rule) (known-rule accepts-argument-count-rule))
  (let* ((count (argument-count known-rule))
         (guaranteed? (< (parameter-position rule) count)))
    (cond ((and guaranteed? (alexandria:type= (parameter-type rule) t))
           (values (make-constantly-rule t)
                   t))
          (t
           (values rule nil)))))

(defmethod remove-rule-tautologies ((rule keyword-parameter-type-rule) known-rule)
  (declare (ignore known-rule))
  (if (alexandria:type= t (parameter-type rule))
      (values (make-constantly-rule t) t)
      (values rule nil)))

(defmethod remove-rule-tautologies ((rule fixed-argument-count-rule) (known-rule accepts-argument-count-rule))
  (let* ((count (argument-count rule))
         (known-count (argument-count known-rule)))
    (cond ((>= count known-count)
           (values rule nil))
          (t
           (values (make-constantly-rule nil)
                   t)))))

(defmethod remove-rule-tautologies ((rule fixed-argument-count-rule) (known-rule argument-count-less-than-rule))
  (let* ((count (argument-count rule))
         (known-count (argument-count known-rule)))
    (cond ((>= count known-count)
           (values (make-constantly-rule nil)
                   t))
          (t
           (values rule nil)))))

(defmethod remove-rule-tautologies ((rule argument-count-less-than-rule) (known-rule accepts-argument-count-rule))
  (let* ((count (argument-count rule))
         (known-count (argument-count known-rule)))
    (if (= (1+ known-count) count)
        (values (make-fixed-argument-count-rule known-count)
                t)
        (values rule nil))))

(defmethod remove-rule-tautologies ((rule accepts-argument-count-rule) (known-rule argument-count-less-than-rule))
  (let* ((count (argument-count rule))
         (known-count (argument-count known-rule)))
    (if (= (1+ count) known-count)
        (values (make-fixed-argument-count-rule count)
                t)
        (values rule nil))))
