(in-package "SPECIALIZATION-STORE.DISPATCH.VARIABLE-ARITY")

(defun fixed-arity-tuple (specialization-parameters c)
  (let* ((lower-bound (specialization-parameters-lower-bound specialization-parameters))
         (upper-bound (specialization-parameters-upper-bound specialization-parameters))
         (required-count (length (required-parameters specialization-parameters)))
         (optional-count (length (optional-parameters specialization-parameters))))
    (assert (<= lower-bound c upper-bound))
    (let ((rv (append (loop
                         for (nil type) in (required-parameters specialization-parameters)
                         collect type)
                      (loop
                         for nil in (optional-parameters specialization-parameters)
                         for index from required-count below c
                         collect t)
                      (when (rest-parameter specialization-parameters)
                        (loop
                           for index from (+ required-count optional-count) below c
                           collect t)))))
      (assert (= c (length rv)))
      rv)))

(defun make-tree-for-set-X (set-X c)
  (let* ((set-X-tuples (loop
                          for specialization-parameters in set-X                          
                          collect (fixed-arity-tuple specialization-parameters c))))
    (labels ((build-tree ()
               (specialization-store.dispatch.fixed-arity:build-tree
                (specialization-store.dispatch.fixed-arity:make-set set-X-tuples)))
             (select-specialization (tuple)
               (let* ((candidates (loop
                                     for specialization-parameters in set-X
                                     for specialization-tuple in set-X-tuples
                                     when (equal tuple specialization-tuple)
                                     collect specialization-parameters)))
                 (first (sort candidates #'> :key #'specialization-parameters-lower-bound))))
             (rewrite-node (node)
               (when node
                 (let* ((value (node-value node)))
                   (cond ((typep value 'specialization-store.dispatch.fixed-arity:set)
                          (let* ((tuples (specialization-store.dispatch.fixed-arity:set-specializations value))
                                 (tuple (first tuples)))
                            (ecase (length tuples)
                              (0 nil)
                              (1 (make-node (select-specialization tuple))))))
                         ((typep value 'specialization-store.dispatch.fixed-arity:fixed-arity-rule)
                          (let* ((type (specialization-store.dispatch.fixed-arity:fixed-arity-rule-type value))
                                 (index (specialization-store.dispatch.fixed-arity:fixed-arity-rule-index value)))
                            (make-node (make-positional-parameter-type-rule index type)
                                       (rewrite-node (node-left node))
                                       (rewrite-node (node-right node)))))
                         (t
                          (error "Do not know how to process node with value ~W." value)))))))
      (rewrite-node (build-tree)))))

(defun compute-c (set-Z previous-c)
  (loop
     with min-lower-bound-count = 0
     with min-lower-bound = most-positive-fixnum
     for specialization-parameters in set-Z
     for lower-bound = (specialization-parameters-lower-bound specialization-parameters)
     for upper-bound = (specialization-parameters-upper-bound specialization-parameters)
     when (> lower-bound previous-c)
     do
       (incf min-lower-bound-count)
       (setf min-lower-bound (min min-lower-bound lower-bound))
     minimizing upper-bound into min-upper-bound
     finally (return (if (zerop min-lower-bound-count)
                         previous-c
                         (min min-lower-bound min-upper-bound)))))

(defun split-set-Z (set-Z c)
  (loop
     for specialization-parameters in set-Z
     for lower-bound = (specialization-parameters-lower-bound specialization-parameters)
     for upper-bound = (specialization-parameters-upper-bound specialization-parameters)
     when (<= lower-bound c upper-bound)
     collect specialization-parameters into set-X
     when (> upper-bound c)
     collect specialization-parameters into set-Y
     finally (return (list set-X set-Y))))

(defun make-tree-for-set-Z (set-Z &optional (previous-c -1))
  (when set-Z
    (let* ((c (compute-c set-Z previous-c)))
      (if (> c previous-c)
          (destructuring-bind (set-X set-Y) (split-set-Z set-Z c)
            (make-node (make-accepts-argument-count-rule (1+ c))
                       (make-tree-for-set-Z set-Y c)
                       (make-tree-for-set-X set-X c)))
          (make-tree-for-set-X set-Z c)))))

(defun make-initial-dispatch-tree (store-parameters all-specialization-parameters)
  (assert (variable-arity-store-parameters-p store-parameters))
  (assert (loop
             for specialization-parameters in all-specialization-parameters
             always (congruent-parameters-p store-parameters specialization-parameters)))
  (make-tree-for-set-Z all-specialization-parameters))
