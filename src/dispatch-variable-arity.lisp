(in-package "SPECIALIZATION-STORE.DISPATCH.VARIABLE-ARITY")

(defun fixed-arity-tuple (specialization-parameters c)
  (let* ((lower-bound (specialization-store.dispatch:specialization-parameters-lower-bound specialization-parameters))
         (upper-bound (specialization-store.dispatch:specialization-parameters-upper-bound specialization-parameters))
         (required-count (length (specialization-store.lambda-lists:required-parameters specialization-parameters))))
    (assert (<= lower-bound c upper-bound))
    (let* ((rest (specialization-store.lambda-lists:rest-parameter specialization-parameters))
           (rv (append (loop
                         for parameter in (specialization-store.lambda-lists:required-parameters specialization-parameters)
                         collect (specialization-store.lambda-lists:parameter-type parameter))
                      (when rest
                        (loop
                           for index from required-count below c
                           collect (specialization-store.lambda-lists:parameter-each-type rest))))))
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
                 (first (sort candidates #'> :key #'specialization-store.dispatch:specialization-parameters-lower-bound))))
             (rewrite-node (node)
               (when node
                 (let* ((value (specialization-store.dispatch:node-value node)))
                   (cond ((typep value 'specialization-store.dispatch.fixed-arity:set)
                          (let* ((tuples (specialization-store.dispatch.fixed-arity:set-specializations value)))
                            (when tuples
                              (specialization-store.dispatch:make-node (select-specialization (first tuples))))))
                         ((typep value 'specialization-store.dispatch.fixed-arity:fixed-arity-rule)
                          (let* ((type (specialization-store.dispatch.fixed-arity:fixed-arity-rule-type value))
                                 (index (specialization-store.dispatch.fixed-arity:fixed-arity-rule-index value)))
                            (specialization-store.dispatch:make-node
                             (specialization-store.dispatch:make-positional-parameter-type-rule index type)
                             (rewrite-node (specialization-store.dispatch:node-pass node))
                             (rewrite-node (specialization-store.dispatch:node-fail node)))))
                         (t
                          (error "Do not know how to process node with value ~W." value)))))))
      (rewrite-node (build-tree)))))

(defun make-final-tree (set-Z c)
  (loop
    for parameters in set-Z
    unless (<= (specialization-store.dispatch:specialization-parameters-lower-bound parameters)
               c)
      do
         (error "Specialization parameters ~A is invalid when invoked with ~d arguments."
                parameters c))
  (let* ((set-Z-tuples (loop
                          for specialization-parameters in set-Z
                          collect (fixed-arity-tuple specialization-parameters c))))
    (labels ((build-tree ()
               (specialization-store.dispatch.fixed-arity:build-tree
                (specialization-store.dispatch.fixed-arity:make-set set-Z-tuples)))
             (select-specializations (tuple)
               (loop
                 for specialization-parameters in set-Z
                 for specialization-tuple in set-Z-tuples
                 when (every #'subtypep tuple specialization-tuple)
                   collect specialization-parameters))
             (make-rest-rule-tree (specialization others)
               (when specialization
                 (let* ((rest (specialization-store.lambda-lists:rest-parameter specialization)))
                   (cond (rest
                          (specialization-store.dispatch:make-node
                           (specialization-store.dispatch:make-rest-objects-rule
                            (specialization-store.lambda-lists:parameter-each-type rest)
                            (specialization-store.dispatch:specialization-parameters-lower-bound specialization))
                           (specialization-store.dispatch:make-node specialization)
                           (make-rest-rule-tree (first others) (rest others))))
                         (t
                          (specialization-store.dispatch:make-node
                           (specialization-store.dispatch:make-fixed-argument-count-rule c)
                           (specialization-store.dispatch:make-node specialization)
                           (make-rest-rule-tree (first others) (rest others))))))))
             (rewrite-node (node)
               (when node
                 (let* ((value (specialization-store.dispatch:node-value node)))
                   (cond ((typep value 'specialization-store.dispatch.fixed-arity:set)
                          (let* ((tuples (specialization-store.dispatch.fixed-arity:set-specializations value))
                                 (specializations (remove-duplicates
                                                   (loop
                                                     for tuple in tuples
                                                     append (select-specializations tuple)))))
                            (when tuples
                              (make-rest-rule-tree (first specializations) (rest specializations)))))
                         ((typep value 'specialization-store.dispatch.fixed-arity:fixed-arity-rule)
                          (let* ((type (specialization-store.dispatch.fixed-arity:fixed-arity-rule-type value))
                                 (index (specialization-store.dispatch.fixed-arity:fixed-arity-rule-index value)))
                            (specialization-store.dispatch:make-node
                             (specialization-store.dispatch:make-positional-parameter-type-rule index type)
                             (rewrite-node (specialization-store.dispatch:node-pass node))
                             (rewrite-node (specialization-store.dispatch:node-fail node)))))
                         (t
                          (error "Do not know how to process node with value ~W." value)))))))
      (rewrite-node (build-tree)))))

(defun compute-c (set-Z previous-c)
  (loop
     with min-lower-bound-count = 0
     with min-lower-bound = most-positive-fixnum
     for specialization-parameters in set-Z
     for lower-bound = (specialization-store.dispatch:specialization-parameters-lower-bound specialization-parameters)
     when (> lower-bound previous-c)
       do
          (incf min-lower-bound-count)
          (setf min-lower-bound (min min-lower-bound lower-bound))
     finally (return (if (zerop min-lower-bound-count)
                         previous-c
                         min-lower-bound))))

(defun split-set-Z (set-Z c)
  (loop
     for specialization-parameters in set-Z
     for lower-bound = (specialization-store.dispatch:specialization-parameters-lower-bound specialization-parameters)
     for upper-bound = (specialization-store.dispatch:specialization-parameters-upper-bound specialization-parameters)
     when (<= lower-bound c upper-bound)
     collect specialization-parameters into set-X
     when (> upper-bound c)
     collect specialization-parameters into set-Y
     finally (return (list set-X set-Y))))

(defun make-tree-for-set-Z (set-Z &optional (previous-c -1))
  (when set-Z
    (let* ((c (compute-c set-Z previous-c)))
      (destructuring-bind (set-X set-Y) (split-set-Z set-Z c)
        (let* ((next-c (compute-c set-Y c)))
          (cond ((= c next-c)
                 (make-final-tree set-Z c))
                (t
                 (specialization-store.dispatch:make-node
                  (specialization-store.dispatch:make-accepts-argument-count-rule next-c)
                  (make-tree-for-set-Z set-Y c)
                  (specialization-store.dispatch:make-node
                   (specialization-store.dispatch:make-fixed-argument-count-rule c)
                   (make-tree-for-set-X set-X c))))))))))

(defun make-initial-dispatch-tree (store-parameters all-specialization-parameters)
  (assert (specialization-store.dispatch:variable-arity-store-parameters-p store-parameters))
  (assert (loop
             for specialization-parameters in all-specialization-parameters
             always (specialization-store.lambda-lists:congruent-parameters-p store-parameters specialization-parameters)))
  (make-tree-for-set-Z all-specialization-parameters))
