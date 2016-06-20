(in-package "SPECIALIZATION-STORE.TESTS")

(defparameter *syntax-layer-tests-pathname* (make-pathname :name "standard-store-syntax-layer"
                                                           :type "lisp"
                                                           :defaults #. (or *compile-file-pathname*
                                                                            *load-pathname*)))

(defmacro syntax-layer-test (name &body body)
  (declare (ignore name))
  `(progn
     ,@body))

(defun process-syntax-layer-test-stream (stream)
  (let ((temp-package-name "SPECIALIZATION-STORE.TESTS.SYNTAX-LAYER"))
    (when (find-package temp-package-name)
      (delete-package temp-package-name))
    (let ((*package* (make-package temp-package-name
                                   :use '("COMMON-LISP" "SPECIALIZATION-STORE" "FIVEAM"))))
      (import 'syntax-layer-test *package*)
      (let ((form (read stream nil nil)))
        (cond
          ((null form)
           nil)
          ((and (listp form) (eql 'syntax-layer-test (first form)))
           (let ((suite (make-suite 'syntax-layer-test))
                 (name (second form)))
             (in-suite syntax-layer-test)
             (handler-case (progn
                             (eval form)
                             (let ((report (let ((*test-dribble* nil))
                                             (run suite))))
                               (cond
                                 ((fiveam:results-status report)
                                  (format t "~&Syntax layer test ~W passed.~%" name))
                                 (t
                                  (format t "~&Syntax layer test ~W failed.~%" name)
                                  (fiveam:explain! report)))))
               (error (c)
                 (format t "~&;;;; Unhandled error occurred in syntax layer test ~W.~%" name)
                 (format t "~&;;;; Condition: ~W~%" c)
                 (let ((*print-escape* nil))
                   (format t "~&;;;;            ~W~%" c))))
             (process-syntax-layer-test-stream stream)))
          (t
           (process-syntax-layer-test-stream stream)))))))

(defun process-syntax-layer-tests ()
  (with-open-file (in *syntax-layer-tests-pathname*)
    (process-syntax-layer-test-stream in)))
