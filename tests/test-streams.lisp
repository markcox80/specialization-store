(in-package "SPECIALIZATION-STORE.TESTS")

(defparameter *syntax-layer-tests-pathname* (make-pathname :name "standard-store-syntax-layer"
                                                           :type "lisp"
                                                           :defaults #. (or *compile-file-pathname*
                                                                            *load-pathname*)))

(defparameter *glue-layer-tests-pathname* (make-pathname :name "standard-store-glue-layer"
                                                         :type "lisp"
                                                         :defaults #. (or *compile-file-pathname*
                                                                          *load-pathname*)))

(defmacro syntax-layer-test (name &body body)
  (declare (ignore name))
  `(progn
     ,@body))

(defmacro glue-layer-test (name &body body)
  (declare (ignore name))
  `(progn
     ,@body))

(defun process-test-stream (stream)
  (let ((temp-package-name "SPECIALIZATION-STORE.TESTS.TEST-STREAM"))
    (when (find-package temp-package-name)
      (delete-package temp-package-name))
    (let ((*package* (make-package temp-package-name
                                   :use '("COMMON-LISP" "SPECIALIZATION-STORE" "FIVEAM")))
          (*print-case* :downcase)
          (suite (make-suite 'test-stream-test)))
      (in-suite test-stream-test)
      (import 'syntax-layer-test *package*)
      (import 'glue-layer-test *package*)
      (import 'stop *package*)
      (let ((form (read stream nil nil)))
        (cond
          ((null form)
           nil)
          ((and (symbolp form) (eql form 'stop))
           nil)
          ((and (listp form)
                (member (first form) '(glue-layer-test syntax-layer-test)))
           (format t "~%;;;; Preparing global environment for ~W ~W.~%" (second form) (first form))
           (uiop:with-temporary-file (:stream s :pathname p)
             (loop
                for expression in (cddr form)
                do
                  (pprint expression s))
             (close s)
             (load (compile-file p)))
           (let* ((fiveam:*test-dribble* nil)
                  (report (run suite)))
             (format t "~&;; Test ~A.~%"
                     (if (fiveam:results-status report)
                         "passed"
                         "failed"))
             (fiveam:explain! report))
           t)
          (t
           t))))))

(defun process-test-streams ()
  (with-open-file (in/syntax *syntax-layer-tests-pathname*)
    (with-open-file (in/glue *glue-layer-tests-pathname*)
      (with-open-stream (in (make-concatenated-stream in/glue in/syntax))
        (loop while (process-test-stream in))))))
