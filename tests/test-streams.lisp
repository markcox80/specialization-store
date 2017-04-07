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
             (uiop:with-temporary-file (:pathname p-fasl)
               (loop
                 for expression in (cddr form)
                 do
                    (pprint expression s))
               (close s)
               (with-compilation-unit (:override t)
                 (multiple-value-bind (fasl-pathname warningsp failurep) (compile-file p :output-file p-fasl)
                   (cond ((or warningsp failurep)
                          (format t "~&;; Failed to compile test ~A ~A.~%" (second form) (first form)))
                         (t
                          (load fasl-pathname)
                          (let* ((fiveam:*test-dribble* nil)
                                 (report (run suite))
                                 (succeededp (fiveam:results-status report)))
                            (format t "~&;; Test ~A.~%"
                                    (if succeededp
                                        "passed"
                                        "failed"))
                            (unless succeededp
                              (setf fiveam:*test-dribble* t)
                              (fiveam:explain! report)))))))))
           t)
          (t
           t))))))

(defun process-test-streams ()
  (with-open-file (in/syntax *syntax-layer-tests-pathname*)
    (with-open-file (in/glue *glue-layer-tests-pathname*)
      (with-open-stream (in (make-concatenated-stream in/glue in/syntax))
        (loop while (process-test-stream in))))))
