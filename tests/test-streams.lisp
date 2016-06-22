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

(defun process-syntax-layer-test-form (form)
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
          (format t "~&;;;;            ~W~%" c))))))

(defun process-glue-layer-test-form (form)
  (let ((suite (make-suite 'glue-layer-test))
        (name (second form)))
    (in-suite glue-layer-test)
    (handler-case (progn
                    (eval form)
                    (let ((report (let ((*test-dribble* nil))
                                    (run suite))))
                      (cond
                        ((fiveam:results-status report)
                         (format t "~&Glue layer test ~W passed.~%" name))
                        (t
                         (format t "~&Glue layer test ~W failed.~%" name)
                         (fiveam:explain! report)))))
      (error (c)
        (format t "~&;;;; Unhandled error occurred in glue layer test ~W.~%" name)
        (format t "~&;;;; Condition: ~W~%" c)
        (let ((*print-escape* nil))
          (format t "~&;;;;            ~W~%" c))))))

(defun process-test-stream (stream)
  (let ((temp-package-name "SPECIALIZATION-STORE.TESTS.TEST-STREAM"))
    (when (find-package temp-package-name)
      (delete-package temp-package-name))
    (let ((*package* (make-package temp-package-name
                                   :use '("COMMON-LISP" "SPECIALIZATION-STORE" "FIVEAM"))))
      (import 'syntax-layer-test *package*)
      (import 'glue-layer-test *package*)
      (import 'stop *package*)
      (let ((form (read stream nil nil)))
        (cond
          ((null form)
           nil)
          ((and (listp form)
                (eql 'syntax-layer-test (first form)))
           (process-syntax-layer-test-form form)
           t)
          ((and (listp form)
                (eql 'glue-layer-test (first form)))
           (process-glue-layer-test-form form)
           t)
          ((and (symbolp form) (eql form 'stop))
           nil)
          (t
           t))))))

(defun process-test-streams ()
  (with-open-file (in/syntax *syntax-layer-tests-pathname*)
    (with-open-file (in/glue *glue-layer-tests-pathname*)
      (with-open-stream (in (make-concatenated-stream in/glue in/syntax))
        (loop while (process-test-stream in))))))
