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

(defun output-test (form stream)
  (let* ((*package* (find-package "SPECIALIZATION-STORE.TESTS.TEST-STREAM"))
         (*print-case* :downcase))
    (format stream "~&;;;; SANDBOX PREAMBLE~%")
    (dolist (preamble-form `((defpackage "SPECIALIZATION-STORE.TESTS.TEST-STREAM"
                               (:use "COMMON-LISP"
                                     "SPECIALIZATION-STORE"
                                     "FIVEAM")
                               (:import-from "SPECIALIZATION-STORE.TESTS"
                                             "SYNTAX-LAYER-TEST"
                                             "GLUE-LAYER-TEST"
                                             "STOP"))
                             (in-package "SPECIALIZATION-STORE.TESTS.TEST-STREAM")))
      (pprint preamble-form stream))
    (format stream "~%")
    (format stream "(5am:def-suite test-stream-tests)~%")
    (format stream "(5am:in-suite test-stream-tests)~%")
    (format stream "~&~%~%")
    (format stream "~&;;;; ~A ~A~%" (first form) (second form))
    (loop
      for subform in (subseq form 2)
      do
         (pprint subform stream)
         (terpri stream))
    (terpri stream))
  (values))

(defun evaluate-test (form &key pathname (if-exists :error))
  (let* ((test-type (string-downcase (string (first form))))
         (test-name (string-downcase (string (second form)))))
    (format t "~%;;;; Evaluating test: ~A ~A.~%" test-type test-name)
    ;; Compile and load the test.
    (flet ((compile-and-load (pathname)
             (uiop:with-temporary-file (:stream s :pathname fasl-path :type (pathname-type (compile-file-pathname pathname)))
               (close s)
               (ignore-errors (delete-package "SPECIALIZATION-STORE.TESTS.TEST-STREAM"))
               (multiple-value-bind (garbage warningsp failuresp)
                   (with-compilation-unit (:override t)
                     (compile-file pathname :output-file fasl-path))
                 (declare (ignore garbage))
                 (ignore-errors (delete-package "SPECIALIZATION-STORE.TESTS.TEST-STREAM"))
                 (cond ((or warningsp failuresp)
                        (error "Unable to compile test ~A ~A." test-type test-name))
                       ((not (load fasl-path))
                        (error "Unable to load test ~A ~A." test-type test-name)))))))
      (cond (pathname
             (with-open-file (out pathname :if-exists if-exists :direction :output)
               (output-test form out))
             (compile-and-load pathname))
            (t
             (uiop:with-temporary-file (:stream s :pathname p :direction :output)
               (output-test form s)
               (close s)
               (compile-and-load p))))))
  ;; Run the test.
  (let* ((5am:*test-dribble* nil)
         (report (5am:run (intern "TEST-STREAM-TESTS" "SPECIALIZATION-STORE.TESTS.TEST-STREAM")))
         (succeededp (5am:results-status report)))
    (format t "~&;; Sandbox test ~A.~%" (if succeededp "passed" "failed"))
    (unless succeededp
      (setf 5am:*test-dribble* t)
      (5am:explain! report))
    succeededp))

(defun process-test-stream (stream)
  (ignore-errors (delete-package "SPECIALIZATION-STORE.TESTS.TEST-STREAM"))
  (let* ((form (let* ((*package* (make-package "SPECIALIZATION-STORE.TESTS.TEST-STREAM"
                                                   :use '("COMMON-LISP" "SPECIALIZATION-STORE" "FIVEAM"))))
                 (import 'glue-layer-test)
                 (import 'syntax-layer-test)
                 (import 'stop)
                 (read stream nil nil))))
    (cond
      ((null form)
       nil)
      ((and (symbolp form) (eql form 'stop))
       nil)
      ((and (listp form)
            (member (first form) '(glue-layer-test syntax-layer-test)))
       (values t (evaluate-test form :pathname "/tmp/example.lisp" :if-exists :supersede)))
      (t
       (values t t)))))

(defun process-test-streams ()
  (with-open-file (in/syntax *syntax-layer-tests-pathname*)
    (with-open-file (in/glue *glue-layer-tests-pathname*)
      (with-open-stream (in (make-concatenated-stream in/glue in/syntax))
        (loop
          with all-pass = t
          for (continue? pass?) = (multiple-value-list (process-test-stream in))
          while continue?
          do
             (unless pass?
               (setf all-pass nil))
          finally
             (if all-pass
                 (format t "~&;; All sandbox tests passed.~%")
                 (format t "~&;; Some sandbox tests have failed.~%")))))))
