(defpackage "TUT3"
  (:use "COMMON-LISP"
        "SPECIALIZATION-STORE"))
(in-package "TUT3")

(defstore example (object))

;;;; Inlining

(defspecialization (example :inline t) ((a float)) t
  (1+ a))

(defun test-inlining ()
  (let* ((fn (compiler-macro-function 'example)))
    (funcall fn '(example (the float x)) nil)))


;;;; Named specializations

(defspecialization (example :name %example/string) ((a string)) t
  (concatenate 'string "Info: " a))

(defun test-name ()
  (let* ((fn (compiler-macro-function 'example)))
    (funcall fn '(example (the string x)) nil)))

;;;; Expand functions

(define-specialization example ((int integer)) t
  (:function (lambda (int)
               (format t "~&Runtime: ~A.~%" int)))
  (:expand-function (compiler-macro-lambda (int)
                      `(format t "~&Compile time: ~A -> ~A.~%" ',int ,int))))

(defun test-expand-function ()
  (let ((x 0))
    (example x))

  (let ((fn (compile nil '(lambda (x)
                            (example (the integer x))))))
    (funcall fn 1))
  (values))
