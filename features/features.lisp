(in-package "SPECIALIZATION-STORE.FEATURES")


;;;; Function Information

(declaim (ftype (function (integer) (values integer integer)) test-global-function))
(defun test-global-function (a)
  (values (+ a 1) (+ a -1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun function-information (name env)
    (and (fboundp 'introspect-environment:function-information)
         (funcall 'introspect-environment:function-information name env))))

(defun feature/function-information ()
  (flet ((test-local-function (a)
           (values (+ a -1) (+ a 1)))
         (expected-form-p (form)
           (and (listp form)
                (eql 'function (first form))
                (equal '(integer) (second form))
                (listp (third form))
                (destructuring-bind (name first second &rest others) (third form)
                  (declare (ignore others))
                  (and (eql 'values name)
                       (eql 'integer first)
                       (eql 'integer second))))))
    (declare (ftype (function (integer) (values integer integer)) test-local-function)
             (ignorable (function test-local-function)))
    (macrolet ((compute (name &environment env)
                 (multiple-value-bind (binding-type local? declarations)
                     (function-information name env)
                   (declare (ignore local?))
                   (when (eql :function binding-type)
                     `(quote ,(cdr (assoc 'ftype declarations)))))))
      (let* ((global? (expected-form-p (compute test-global-function)))
             (local? (expected-form-p (compute test-local-function))))
        (when (and global? local?)
          (pushnew 'function-declarations *features*))))))

(feature/function-information)


;;;; Variable Types

(defun feature/variable-type ()
  (macrolet ((compute (name &environment env)
               `(quote ,(introspect-environment:variable-type name env))))
    (let ((a 0))
      (declare (type (integer 0) a)
               (ignorable a))
      (when (alexandria:type= (print (compute a)) '(integer 0))
        (pushnew 'variable-types *features*)))))

(feature/variable-type)
