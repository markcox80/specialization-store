(in-package "SPECIALIZATION-STORE")

(defmacro compiler-macro-lambda (lambda-list &body body)
  (introspect-environment:parse-compiler-macro (gensym "COMPILER-MACRO-LAMBDA")
					       lambda-list
					       body))

(defun the-form-p (form)
  (and (listp form)
       (eql 'the (first form))
       (= 3 (length form))))

(defun determine-form-type (form env)
  (let ((form (macroexpand form env)))
    (cond
      ((constantp form env)
       (type-of (introspect-environment:constant-form-value form env)))
      ((the-form-p form)
       (second form))
      ((symbolp form)
       (or (introspect-environment:variable-type form env)
	   t))
      ((and form (listp form))
       (or (car (last (introspect-environment:function-type (first form) env)))
	   t))
      (t
       (error "Do not know how to process form.")))))
