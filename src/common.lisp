(in-package "SPECIALIZATION-STORE")

(defmacro compiler-macro-lambda (lambda-list &body body)
  (introspect-environment:parse-compiler-macro (gensym "COMPILER-MACRO-LAMBDA")
					       lambda-list
					       body))

(defun the-form-p (form)
  (and (listp form)
       (eql 'the (first form))
       (= 3 (length form))))

(defun determine-form-multiple-value-type (form env)
  (let ((form (macroexpand form env)))
    (cond
      ((constantp form env)
       (type-of (introspect-environment:constant-form-value form env)))
      ((the-form-p form)
       (second form))
      ((symbolp form)
       (or (introspect-environment:variable-type form env)
	   *))
      ((and form (listp form))
       (or (car (last (introspect-environment:function-type (first form) env)))
	   '*))
      (t
       (error "Do not know how to process form.")))))

(defun determine-form-value-type (form env &key multiple)
  (let ((v (determine-form-multiple-value-type form env)))
    (cond (multiple v)
          (t (cond ((eql v '*)
                    t)
                   ((and (listp v) (eql 'values (first v)))
                    (let ((value (second v)))
                      (or value 'null)))
                   (t
                    v))))))
