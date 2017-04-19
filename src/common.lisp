(in-package "SPECIALIZATION-STORE")

(defmacro compiler-macro-lambda (lambda-list &body body)
  (introspect-environment:parse-compiler-macro (gensym "COMPILER-MACRO-LAMBDA")
                                               lambda-list
                                               body))

(defun compiler-macro-form-head (form)
  (check-type form list)
  (if (eql 'funcall (first form))
      (subseq form 0 2)
      (subseq form 0 1)))

(defun compiler-macro-form-arguments (form)
  (check-type form list)
  (if (eql 'funcall (first form))
      (subseq form 2)
      (subseq form 1)))

(defun the-form-p (form)
  (and (listp form)
       (eql 'the (first form))
       (= 3 (length form))))

(defun determine-form-multiple-value-type (form env)
  (let ((form (macroexpand form env)))
    (cond
      ((or (numberp form)
           (characterp form))
       `(eql ,form))
      ((constantp form env)
       (type-of (introspect-environment:constant-form-value form env)))
      ((the-form-p form)
       (second form))
      ((symbolp form)
       (or (introspect-environment:variable-type form env)
           '*))
      #+specialization-store.features:function-declarations
      ((and form (listp form))
       (multiple-value-bind (operator local? declarations) (introspect-environment:function-information (first form) env)
         (declare (ignore local?))
         (if (member operator '(:function :special-operator))
             (let ((ftype (cdr (assoc 'ftype declarations))))
               (if (and (listp ftype) (eql 'function (first ftype)) (third ftype))
                   (third ftype)
                   '*))
             '*)))
      #-specialization-store.features:function-declarations
      ((and form (listp form))
       '*)
      (t
       (error "Do not know how to process form.")))))

(defun determine-form-value-type (form env)
  (let ((v (determine-form-multiple-value-type form env)))
    (cond ((eql v '*)
           t)
          ((and (listp v) (eql 'values (first v)))
           (let ((value (second v)))
             (or value 'null)))
          (t
           v))))

(defun generate-symbol (base &optional (suffix nil suffixp))
  (if suffixp
      (gensym (format nil "~A-~A-" (string base) (string suffix)))
      (gensym (string base))))

(defun generate-interned-symbol (base suffix &optional (package *package*))
  (loop
    with prefix = (format nil "~A-~A" (string base) (string suffix))
    for symbol = (gensym prefix)
    do
       (multiple-value-bind (interned-symbol status) (intern (symbol-name symbol) package)
         (unless status
           (return-from generate-function-name interned-symbol)))))
