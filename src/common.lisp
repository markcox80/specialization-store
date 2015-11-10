(in-package "SPECIALIZATION-STORE")

(defmacro compiler-macro-lambda (lambda-list &body body)
  (introspect-environment:parse-compiler-macro (gensym "COMPILER-MACRO-LAMBDA")
					       lambda-list
					       body))

