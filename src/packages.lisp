(defpackage "SPECIALIZATION-STORE"
  (:use "COMMON-LISP"))

(defpackage "SPECIALIZATION-STORE.LAMBDA-LISTS"
  (:use "COMMON-LISP")
  
  ;; Parameters Protocol
  (:export "PARAMETERS"
	   "ORIGINAL-LAMBDA-LIST"
	   "REQUIRED-PARAMETERS"
	   "OPTIONAL-PARAMETERS"
	   "REST-PARAMETER"
	   "KEYWORD-PARAMETERS-P"
	   "ALLOW-OTHER-KEYS-P"
	   "KEYWORD-PARAMETERS"
	   "PARAMETERS-EQUAL")

  ;; Store lambda lists
  (:export "STORE-PARAMETERS"
	   "PARSE-STORE-LAMBDA-LIST"
	   "PARSE-STORE-LAMBDA-LIST-ERROR"))
