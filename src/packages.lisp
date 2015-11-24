(defpackage "SPECIALIZATION-STORE"
  (:use "COMMON-LISP")

  ;; Runtime Protocol (Stores)
  (:export "STORE-ERROR"
	   "NO-STORE-WITH-NAME-ERROR"
           "NO-APPLICABLE-SPECIALIZATION-ERROR"
           "SIGNAL-NO-APPLICABLE-SPECIALIZATION-ERROR"
	   "FUNCALL-STORE"
	   "APPLY-STORE"
	   "EXPAND-STORE"
	   "ADD-SPECIALIZATION"
	   "REMOVE-SPECIALIZATION"
	   "STORE-SPECIALIZATIONS"
	   "STORE-NAME"
	   "STORE-LAMBDA-LIST"
	   "STORE-DOCUMENTATION")

  ;; Runtime Protocol (Specializations)
  (:export "SPECIALIZATION-NAME"
	   "SPECIALIZATION-FUNCTION"
	   "SPECIALIZATION-EXPAND-FUNCTION"
	   "SPECIALIZATION-EQUAL"
	   "SPECIALIZATION-LAMBDA-LIST"
	   "SPECIALIZATION-DOCUMENTATION")

  ;; Glue Layer
  (:export "FIND-STORE"
	   "ENSURE-STORE-ONLY"
	   "ENSURE-STORE"
           "ENSURE-STORE-USING-CLASS"
	   "ENSURE-SPECIALIZATION"
           "ENSURE-SPECIALIZATION-USING-CLASS"
           "MAKE-STORE-UNBOUND"
	   "STORE-SPECIALIZATION-CLASS")

  ;; Syntax Layer
  (:export "DEFSTORE"
	   "DEFSPECIALIZATION"
	   "DEFINE-SPECIALIZATION")
  
  ;; Standard Implementation
  (:export "STANDARD-STORE"
	   "STANDARD-SPECIALIZATION")

  ;; Helpers
  (:export "COMPILER-MACRO-LAMBDA"
	   "DETERMINE-FORM-TYPE"))

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

  ;; Parsing
  (:export "PARSE-LAMBDA-LIST-ERROR"
	   "PARSE-LAMBDA-LIST-ERROR-MESSAGE"
	   "PARSE-LAMBDA-LIST-ERROR-LAMBDA-LIST")

  ;; Store lambda lists
  (:export "STORE-PARAMETERS"
	   "PARSE-STORE-LAMBDA-LIST"
	   "PARSE-STORE-LAMBDA-LIST-ERROR")

  ;; Specialization lambda lists
  (:export "SPECIALIZATION-PARAMETERS"
	   "PARSE-SPECIALIZATION-LAMBDA-LIST"
	   "PARSE-SPECIALIZATION-LAMBDA-LIST-ERROR")

  ;; Congruence
  (:export "CONGRUENCE"
	   "STORE-PARAMETERS"
	   "SPECIALIZATION-PARAMETERS"
	   "CONGRUENT-LAMBDA-LIST-P")

  ;; Conversion
  (:export "ORDINARY-LAMBDA-LIST"
	   "TYPE-DECLARATIONS"
           "REWRITE-INIT-FORMS"
           "MAKE-RUNTIME-COMPLETION-LAMBDA-FORM"
           "MAKE-FORM-TYPE-COMPLETION-LAMBDA-FORM"))

(defpackage "SPECIALIZATION-STORE.DISPATCH"
  (:use "COMMON-LISP"
	"SPECIALIZATION-STORE.LAMBDA-LISTS")
  (:export "MAKE-DISPATCH-TREE"))

(defpackage "SPECIALIZATION-STORE.STANDARD-STORE"
  (:use "COMMON-LISP"
	"SPECIALIZATION-STORE"))

(defpackage "SPECIALIZATION-STORE.GLOBALS")


;;;; Meta Object Requirements

#-(or sbcl ccl cmucl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "No attempt has been made to determine if this implementation supports the Metaobject Protocol."))

(defpackage "SPECIALIZATION-STORE.MOP"
  (:export "FUNCALLABLE-STANDARD-CLASS"
           "SET-FUNCALLABLE-INSTANCE-FUNCTION")
  (:import-from #+sbcl "SB-MOP"
                #+ccl "CCL"
                #+cmucl "PCL"
                "FUNCALLABLE-STANDARD-CLASS"
                "SET-FUNCALLABLE-INSTANCE-FUNCTION"))
