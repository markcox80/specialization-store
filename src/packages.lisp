(defpackage "SPECIALIZATION-STORE"
  (:use "COMMON-LISP")

  ;; Runtime Protocol (Stores)
  (:export "FUNCALL-STORE"
           "APPLY-STORE"
           "EXPAND-STORE"
           "ADD-SPECIALIZATION"
           "REMOVE-SPECIALIZATION"
           "STORE-SPECIALIZATIONS"
           "STORE-LAMBDA-LIST"
           "STORE-DOCUMENTATION"

           ;; Conditions
           "STORE-ERROR"
           "STORE-ERROR-STORE"

           "SIMPLE-STORE-ERROR"
           "SIMPLE-STORE-ERROR-MESSAGE"

           "INAPPLICABLE-ARGUMENTS-ERROR"
           "INAPPLICABLE-ARGUMENTS"

           "INCONGRUENT-SPECIALIZATION-ERROR"
           "INCONGRUENT-SPECIALIZATION")

  ;; Runtime Protocol (Specializations)
  (:export "SPECIALIZATION-FUNCTION"
           "SPECIALIZATION-EXPAND-FUNCTION"
           "SPECIALIZATION-EQUAL"
           "SPECIALIZATION-LAMBDA-LIST"
           "SPECIALIZATION-VALUE-TYPE"
           "SPECIALIZATION-DOCUMENTATION")

  ;; Glue Layer
  (:export "FIND-STORE"
           "INVALID-STORE-NAME-ERROR"
           "INVALID-STORE-NAME"

           "ENSURE-STORE"
           "ENSURE-STORE-USING-OBJECT"
           "ENSURE-STORE-ERROR"
           "INVALID-STORE-LAMBDA-LIST-ERROR"
           "INVALID-STORE-LAMBDA-LIST"
           "INVALID-SPECIALIZATION-CLASS-ERROR"
           "INVALID-SPECIALIZATION-CLASS"
           "INVALID-STORE-CLASS-ERROR"
           "INVALID-STORE-CLASS"

           "ENSURE-SPECIALIZATION"
           "ENSURE-SPECIALIZATION-USING-OBJECT"

           "MAKE-STORE-UNBOUND"

           "SPECIALIZATION-NAME"
           "STORE-NAME"
           "STORE-SPECIALIZATION-CLASS")

  ;; Syntax Layer
  (:export "DEFSTORE"
           "DEFSPECIALIZATION"
           "DEFINE-SPECIALIZATION"
           "DEFSTORE-USING-CLASS"
           "DEFSPECIALIZATION-USING-OBJECT"
           "DEFINE-SPECIALIZATION-USING-OBJECT")

  ;; Standard Implementation
  (:export "STANDARD-STORE"
           "STANDARD-SPECIALIZATION"

           "STANDARD-STORE-CLASS"
           "STANDARD-SPECIALIZATION-CLASS")

  ;; Helpers
  (:export "COMPILER-MACRO-LAMBDA"
           "DETERMINE-FORM-MULTIPLE-VALUE-TYPE"
           "DETERMINE-FORM-VALUE-TYPE"
           "FORM-VALUE-TYPE-P"
           "COMPILER-MACRO-FORM-HEAD"
           "COMPILER-MACRO-FORM-ARGUMENTS"
           "GENERATE-SYMBOL"
           "GENERATE-INTERNED-SYMBOL"))

(defpackage "SPECIALIZATION-STORE.LAMBDA-LISTS"
  (:use "COMMON-LISP")
  (:import-from "SPECIALIZATION-STORE"
                "COMPILER-MACRO-LAMBDA"
                "DETERMINE-FORM-VALUE-TYPE")

  ;; Parameter Protocol
  (:export "PARAMETER"
           "REQUIRED-PARAMETER"
           "OPTIONAL-PARAMETER"
           "KEYWORD-PARAMETER"
           "REST-PARAMETER"

           "MAKE-REQUIRED-PARAMETER"
           "MAKE-OPTIONAL-PARAMETER"
           "MAKE-REST-PARAMETER"
           "MAKE-KEYWORD-PARAMETER"

           "PARAMETERP"
           "REQUIRED-PARAMETER-P"
           "OPTIONAL-PARAMETER-P"
           "KEYWORD-PARAMETER-P"
           "REST-PARAMETER-P"

           "PARAMETER-VAR"
           "PARAMETER-INIT-FORM"
           "PARAMETER-DEPENDENCIES"
           "PARAMETER-VARP"
           "PARAMETER-KEYWORD"
           "PARAMETER-TYPE"
           "PARAMETER-EACH-TYPE"
           "PARAMETER-LAMBDA-LIST-SPECIFICATION")

  ;; Parameters Protocol
  (:export "PARAMETERS"
           "ORIGINAL-LAMBDA-LIST"
           "REQUIRED-PARAMETERS"
           "OPTIONAL-PARAMETERS-P"
           "OPTIONAL-PARAMETERS"
           "REST-PARAMETER-P"
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
           "PARSE-STORE-LAMBDA-LIST-ERROR"

           "PARSE-STORE-OBJECT-LAMBDA-LIST")

  ;; Specialization lambda lists
  (:export "SPECIALIZATION-PARAMETERS"
           "PARSE-SPECIALIZATION-LAMBDA-LIST"
           "PARSE-SPECIALIZATION-LAMBDA-LIST-ERROR")

  ;; Congruence
  (:export "CONGRUENCE"
           "STORE-PARAMETERS"
           "SPECIALIZATION-PARAMETERS"
           "CONGRUENT-PARAMETERS-P")

  ;; Conversion
  (:export "PARAMETER-INIT-FORMS-AS-GLOBAL-FUNCTIONS"
           "ORDINARY-LAMBDA-LIST"
           "TYPE-DECLARATIONS"
           "MAKE-VALUE-COMPLETION-LAMBDA-FORM"
           "MAKE-TYPE-COMPLETION-LAMBDA-FORM"
           "MAKE-FORM-TYPES-LAMBDA-FORM"
           "FUNCTION-TYPE"
           "REWRITE-STORE-FUNCTION-FORM"))

(defpackage "SPECIALIZATION-STORE.DISPATCH"
  (:use "COMMON-LISP")

  ;; Types
  (:export "LAMBDA-PARAMETERS-COUNT")

  ;; Operations
  (:export "MAKE-DISPATCH-TREE"
           "REMOVE-DISPATCH-TREE-TAUTOLOGIES"
           "REMOVE-DISPATCH-TREE-CONSTANT-RULES"
           "REMOVE-RULE-TAUTOLOGIES"
           "REMOVE-CONSTANT-RULES"
           "EVALUATE-RULE"

           "FIXED-ARITY-STORE-PARAMETERS-P"
           "VARIABLE-ARITY-STORE-PARAMETERS-P"

           "SPECIALIZATION-PARAMETERS-LOWER-BOUND"
           "SPECIALIZATION-PARAMETERS-UPPER-BOUND"

           "PRETTY-PRINT-DISPATCH-TREE")

  ;; Trees
  (:export "NODE"
           "NODE-VALUE"
           "NODE-PASS"
           "NODE-FAIL"
           "MAKE-NODE"
           "LEAFP"

           "DEEPEN-TREE")

  ;; Rules
  (:export "FIXED-ARGUMENT-COUNT-RULE"
           "ARGUMENT-COUNT"
           "MAKE-FIXED-ARGUMENT-COUNT-RULE"

           "ACCEPTS-ARGUMENT-COUNT-RULE"
           "ARGUMENT-COUNT"
           "MAKE-ACCEPTS-ARGUMENT-COUNT-RULE"

           "POSITIONAL-PARAMETER-TYPE-RULE"
           "PARAMETER-POSITION"
           "PARAMETER-TYPE"
           "MAKE-POSITIONAL-PARAMETER-TYPE-RULE"

           "KEYWORD-PARAMETER-TYPE-RULE"
           "PARAMETER-KEYWORD"
           "PARAMETER-TYPE"
           "MAKE-KEYWORD-PARAMETER-TYPE-RULE"

           "CONSTANTLY-RULE"
           "CONSTANTLY-RULE-VALUE"
           "MAKE-CONSTANTLY-RULE"

           "REST-OBJECTS-RULE"
           "REST-OBJECTS-RULE-TYPE"
           "REST-OBJECTS-RULE-POSITION"
           "MAKE-REST-OBJECTS-RULE"

           "RULE-EQUAL"))

(defpackage "SPECIALIZATION-STORE.DISPATCH.FIXED-ARITY"
  (:use "COMMON-LISP")
  (:shadow "SET")
  (:export "MAKE-INITIAL-DISPATCH-TREE"
           "BUILD-TREE")
  (:export "SET"
           "SET-SPECIALIZATIONS"
           "SET-COUNT"
           "MAKE-SET")
  (:export "FIXED-ARITY-RULE"
           "FIXED-ARITY-RULE-INDEX"
           "FIXED-ARITY-RULE-TYPE"))

(defpackage "SPECIALIZATION-STORE.DISPATCH.VARIABLE-ARITY"
  (:use "COMMON-LISP")
  (:shadow "SET")
  (:export "MAKE-INITIAL-DISPATCH-TREE"))

(defpackage "SPECIALIZATION-STORE.STANDARD-STORE"
  (:use "COMMON-LISP"
        "SPECIALIZATION-STORE"))

(defpackage "SPECIALIZATION-STORE.GLOBALS")


;;;; Meta Object Requirements

#-(or sbcl ccl cmucl ecl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "No attempt has been made to determine if this implementation supports the Metaobject Protocol."))

(defpackage "SPECIALIZATION-STORE.MOP"
  (:export "FUNCALLABLE-STANDARD-CLASS"
           "FUNCALLABLE-STANDARD-OBJECT"
           "SET-FUNCALLABLE-INSTANCE-FUNCTION"
           "VALIDATE-SUPERCLASS")
  (:import-from #+sbcl "SB-MOP"
                #+ccl "CCL"
                #+cmucl "PCL"
                #+ecl "CLOS"
                "FUNCALLABLE-STANDARD-CLASS"
                "FUNCALLABLE-STANDARD-OBJECT"
                "SET-FUNCALLABLE-INSTANCE-FUNCTION"
                "VALIDATE-SUPERCLASS"))
