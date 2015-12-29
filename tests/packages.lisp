(defpackage "SPECIALIZATION-STORE.TESTS"
  (:use "COMMON-LISP"
	"SPECIALIZATION-STORE"
	"5AM")
  (:export "ALL-TESTS"
           "PROCESS-SYNTAX-LAYER-TESTS"))

(defpackage "SPECIALIZATION-STORE.LAMBDA-LISTS.TESTS"
  (:use "COMMON-LISP"
	"SPECIALIZATION-STORE.LAMBDA-LISTS"
	"5AM")
  (:export "LAMBDA-LIST-TESTS"))

(defpackage "SPECIALIZATION-STORE.STANDARD-STORE.TESTS"
  (:use "COMMON-LISP"
        "SPECIALIZATION-STORE"
        "SPECIALIZATION-STORE.STANDARD-STORE"
        "5AM")
  (:export "STANDARD-STORE-TESTS"))

(5am:def-suite specialization-store.tests:all-tests)
(5am:def-suite specialization-store.lambda-lists.tests:lambda-list-tests :in specialization-store.tests:all-tests)
(5am:def-suite specialization-store.standard-store.tests:standard-store-tests :in specialization-store.tests:all-tests)
