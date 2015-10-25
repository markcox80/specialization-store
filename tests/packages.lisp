(defpackage "SPECIALIZATION-STORE.TESTS"
  (:use "COMMON-LISP"
	"SPECIALIZATION-STORE"
	"5AM")
  (:export "ALL-TESTS"))

(defpackage "SPECIALIZATION-STORE.LAMBDA-LISTS.TESTS"
  (:use "COMMON-LISP"
	"SPECIALIZATION-STORE.LAMBDA-LISTS"
	"5AM")
  (:export "LAMBDA-LIST-TESTS"))

(5am:def-suite specialization-store.tests:all-tests)
(5am:def-suite specialization-store.lambda-lists.tests:lambda-list-tests :in specialization-store.tests:all-tests)
