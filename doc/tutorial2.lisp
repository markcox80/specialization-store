(defpackage "TUT2"
  (:use "COMMON-LISP"
        "SPECIALIZATION-STORE"))
(in-package "TUT2")

;; Helper
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-display (form fn)
    (handler-case (format t "~&~W => ~{~W~^ ~}~%"
                          form
                          (multiple-value-list (funcall fn)))
      (error (c)
        (format t "~&~W => Signalled error: ~W.~%" form (type-of c))))
    (values)))

(defmacro display (form)
  `(do-display ',form (lambda () ,form)))

;; y := a x + b y
(defstore nxpy (y x &optional (a 1) (b 1)))
(defspecialization nxpy ((y sequence) (x sequence) (a number) (b number)) (values)
  (map-into y (lambda (y-i x-i)
                (+ (* b y-i) (* a x-i)))
            y x))

(defun test1 ()
  (let* ((y (list 1 2 3))
         (x (list 4 5 6)))
    (nxpy y x 2 3)
    (print y)))

;; y := Hadamard_product(a x, y)
;; or
;; y-i := a x-i y-i
(defstore nhadamard-product (y x &key (a 1)))
(defspecialization nhadamard-product ((y sequence) (x sequence) &key (a number)) (values)
  (map-into y (lambda (y-i x-i)
                (* a y-i x-i))
            y x))

(defspecialization nhadamard-product ((y number) (x number) &key (a number) (b "hello")) t
  (list y x a b))

(defun test2 ()
  (let* ((y (list 1 2 3))
         (x (list 4 5 6)))
    (nhadamard-product y x :a 2)
    (print y))

  (display (nhadamard-product 1 2))
  (display (nhadamard-product 3 4 :b "garbage" :allow-other-keys t)))


;; Add
;; y = x1 + x2 + ... + xN

(defstore add (&rest args))

(defspecialization add () (eql 0)
  0)

(defspecialization add ((x1 number)) number
  x1)

(defspecialization add ((x1 number) (x2 number)) number
  (+ x1 x2))

(defspecialization add ((x1 number) (x2 number) (x3 number) &rest (args number)) number
  (apply #'add (add x1 x2) x3 args))

(defspecialization add ((x1 number) (x2 number) (x3 number) (x4 number) &rest (args number)) number
  (print "here")
  (values (apply #'add (add x1 x2) x3 x4 args)
          "Arity 4"))

(defun test3 ()
  (display (add))
  (display (add 1))
  (display (add 1 2))
  (display (add 1 2 3))
  (display (add 1 2 3 4 "hello"))
  (display (add 1 2 3 4)))
