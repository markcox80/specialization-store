(defpackage "TUT1"
  (:use "COMMON-LISP"
        "SPECIALIZATION-STORE"))
(in-package "TUT1")

(defstore naxpy (a x y))

(defspecialization naxpy ((a number) (x sequence) (y sequence)) (values)
  (unless (zerop a)
    (map-into y (lambda (x-i y-i)
                  (+ (* a x-i) y-i))
              x y))
  (values))

(defspecialization naxpy ((a number) (x list) (y list)) (values)
  (unless (zerop a)
    (loop
      for sub-y on y
      for x-i in x
      for y-i = (car sub-y)
      do
         (incf (car sub-y) (* a x-i))))
  (values))

(defspecialization naxpy ((a number) (x array) (y array)) (values)
  (unless (zerop a)
    (dotimes (i (array-total-size x))
      (incf (row-major-aref y i) (* a (row-major-aref x i)))))
  (values))

(defspecialization naxpy ((a (satisfies zerop)) (x t) (y t)) (values)
  (declare (ignore a x y))
  (values))
