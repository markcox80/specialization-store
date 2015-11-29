(in-package "SPECIALIZATION-STORE.LAMBDA-LISTS.TESTS")
(in-suite lambda-list-tests)


;;;; parse-store-lambda-list

(test parse-store-lambda-list
  (flet ((do-trial (store-lambda-list req opt rest key? keys allow-other-keys?)
	   (let ((store-parameters (parse-store-lambda-list store-lambda-list)))
	     (is-true (typep store-parameters 'store-parameters))
	     (is (and (equal req (required-parameters store-parameters))
		      (equal opt (optional-parameters store-parameters))
		      (eql rest (rest-parameter store-parameters))
		      (eql key? (keyword-parameters-p store-parameters))
		      (equal keys (keyword-parameters store-parameters))
		      (eql allow-other-keys? (allow-other-keys-p store-parameters)))
		 "Failed to parse store lambda list ~W correctly." store-lambda-list))))
    (let ((required '((nil . nil)
		      ((a) . (a))
		      ((a b) . (a b))))
	  (optional '((nil . nil)
		      ((&optional) . nil)
		      ((&optional d) . ((d nil)))
		      ((&optional (e 5) d) . ((e 5) (d nil)))))
	  (rest '((nil . nil)
		  ((&rest args) . args)))
	  (keys '((nil nil nil nil)
		  ((&key) nil t nil)
		  ((&key m) ((:m m nil)) t nil)
		  ((&key m (n 2)) ((:m m nil) (:n n 2)) t nil)
		  ((&key (o 5) &allow-other-keys) ((:o o 5)) t t)
		  ((&key ((:o p) 5)) ((:o p 5)) t nil))))
      (loop for (r-list . r) in required do
	   (loop for (o-list . o) in optional do
		(loop for (rest-list . rest-var) in rest do
		     (loop for (keys-list k keys? allow-other-keys?) in keys do
			  (let ((lambda-list (append r-list o-list rest-list keys-list)))
			    (do-trial lambda-list r o rest-var keys? k allow-other-keys?)))))))))

(test parse-store-lambda-list/invalid-store-lambda-lists
  (flet ((trial (store-lambda-list)
	   (signals parse-store-lambda-list-error (parse-store-lambda-list store-lambda-list))))
    ;; Invalid markers
    (trial '(&rest))
    (trial '(&rest &rest))
    (trial '(&rest &key))
    (trial '(&rest &optional))
    (trial '(&rest &allow-other-keys))
    (trial '(&rest var &optional))
    (trial '(&rest var &allow-other-keys))
    (trial '(&optional &allow-other-keys))
    (trial '(&key &optional))
    (trial '(&key &rest))
    (trial '(&key &key))
    (trial '(&key &allow-other-keys &allow-other-keys))
    (trial '(&allow-other-keys))
    ;; Invalid parameter specifications
    (trial '((a 1)))
    (trial '(nil))
    (trial '(&optional (a nil a-p)))
    (trial '(&optional nil))
    (trial '(&optional (nil nil)))
    (trial '(&rest nil))
    (trial '(&rest (args 1)))
    (trial '(&key nil))
    (trial '(&key (a nil a-p)))
    (trial '(&key (nil nil)))))

(test parse-store-lambda-list/duplicate-keywords
  (labels ((do-trial (lambda-list)
	     (signals parse-store-lambda-list-error (parse-store-lambda-list lambda-list))))
    (macrolet ((trial (lambda-list)
		 `(do-trial ',lambda-list)))
      (trial (&key a a))
      (trial (&key a a b))
      (trial (&key a (a 1)))
      (trial (&key a ((:a b))))
      (trial (&key ((:a b)) (a 2))))))

;;;; parse-specialization-lambda-list

(test parse-specialization-lambda-list
  (flet ((do-trial (specialization-lambda-list req opt rest key? keys allow-other-keys?)
	   (let ((specialization-parameters (parse-specialization-lambda-list specialization-lambda-list)))
	     (is-true (typep specialization-parameters 'specialization-parameters))
	     (is (and (equal req (required-parameters specialization-parameters))
		      (equal opt (optional-parameters specialization-parameters))
		      (eql rest (rest-parameter specialization-parameters))
		      (eql key? (keyword-parameters-p specialization-parameters))
		      (equal keys (keyword-parameters specialization-parameters))
		      (eql allow-other-keys? (allow-other-keys-p specialization-parameters)))
		 "Failed to parse specialization lambda list ~W correctly." specialization-lambda-list))))
    (let ((required '((nil . nil)
		      ((a) . ((a t)))
		      ((a b) . ((a t) (b t)))
		      ((a (b integer)) . ((a t) (b integer)))))
	  (optional '((nil . nil)
		      ((&optional) . nil)
		      ((&optional d) . ((d nil nil)))
		      ((&optional (e 5) d) . ((e 5 nil) (d nil nil)))
		      ((&optional (e 6 e-p)) . ((e 6 e-p)))))
	  (rest '((nil . nil)
		  ((&rest args) . args)))
	  (keys '((nil nil nil nil)
		  ((&key) nil t nil)
		  ((&key m) ((:m m nil nil)) t nil)
		  ((&key m (n 2)) ((:m m nil nil) (:n n 2 nil)) t nil)
		  ((&key (o 5) &allow-other-keys) ((:o o 5 nil)) t t)
		  ((&key (m 10 m-p)) ((:m m 10 m-p)) t nil)
		  ((&key ((:k m) 10 m-p) &allow-other-keys) ((:k m 10 m-p)) t t))))
      (loop for (r-list . r) in required do
	   (loop for (o-list . o) in optional do
		(loop for (rest-list . rest-var) in rest do
		     (loop for (keys-list k keys? allow-other-keys?) in keys do
			  (let ((lambda-list (append r-list o-list rest-list keys-list)))
			    (do-trial lambda-list r o rest-var keys? k allow-other-keys?)))))))))

(test parse-specialization-lambda-list/invalid-specialization-lambda-lists
  (flet ((trial (specialization-lambda-list)
	   (signals parse-specialization-lambda-list-error (parse-specialization-lambda-list specialization-lambda-list))))
    ;; Invalid markers
    (trial '(&rest))
    (trial '(&rest &rest))
    (trial '(&rest &key))
    (trial '(&rest &optional))
    (trial '(&rest &allow-other-keys))
    (trial '(&rest var &optional))
    (trial '(&rest var &allow-other-keys))
    (trial '(&optional &allow-other-keys))
    (trial '(&key &optional))
    (trial '(&key &rest))
    (trial '(&key &key))
    (trial '(&key &allow-other-keys &allow-other-keys))
    (trial '(&allow-other-keys))
    ;; Invalid parameter specifications
    (trial '(nil))
    (trial '(&optional nil))
    (trial '(&optional (nil nil)))
    (trial '(&rest nil))
    (trial '(&rest (args 1)))
    (trial '(&key nil))
    (trial '(&key (nil nil)))))

(test parse-specialization-lambda-list/duplicate-keywords
  (labels ((do-trial (lambda-list)
	     (signals parse-specialization-lambda-list-error (parse-specialization-lambda-list lambda-list))))
    (macrolet ((trial (lambda-list)
		 `(do-trial ',lambda-list)))
      (trial (&key a a))
      (trial (&key a a b))
      (trial (&key a (a 1)))
      (trial (&key a ((:a b))))
      (trial (&key ((:a b)) (a 2))))))

(test parse-specialization-lambda-list/duplicate-variables
  (labels ((do-trial (lambda-list)
	     (signals parse-specialization-lambda-list-error (parse-specialization-lambda-list lambda-list))))
    (macrolet ((trial (lambda-list)
		 `(do-trial ',lambda-list)))
      (trial (a a))
      (trial (a &optional a))
      (trial (a &optional (a nil)))
      (trial (a &optional (a nil a-p)))
      (trial (a &optional (b nil a)))
      (trial (a &rest a))
      (trial (a &key a))
      (trial (a &key (a nil)))
      (trial (a &key (a nil a-p)))
      (trial (a &key ((:b a) nil a-p)))
      (trial (a &key (b nil a)))
      (trial (&optional a &rest a))
      (trial (&optional a &key a))
      (trial (&optional a &key (a nil)))
      (trial (&optional a &key (a nil a-p)))
      (trial (&optional a &key ((:b a) nil a-p)))
      (trial (&optional a &key (b nil a)))
      (trial (&rest a &key a))
      (trial (&rest a &key (a nil)))
      (trial (&rest a &key (a nil a-p)))
      (trial (&rest a &key ((:b a) nil a-p)))
      (trial (&rest a &key (b nil a)))
      (trial (&key a a))
      (trial (&key a (a nil)))
      (trial (&key a (a nil a-p)))
      (trial (&key a (b nil a))))))


;;;; Congruent Lambda lists

(test congruent-specialization-parameters
  (flet ((do-trial (expected store-lambda-list specialization-lambda-list)
	   (let* ((store-parameters (parse-store-lambda-list store-lambda-list))
                  (specialization-parameters (parse-specialization-lambda-list specialization-lambda-list))
                  (congruence (congruent-parameters-p store-parameters specialization-parameters)))
	     (if expected
		 (is-true congruence)
		 (is-false congruence)))))
    (macrolet ((true (store-lambda-list specialization-lambda-list)
		 `(do-trial t ',store-lambda-list ',specialization-lambda-list))
	       (false (store-lambda-list specialization-lambda-list)
		 `(do-trial nil ',store-lambda-list ',specialization-lambda-list)))
      ;; Positional
      (true (a) (b))
      (false (a) ())

      (true (a &optional b) (b c))
      (false (a &optional b) (b))

      (true (a &rest args) (a b))
      (true (a &rest args) (a &optional b))

      ;; Keys
      (true (a &optional b &key c) (a b &key c))
      (true (a &optional b &key c) (a b &key c d))
      (true (&key c) (&key ((:c d))))
      (false (a &key c d) (a &key d))
      (false (a &key c d) (a &key c))
      (false (a) (b &key))
      (false (a &key) (b)))))

(test congruent-store-parameters
  (flet ((do-trial (expected lambda-list-1 lambda-list-2)
           (let* ((p1 (parse-store-lambda-list lambda-list-1))
                  (p2 (parse-store-lambda-list lambda-list-2))
                  (congruence-1 (congruent-parameters-p p1 p2))
                  (congruence-2 (congruent-parameters-p p2 p1)))
             (is (eql congruence-1 congruence-2) "The function ~W should be commutative with arguments ~W and ~W."
                 'congruent-parameters-p lambda-list-1 lambda-list-2)
             (if expected
                 (is-true congruence-1 "Store lambda list ~W should not be congruent with ~W." lambda-list-1 lambda-list-2)
                 (is-false congruence-1 "Store lambda list ~W should be congruent with ~W." lambda-list-1 lambda-list-2)))))
    (macrolet ((true (lambda-list-1 lambda-list-2)
                 `(do-trial t ',lambda-list-1 ',lambda-list-2))
               (false (lambda-list-1 lambda-list-2)
                 `(do-trial nil ',lambda-list-1 ',lambda-list-2)))
      ;; Required
      (true (a) (b))
      (true (a b) (c d))
      (false (a) ())
      (false (a) (b c))
      (false (a) (&optional b))
      (false (a) (&rest args))
      (false (a) (&key))
      (false (a) (&key hey))
      (false (a) (&key hey &allow-other-keys))

      ;; Optional
      (true (&optional a) (&optional b))
      (true (&optional a b) (&optional b c))
      (false (&optional c) ())
      (false (&optional c) (a))
      (false (&optional c) (a b))
      (false (&optional c) (&rest args))
      (false (&optional c) (&key))
      (false (&optional c) (&key d))
      (false (&optional c) (&key d &allow-other-keys))

      ;; Rest
      (true (&rest args) (&rest args))
      (true (&rest args) (&rest others))
      (false (&rest args) ())
      (false (&rest args) (a))
      (false (&rest args) (&optional a))
      (false (&rest args) (&key c))
      (false (&rest args) (&key c &allow-other-keys))

      ;; Keys
      (true (&key) (&key))
      (true (&key) (&key &allow-other-keys))
      (true (&key &allow-other-keys) (&key &allow-other-keys))
      (true (&key c) (&key c))
      (true (&key c) (&key ((:c d))))
      (true (&rest args &key c) (&rest others &key c))
      (true (&rest args &key c) (&key c))
      (false (&key c) (a))
      (false (&key c) (&optional a))
      (false (&key c) (&rest args))
      (false (&key c) (&key c d))
      (false (&key c) (&key))
      (false (&key c) (&key &allow-other-keys))

      ;; Complex
      (true (a b &optional c &rest args) (e f &optional g &rest others))
      (true (a &optional b &rest args &key f) (z &optional y &rest others &key ((:f x))))
      (false (a b &optional c &rest args) (e f &optional g &rest others &key &allow-other-keys))
      (false (a b &optional c &rest args) (e f &optional g))
      (false (a b &optional c &rest args) (e &optional g)))))
