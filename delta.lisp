;;;; Derivatives

(defvar *delta-table* '((sin (cos x)) (cos (0 - (sin x)))
			(tan (1 / ((cos x) ^ 2)))
			(sinh (cosh x)) (cosh (sinh x))
			(tanh (1 / ((cosh x) ^ 2)))
			(asin (1 / ((1 - (x ^ 2)) ^ 1/2)))
			(acos (0 - (1 / ((1 - (x ^ 2)) ^ 1/2))))
			(atan (1 / (1 + (x ^ 2))))
			(asinh (1 / ((1 + (x ^ 2)) ^ 1/2)))
			(acosh (1 / (((x - 1) * (x + 1)) ^ 1/2)))
			(atanh (1 / (1 - (x ^ 2)))) (log (1 / x))))

(defun delta-atom (atom wrt)
  (if (eql atom wrt) 1 0))

(defun delta-+ (list wrt)
  (let ((a (first list)) (b (third list)))
    (simple-+ (delta a wrt) (delta b wrt))))

(defun delta-- (list wrt)
  (let ((a (first list)) (b (third list)))
    (simple-- (delta a wrt) (delta b wrt))))

(defun delta-* (list wrt)
  (let ((a (first list)) (b (third list)))
    (simple-+ (simple-* (delta a wrt) b)
	      (simple-* (delta b wrt) a))))

(defun delta-/ (list wrt)
  (let ((a (first list)) (b (third list)))
    (simple-/ (simple-- (simple-* (delta a wrt) b)
			(simple-* (delta b wrt) a))
	      (simple-^ b 2))))

(defun delta-power (list wrt)
  (let ((a (first list)) (b (third list)))
    (simple-* (delta a wrt)
	      (simple-* b (simple-^ a (simple-- b 1))))))

(defun delta-exp (list wrt)
  (let ((a (first list)) (b (third list)))
    (simple-* (delta b wrt)
	      (simple-* `(log ,a) (simple-^ a b)))))

(defun delta-^ (list wrt)
  (simple-+ (delta-power list wrt)
	    (delta-exp list wrt)))

(defun delta-operate (operator list wrt)
  (cond
    ((eql operator '+) (delta-+ list wrt))
    ((eql operator '-) (delta-- list wrt))
    ((eql operator '*) (delta-* list wrt))
    ((eql operator '/) (delta-/ list wrt))
    ((eql operator '^) (delta-^ list wrt))
    (t (dolist (pair *delta-table*)
	 (when (eql operator (first pair))
	   (return (simple-* (delta (second list) wrt)
			     (simplify (second pair)))))))))

(defun delta (expr wrt)
  (cond
    ((atom expr) (delta-atom expr wrt))
    ((null (rest expr)) (delta (first expr) wrt))
    ((eql (first expr) '-) (delta-* (negative expr) wrt))
    (t (dolist (pair *op*)
	 (when (eql (first pair) (nth (second pair) expr))
	   (return (delta-operate (first pair) expr wrt)))))))
