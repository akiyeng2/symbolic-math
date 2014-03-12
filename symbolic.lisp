;;;; Some symbolic algebra in infix notation

;; Fix clashes

;;; Set constants and aliases to functions
(defconstant e (exp 1) "Euler's Number")  
(defconstant i #C(0 1) "Imaginary Number")
(defun ^ (a b) (expt a b))

(defparameter op '((+ 1) (- 1) (* 1) (/ 1) (^ 1)
		   (log 0) (sin 0) (cos 0) (tan 0)
		   (sinh 0) (cosh 0) (tanh 0)
		   (asin 0) (acos 0) (atan 0)
		   (asinh 0) (acosh 0) (atanh 0)))

(defun delta-op (op)
  (read-from-string (concatenate 'string "delta-" (string op))))

;; Symbolic macros
(defmacro simple-infix (a b operator clauses-one clauses-two)
  `(cond ((and (numberp ,a) (numberp ,b))
	  (,operator ,a ,b))
	 ((numberp ,a)
	  (cond ,@clauses-one))
	 ((numberp ,b)
	  (cond ,@clauses-two))
	 (t `(,a ,',operator ,b))))
 
;;; Simplifying functions
(defun simple (operator a &optional (b nil))
  (cond ((numberp a) (funcall operator a))
	(t '(operator a))))

(defun simple-+ (a b)
  (simple-infix a b +
		(((zerop a) b) (t `(,a + ,b)))
		(((zerop b) a) (t `(,a + ,b)))))

(defun simple-- (a b)
  (simple-infix a b -
		(((zerop a) `(- ,b)) (t `(,a - ,b)))
		(((zerop b) a) (t `(,a - ,b)))))

(defun simple-* (a b)
  (simple-infix a b *
		(((zerop a) 0) ((= a 1) b) (t `(,a * ,b)))
		(((zerop b) 0) ((= b 1) a) (t `(,a * ,b)))))

(defun simple-/ (a b)
  (simple-infix a b /
		(((zerop a) 0) (t `(,a / ,b)))
		((t `(,a / ,b)))))

(defun simple-^ (a b)
  (simple-infix a b ^
		(((zerop a) 0) ((= a 1) 1) (t `(,a ^ ,b)))
		(((zerop b) 1) ((= b 1) a) (t `(,a ^ ,b)))))

;;; Derivative helper functions
(defun delta-atom (atom wrt)
  (if (eql atom wrt) 1 0))

(defun delta-+ (+-list wrt)
  (let ((a (first +-list)) (b (third +-list)))
    (simple-+ (delta a wrt) (delta b wrt))))

(defun delta-- (--list wrt)
  (let ((a (first --list)) (b (third --list)))
    (simple-- (delta a wrt) (delta b wrt))))

(defun delta-* (*-list wrt)
  (let ((a (first *-list)) (b (third *-list)))
    (simple-+ (simple-* (delta a wrt) b)
	      (simple-* (delta b wrt) a))))

(defun delta-/ (/-list wrt)
  (let ((a (first /-list)) (b (third /-list)))
    (simple-/ (simple-- (simple-* (delta a wrt) b)
			(simple-* (delta b wrt) a))
	      (simple-^ b 2))))

(defun delta-power (power-list wrt)
  (let ((a (first power-list)) (b (third power-list)))
    (simple-* (delta a wrt)
	      (simple-* b (simple-^ a (simple-- b 1))))))

(defun delta-exp (exp-list wrt)
  (let ((a (first exp-list)) (b (third exp-list)))
    (simple-* (delta b wrt)
	      (simple-* `(log ,a) (simple-^ a b)))))

(defun delta-^ (^-list wrt)
  (simple-+ (delta-power ^-list wrt)
	    (delta-exp ^-list wrt)))

(defun delta-log (log-list wrt)
  (let ((a (second log-list)))
    (simple-* (delta a wrt) (simple-/ 1 a))))

(defun delta-sin (sin-list wrt)
  (let ((a (second sin-list)))
    (simple-* (delta a wrt) (simple 'cos a))))

(defun delta-cos (cos-list wrt)
  (let ((a (second cos-list)))
    (simple-* (delta a wrt) (simple-- 0 (simple 'sin a)))))

(defun delta-tan (tan-list wrt)
  (let ((a (second tan-list)))
    (simple-* (delta a wrt)
	      (simple-/ 1 (simple-^ (simple 'cos a) 2)))))

(defun delta-asin (asin-list wrt)
  (let ((a (second asin-list)))
    (simple-* (delta a wrt)
	      (simple-/ 1 (simple-^ (simple-- 1 (simple-^ a 2)) 1/2)))))

(defun delta-acos (acos-list wrt)
  (let ((a (second acos-list)))
    (simple-* (delta a wrt)
	      (simple-- 0 (simple-/ 1 (simple-^ (simple-- 1 (simple-^ a 2)) 1/2))))))

(defun delta-atan (atan-list wrt)
  (let ((a (second atan-list)))
    (simple-* (delta a wrt)
	      (simple-/ 1 (simple-+ 1 (simple-^ a 2))))))

(defun delta-sinh (sinh-list wrt)
  (let ((a (second sinh-list)))
    (simple-* (delta a wrt) (simple 'cosh a))))

(defun delta-cosh (cosh-list wrt)
  (let ((a (second cosh-list)))
    (simple-* (delta a wrt) (simple 'sinh a))))

(defun delta-tanh (tanh-list wrt)
  (let ((a (second tanh-list)))
    (simple-* (delta a wrt)
	      (simple-/ 1 (simple-^ (simple 'cosh a) 2)))))

(defun delta-asinh (asinh-list wrt)
  (let ((a (second asinh-list)))
    (simple-* (delta a wrt)
	      (simple-/ 1 (simple-^ (simple-+ 1 (simple-^ a 2)) 1/2)))))

(defun delta-acosh (acosh-list wrt)
  (let ((a (second acosh-list)))
    (simple-* (delta a wrt)
	      (simple-/ 1 (simple-* (simple-^ (simple-- a 1) 1/2)
				    (simple-^ (simple-+ a 1) 1/2))))))

(defun delta-atanh (atanh-list wrt)
  (let ((a (second atanh-list)))
    (simple-* (delta a wrt)
	      (simple-/ 1 (simple-- 1 (simple-^ a 1/2))))))

;;; Derivative taker
(defun delta (expression wrt)
  (cond 
    ((atom expression)
     (delta-atom expression wrt))
    ((null (rest expression))
     (delta (first expression) wrt))
    ((eql (first expression) '-)
     (delta-- `(0 - ,(second expression)) wrt))
    (t
     (dolist (pair op)
       (when (eql (first pair) (nth (second pair) expression))
	 (return
	   (funcall (delta-op (first pair)) expression wrt)))))))

	



