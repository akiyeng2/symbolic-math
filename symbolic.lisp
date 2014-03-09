;;;; Some symbolic algebra in infix notation

;;; Set constants and aliases to functions
(defconstant e (exp 1) "Euler's Number")  
(defconstant i #C(0 1) "Imaginary Number")
(setf (symbol-function '^) #'expt)

;; Symbolic macros
(defmacro simple-prefix (a operator &rest clauses)
  `(cond ((numberp ,a) (,operator ,a))
	 ,@clauses
	 (t `(,',operator ,a))))

(defmacro simple-infix (a b operator leftclauses rightclauses)
  `(cond ((and (numberp ,a) (numberp ,b))
	  (,operator ,a ,b))
	 ((numberp ,a)
	  (cond ,@leftclauses))
	 ((numberp ,b)
	  (cond ,@rightclauses))
	 (t `(,a ,',operator ,b))))

;;; Simplifying functions
;; Infix
; Trignometric
(defun simple-sin (a)
  (simple-prefix a sin))

(defun simple-cos (a)
  (simple-prefix a cos))

(defun simple-tan (a)
  (simple-prefix a tan))

(defun simple-cot (a)
  (simple-prefix a cot))

(defun simple-asin (a)
  (simple-prefix a asin))

(defun simple-acos (a)
  (simple-prefix a acos))

(defun simple-atan (a)
  (simple-prefix a atan))

(defun simple-acot (a)
  (simple-prefix a acot))

; Hyperbolic
(defun simple-sinh (a)
  (simple-prefix a sinh))

(defun simple-cosh (a)
  (simple-prefix a cosh))

(defun simple-tanh (a)
  (simple-prefix a tanh))

(defun simple-coth (a)
  (simple-prefix a coth))

(defun simple-asinh (a)
  (simple-prefix a asinh))

(defun simple-acosh (a)
  (simple-prefix a acosh))

(defun simple-atanh (a)
  (simple-prefix a atanh))

(defun simple-acoth (a)
  (simple-prefix a acoth))

;; Prefix
(defun simple-+ (a b)
  (simple-infix a b +
		(((zerop a) b) (t `(,a + ,b)))
		(((zerop b) a) (t `(,a + ,b)))))

(defun simple-- (b &optional (a 0))
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

;;; Derivative taker
(defun delta (expression wrt)
  (cond ((atom expression)
	 (delta-atom expression wrt))
	((eql (first expression) '-)
	 (delta-- `(0 - ,(second expression)) wrt))
	((eql (second expression) '+)
	 (delta-+ expression wrt))
	((eql (second expression) '-)
	 (delta-- expression wrt))
	((eql (second expression) '*)
	 (delta-* expression wrt))
	((eql (second expression) '/)
	 (delta-/ expression wrt))
	((eql (second expression) '^)
	 (delta-^ expression wrt))))
