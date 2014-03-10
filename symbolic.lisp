;;;; Some symbolic algebra in infix notation

;; TODO: write a macro to autogen delta code

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

(defun simple-asin (a)
  (simple-prefix a asin))

(defun simple-acos (a)
  (simple-prefix a acos))

(defun simple-atan (a)
  (simple-prefix a atan))

; Hyperbolic
(defun simple-sinh (a)
  (simple-prefix a sinh))

(defun simple-cosh (a)
  (simple-prefix a cosh))

(defun simple-tanh (a)
  (simple-prefix a tanh))

(defun simple-asinh (a)
  (simple-prefix a asinh))

(defun simple-acosh (a)
  (simple-prefix a acosh))

(defun simple-atanh (a)
  (simple-prefix a atanh))

;; Prefix
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

(defun delta-sin (sin-list wrt)
  (let ((a (second sin-list)))
    (simple-* (delta a wrt) (simple-cos a))))

(defun delta-cos (cos-list wrt)
  (let ((a (second cos-list)))
    (simple-* (delta a wrt) (simple-- 0 (simple-sin a)))))

(defun delta-tan (tan-list wrt)
  (let ((a (second tan-list)))
    (simple-* (delta a wrt)
	      (simple-/ 1 (simple-^ (simple-cos a) 2)))))

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
    (simple-* (delta a wrt) (simple-cosh a))))

(defun delta-cosh (cosh-list wrt)
  (let ((a (second cosh-list)))
    (simple-* (delta a wrt) (simple-sinh a))))

(defun delta-tanh (tanh-list wrt)
  (let ((a (second tanh-list)))
    (simple-* (delta a wrt)
	      (simple-/ 1 (simple-^ (simple-cosh a) 2)))))

(defun delta-asinh (asinh-list wrt)
  (let ((a (second asinh-list)))
    (simple-* (delta a wrt)
	      (simple-/ 1 (simple-^ (simple-+ 1 (simple-^ a 2)) 1/2)))))

(defun delta-acosh (acosh-list wrt)
  (let ((a (second acosh-list)))
    (simple-* (delta a wrt)
	      (simple-/ 1 (simple-* (simple-^ (simple-- a 1) 1/2)
				    (simple-^ (simple-+ a 1) 1.2))))))

(defun delta-atanh (atanh-list wrt)
  (let ((a (second atanh-list)))
    (simple-* (delta a wrt)
	      (simple-/ 1 (simple-- 1 (simple-^ a 1/2))))))

;;; Derivative taker
(defun delta (expression wrt)
  (cond ((atom expression)
	 (delta-atom expression wrt))
	((null (rest expression))
	 (delta (first expression) wrt))
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
	 (delta-^ expression wrt))
	((eql (first expression) 'sin)
	 (delta-sin expression wrt))
	((eql (first expression) 'cos)
	 (delta-cos expression wrt))
	((eql (first expression) 'tan)
	 (delta-tan expression wrt))
	((eql (first expression) 'asin)
	 (delta-asin expression wrt))
	((eql (first expression) 'acos)
	 (delta-acos expression wrt))
	((eql (first expression) 'atan)
	 (delta-atan expression wrt))
	((eql (first expression) 'sinh)
	 (delta-sinh expression wrt))
	((eql (first expression) 'cosh)
	 (delta-cosh expression wrt))
	((eql (first expression) 'tanh)
	 (delta-tanh expression wrt))
	((eql (first expression) 'asinh)
	 (delta-asinh expression wrt))
	((eql (first expression) 'acosh)
	 (delta-acosh expression wrt))
	((eql (first expression) 'atanh)
	 (delta-atanh expression wrt))))
