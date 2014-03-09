;;;; Some symbolic algebra in infix notation

;;; TODO: Write a macro to auto-gen code for simple- functions.

;;; Set constants and aliases to functions
(defconstant e (exp 1) "Euler's Number")  
(defconstant i #C(0 1) "Imaginary Number")
(setf (symbol-function '^) #'expt)

(defmacro simple-infix (leftclauses rightclauses a b operator)
  `(cond ((and (numberp ,a) (numberp ,b))
	 (,operator ,a ,b))
	((numberp ,a)
	 (cond ,@leftclauses)
	((numberp ,b)
	 (cond ,@rightclauses)))
	(t ,`(,a ',operator ,b))))

;;; Simplifying functions
(defun simple-+ (a b)
  (simple-infix (((zerop a) 0) (t `(,a + ,b)))
		(((zerop b) 0) (t `(,a + ,b)))
		a b +))

(defun simple-- (a b)
  (simple-infix (((zerop a) `(- ,b)) (t `(,a - ,b)))
		(((zerop b) a) (t `(,a - ,b)))
		a b -))

(defun simple-* (a b)
  (simple-infix (((zerop a) 0) ((= a 1) b) (t `(,a * ,b)))
		(((zerop b) 0) ((= b 1) a) (t `(,a * ,b)))
		a b *))

(defun simple-/ (a b)
  (simple-infix (((zerop a) 0) (t `(,a / ,b)))
		(t `(,a / ,b))
		a b /))

(defun simple-^ (a b)
  (simple-infix (((zerop a) 0) ((= a 1) 1) (t `(,a ^ ,b)))
		(((zerop b) 1) ((= b 1) a) (t `(,a ^ ,b)))
		a b ^))

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

(defun delta-power (power-list wrt)
  (let ((a (first power-list)) (b (third power-list)))
    (simple-* (delta a wrt)
		  (simple-* b (simple-^ a (simple-- b 1))))))

(defun delta-exp (exp-list wrt)
  (let ((a (first exp-list)) (b (third exp-list)))
    (simple-* (delta b wrt)
		  (simple-* (list 'log a)
				(simple-^ a b)))))

(defun delta-^ (^-list wrt)
  (simple-+ (delta-power ^-list wrt)
	    (delta-exp ^-list wrt)))

;;; Derivative taker
(defun delta (expression wrt)
  (cond
    ((atom expression)
     (delta-atom expression wrt))
    ((eql (first expression) '-)
     (delta-- (list 0 '- (second expression)) wrt))
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
