;;;; Some symbolic algebra in infix notation

;;; TODO: Write a macro to auto-gen code for simple- functions.

;;; Set constants and aliases to functions
(defconstant e (exp 1) "Euler's Number")  
(defconstant i #C(0 1) "Imaginary Number")
(setf (symbol-function '^) #'expt)

;;; Simplifying functions
(defun simple-+ (a b)
  (cond ((and (numberp a) (numberp b))
	 (+ a b))
	((numberp a)
	 (if (zerop a)
	     b
	     (list b '+ a)))
	((numberp b)
	 (if (zerop b)
	     a
	     (list a '+ b)))
	(t (list a '+ b))))

(defun simple-- (a b)
  (cond ((and (numberp a) (numberp b))
	 (- a b))
	((numberp a)
	 (if (zerop a)
	     (list '- b)
	     (list b '- a)))
	((numberp b)
	 (if (zerop b)
	     a
	     (list a '- b)))
	(t (list a '- b))))

(defun simple-* (a b)
  (cond ((and (numberp a) (numberp b))
	 (* a b))
	((numberp a)
	 (cond  ((zerop a) 0) 
		((= a 1) b)
		(t (list a '* b))))
	((numberp b)
	 (cond ((zerop b) 0) 
	       ((= b 1) a)
	       (t (list a '* b))))
	(t (list a '* b))))

(defun simple-/ (a b)
  (cond ((and (numberp a) (numberp b))
	 (/ a b))
	((numberp a)
	 (cond  ((zerop a) 0) 
		(t (list a '/ b))))
	((numberp b)
	       (list a '/ b))
	(t (list a '* b))))

(defun simple-^ (a b)
  (cond ((and (numberp a) (numberp b))
	 (^ a b))
	((numberp a)
	 (cond ((zerop a) 0)
	       ((= a 1) 1)
	       (t (list a '^ b))))
	((numberp b)
	 (cond ((zerop b) 1)
	       ((= b 1) a)
	       (t (list a '^ b))))
	(t (list a '^ b))))

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
     
    
	
  
