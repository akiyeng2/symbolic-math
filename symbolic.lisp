;;;; Some symbolic algebra in infix notation

;; TODO: rewrite macro and delta-op to infix

;;; Set globals
(defconstant e (exp 1) "Euler's Number")  
(defconstant i #C(0 1) "Imaginary Number")
(defun ^ (a b) (expt a b))

(defvar op '((+ 1) (- 1) (* 1) (/ 1) (^ 1) (log 0)
	     (sin 0) (cos 0) (tan 0) (sinh 0) (cosh 0)
	     (tanh 0) (asin 0) (acos 0) (atan 0) (asinh 0)
	     (acosh 0) (atanh 0)))

(defvar delta-table '((sin (cos x)) (cos (* -1 (sin x)))
		      (tan (/ (^ (cos x) 2)))
		      (sinh (cosh x)) (cosh (sinh x))
		      (tanh (/ (^ (cosh x) 2)))
		      (asin (/ (^ (- 1 (^ x 2)) 1/2)))
		      (acos (- (/ (^ (- 1 (^ x 2)) 1/2))))
		      (atan (/ (+ (^ x 2))))
		      (asinh (/ (^ (+ 1 (^ x 2)) 1/2)))
		      (acosh (/ (^ (* (- x 1) (+ x 1)) 1/2)))
		      (atanh (/ (- 1 (^ x 2)))) (log (/ x))))

(defmacro simple-infix (a b operator clauses-one clauses-two &optional (clauses-three nil))
  `(cond ((and (numberp ,a) (numberp ,b))
	  (,operator ,a ,b))
	 ((numberp ,a)
	  (cond ,@clauses-one))
	 ((numberp ,b)
	  (cond ,@clauses-two))
	 (t (cond ,@clauses-three
		  (t `(,a ,',operator ,b))))))

;;; Simplifying functions
(defun get-var (list)
  (first (member-if #'symbolp list)))

(defun get-num (list)
  (first (member-if #'numberp list)))

(defun simple-+ (a b)
  (simple-infix a b +
		(((zerop a) b) (t `(,a + ,b)))
		(((zerop b) a) (t `(,a + ,b)))
		(((eql a b) `(,2 * ,a))
		 ((and (listp b) (listp a)
		       (member '* b) (member '* a)
		       (eql (get-var a) (get-var b)))
		  (simple-* (get-var a) (+ (get-num a) (get-num b))))
		 ((and (listp b) (member '* b) (member a b))
		  (if (member-if #'numberp (member '* b))
		      (simple-* a (simple-+ (third b) 1))
		      (simple-* a (simple-+ (first b) 1))))
		 ((and (listp a) (member '* a) (member b a))
		  (if (member-if #'numberp (member '* a))
		      (simple-* b (simple-+ (third a) 1))
		      (simple-* b (simple-+ (first a) 1)))))))

(defun simple-- (a b)
  (simple-infix a b -
		(((zerop a) `(- ,b)) (t `(,a - ,b)))
		(((zerop b) a) (t `(,a - ,b)))
		(((eql a b) 0)
		 ((and (listp b) (listp a)
		       (member '* b) (member '* a)
		       (eql (get-var a) (get-var b)))
		  (simple-* (get-var a) (- (get-num a) (get-num b))))
		 ((and (listp b) (member '* b) (member a b))
		  (if (member-if #'numberp (member '* b))
		      (simple-* a (simple-- 1 (third b)))
		      (simple-* a (simple-- 1 (first b)))))
		 ((and (listp a) (member '* a) (member b a))
		  (if (member-if #'numberp (member '* a))
		      (simple-* b (simple-- (third a) 1))
		      (simple-* b (simple-- (first a) 1)))))))

(defun simple-* (a b)
  (simple-infix a b *
		(((zerop a) 0) ((= a 1) b) (t `(,a * ,b)))
		(((zerop b) 0) ((= b 1) a) (t `(,a * ,b)))
		(((eql a b) `(,a ^ ,2))
		 ((and (listp b) (listp a)
		       (member '^ b) (member '^ a)
		       (eql (get-var a) (get-var b)))
		  (simple-^ (get-var a) (+ (get-num a) (get-num b))))
		 ((and (listp b) (member '^ b) (member a b))
		  (if (member-if #'numberp (member '^ b))
		      (simple-^ a (simple-+ (third b) 1))
		      (simple-^ a (simple-+ (first b) 1))))
		 ((and (listp a) (member '^ a) (member b a))
		  (if (member-if #'numberp (member '^ a))
		      (simple-^ b (simple-+ (third a) 1))
		      (simple-^ b (simple-+ (first a) 1)))))))

(defun simple-/ (a b)
  (simple-infix a b /
		(((zerop a) 0) (t `(,a / ,b)))
		((t `(,a / ,b)))))

(defun simple-^ (a b)
  (simple-infix a b ^
		(((zerop a) 0) ((= a 1) 1) (t `(,a ^ ,b)))
		(((zerop b) 1) ((= b 1) a) (t `(,a ^ ,b)))))

(defun simple-prefix (operator a)
  (cond ((numberp a) (funcall operator a))
	(t `(,operator ,a))))

(defun simple (operator a &optional (b nil))
  (cond ((eq operator '+) (simple-+ a b))
	((eq operator '-) (simple-- a b))
	((eq operator '*) (simple-* a b))
	((eq operator '/) (simple-/ a b))
	((eq operator '^) (simple-^ a b))
	(t (simple-prefix operator a))))

(defun simplify (expr)
  (dolist (pair op)
    (when (atom expr) (return expr))
    (when (null (rest expr)) (return (simplify (first expr))))
    (when (eql (first pair) (second expr))
      (return (simple (second expr) (simplify (first expr))
		      (simplify (third expr)))))))

;;; Derivative helper functions
(defun delta-atom (atom wrt)
  (if (eql atom wrt) 1 0))

(defun delta-operate (operator list wrt)
  (cond
    ((eql operator '+) (delta-+ list wrt))
    ((eql operator '-) (delta-- list wrt))
    ((eql operator '*) (delta-* list wrt))
    ((eql operator '/) (delta-/ list wrt))
    ((eql operator '^) (delta-^ list wrt))
    (t (dolist (pair delta-table)
	 (when (eql operator (first pair))
	   (return (simple-* (delta (second list) wrt)
			     (simplify (second pair)))))))))

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

;;; Derivative taker
(defun delta (expr wrt)
  (cond
    ((atom expr) (delta-atom expr wrt))
    ((null (rest expr)) (delta (first expr) wrt))
    ((eql (first expr) '-) (delta-- `(0 - ,(second expr)) wrt))
    (t (dolist (pair op)
	 (when (eql (first pair) (nth (second pair) expr))
	   (return (delta-operate (first pair) expr wrt)))))))
