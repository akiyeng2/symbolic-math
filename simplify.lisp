;;;; Functions for handling symbolic manipulation

(defconstant e (exp 1) "Euler's Number")  
(defconstant i #C(0 1) "Imaginary Number")
(defun ^ (a b) (expt a b))
(defparameter *ops* nil)

(defun flatten (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

(defmacro defsimple ((operator &rest aliases) (&key (infix nil) (prefix nil)) (a &optional b) &body body)
  (let ((name (gensym)))
    (pushnew `(,operator ,name) *ops*)
    (dolist (alias aliases)
      (pushnew `(,alias ,name) *ops*))
    (cond (infix `(defun ,name (,a ,b)
		    (cond ((and (numberp ,a) (numberp ,b))
			   (,operator ,a ,b))
			  ,@body
			  (t `(,a ,',operator ,b)))))
	  (prefix `(defun ,name (,a)
		     (cond ,@body
			   (t `(,',operator ,a))))))))

(defmacro do-infix ((var1 var2) (var3 var4) &body body)
  `(dolist (,var1 '(0 2))
     (dolist (,var2 '(0 2))
       (let ((,var3 (- 2 ,var1)) (,var4 (- 2 ,var2)))
	 ,@body))))

(defmacro let-infix ((left right list) &body body)
  `(let ((,left (first ,list)) (,right (third ,list)))
     ,@body))

(defun greater-order (operator)
  (cond ((or (eql operator '+) (eql operator '-)) '*)
	((or (eql operator '*) (eql operator '/)) '^)))

(defun like-order (operator)
  (cond ((or (eql operator '+) (eql operator '-)) '+)
	((or (eql operator '*) (eql operator '/)) '*)))

(defun lower-order (operator)
  (cond ((or (eql operator '+) (eql operator '-)) '+)
	((or (eql operator '*) (eql operator '/)) '+)
	((or (eql operator '^) '*))))

(defun base-order (operator)
  (cond ((or (eql operator '+) (eql operator '*)) '+)
	((or (eql operator '-) (eql operator '/)) '-)))

(defun exprp (object)
  (or (listp object) (symbolp object)))

(defun negative (object)
  (simple '* -1 object))

(defun plusify (list)
  (simple '+ (first list) (negative (third list))))

(defun operator-memberp (arg1 op1 &optional (arg2 t) (op2 t))
  (and (listp arg1) (member op1 arg1) (listp arg2) (member op2 arg2)))

(defun combine-like (arg1 arg2 op)
  (let ((same-op (like-order op)))
    (do-infix (k j) (p q)
      (when (and (exprp (nth k arg1)) (exprp (nth j arg2)))
	(return-from combine-like
	  (simple same-op
		  (simple op (nth k arg1) (nth j arg2))
		  (simple op (nth p arg1) (nth q arg2))))))))

(defun combine-higher (arg1 arg2 op)
  (let ((big-op (greater-order op)) (base-op (base-order op)))
    (do-infix (k j) (p q)
      (when (and (exprp (nth k arg1)) (exprp (nth j arg2)))
	(return-from combine-higher
	  (simple big-op (nth k arg1)
		  (simple base-op (nth p arg1) (nth q arg2))))))))

(defun combine-hybrid (arg1 arg2 op)
  (let ((big-op (greater-order op)))
    (do-infix (k j) (p q)
      (when (and (exprp (nth k arg1)) (exprp (nth j arg2)))
	(return-from combine-hybrid
	  (simple op (simple op arg1 (nth j arg2))
		  (nth q arg2)))))))

(defun combine-hybrid-higher (arg1 arg2 op)
  (let ((big-op (greater-order op)))
    (do-infix (k j) (p q)
      (when (and (exprp (nth k arg1)))
	(return-from combine-hybrid-higher
	  (simple op (simple op arg1 (nth j arg2))
		  (nth q arg2)))))))

(defun combine-hybrid-atom (arg1 arg2 op)
  (let ((same-op (like-order op)))
    (do-infix (k j) (p q)
      (when (or (and (symbolp arg2) (exprp (nth k arg1)))
		(and (numberp arg2) (numberp (nth k arg1))))
	(return-from combine-hybrid-atom
	  (simple same-op (simple op (nth k arg1) arg2)
		  (nth p arg1)))))))

(defun identical (arg1 arg2)
  (or (equal arg1 arg2)
      (and (listp arg1) (listp arg2)
	   (not (set-difference (flatten arg1) (flatten arg2))))))

(defsimple (+) (:infix t) (a b)
  ((equalp a 0) b) ((equalp b 0) a)
  ((identical a b) `(,2 * ,a))
  ((and (listp b) (eql (first b) '-))
   (simple '- a (second b)))
  ((operator-memberp a '+ b '+)
   (combine-like a b '+))
  ((operator-memberp a '+ b '-)
   (simple '+ a (plusify b)))
  ((operator-memberp a '- b '+)
   (simple '+ b a))			; Reverse order.
  ((operator-memberp a '- b '-)
   (simple '+ (plusify a) (plusify b))) ; Cheat.
  ((operator-memberp a '* b '*)
   (combine-higher a b '+))
  ((operator-memberp a '* b '+)
   (combine-hybrid a b '+))
  ((operator-memberp a '+ b '*)
   (simple '+ b a))			; Reverse order.
  ((operator-memberp a '* b '-)
   (simple '+ a (plusify b)))
  ((operator-memberp a '- b '*)
   (simple '+ b a))
  ((operator-memberp a '+)
   (combine-hybrid-atom a b '+))
  ((operator-memberp b '+)
   (simple '+ b a)))

(defsimple (-) (:infix t) (a b)
  ((equalp a 0) (negative b)) ((equalp b 0) a)
  ((identical a b) 0)
  ((and (listp b) (eql (first b) '-))
   (simple '+ a (second b)))
  ((operator-memberp a '+ b '+)
   (combine-like a b '-))
  ((operator-memberp a '+ b '-)
   (simple '- a (plusify b)))
  ((operator-memberp a '- b '+)
   (negative (simple-- b a)))		; Reverse order.
  ((operator-memberp a '- b '-)
   (simple '- (plusify a) (plusify b))) ; Cheat.
  ((operator-memberp a '* b '*)
   (combine-higher a b '-))
  ((operator-memberp a '* b '+)
   (combine-hybrid a b '-))
  ((operator-memberp a '+ b '*)
   (negative (simple '- b a)))		; Reverse order.
  ((operator-memberp a '-)
   (combine-hybrid-atom a b '-))
  ((operator-memberp b '-)
   (negative (simple '- b a))))

(defsimple (*) (:infix t) (a b)
  ((equalp a 0) 0) ((equalp b 0) 0) 
  ((equalp a 1) b) ((equalp b 1) a)
  ((identical a b) `(,a ^ ,2))
  ((operator-memberp a '^ b '^)
   (combine-higher a b '*))
  ((operator-memberp a '^ b '*)
   (combine-hybrid a b '*))
  ((operator-memberp a '^)
   (combine-hybrid-atom a b '*))
  ((operator-memberp b '^)
   (simple '* b a)))

(defsimple (/) (:infix t) (a b)
  ((equalp a 0) 0) ((equalp b 0) `(,a / ,0))
  ((identical a b) 1))

(defsimple (^) (:infix t) (a b)
  ((equalp a 0) 0) ((equalp b 0) 1)
  ((equalp a 1) 1) ((equalp b 1) a))

(defsimple (log ln) (:prefix t) (a)
  ((or (eql a 'e) (equalp a e)) 1))

(defsimple (sin) (:prefix t) (a))
(defsimple (cos) (:prefix t) (a))
(defsimple (tan) (:prefix t) (a))
(defsimple (asin) (:prefix t) (a))
(defsimple (acos) (:prefix t) (a))
(defsimple (atan) (:prefix t) (a))
(defsimple (sinh) (:prefix t) (a))
(defsimple (cosh) (:prefix t) (a))
(defsimple (asinh) (:prefix t) (a))
(defsimple (acosh) (:prefix t) (a))
(defsimple (atanh) (:prefix t) (a))
 
(defun simple (operator &rest args)
  (dolist (pair *ops*)
    (when (eql operator (first pair))
      (return (apply (second pair) args)))))

(defun simplify (expr)
  (cond ((atom expr) expr)
	((null (rest expr)) (simplify (first expr)))
	((eql (first expr) '-) (simple '* -1 (second expr)))
	((= (length expr) 3)
	 (simple (second expr) (simplify (first expr)) (simplify (third expr)))) 
	((= (length expr) 2)
	 (simple (first expr) (simplify (second expr))))))
