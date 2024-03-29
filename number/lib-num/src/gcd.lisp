(in-package :lib-num)
;; ***** NUMBER-GCD *****

;; functions for calculate (x_1,...x_n) satisfying
;; a_1x_1 + a_2x_2 + ... + a_nx_n = d
;; and give d = gcd(a_1,...a_n)

(defun rev-gcd (a b)
  (declare (integer a b))
  (labels ((f (a b s1 s2)
	     (declare (integer a b))
	     (cond ((< a b) (f b a s2 s1))
		   ((< b 0) (f a (- b) s1 (cons (- (the integer (car s2)))
						(- (the integer (cdr s2))))))
		   ((zerop b) (cons (list a) s1))
		   (t (multiple-value-bind (q r) (floor a b)
			(declare (integer q r))
					  (f b
					     r
					     s2
					     (cons (- (the integer (car s1))
						      (the integer
							   (* q (car s2))))
						   (- (the integer (cdr s1))
						      (the integer
							   (* q (cdr s2)))))
					     ))))))
    (f a b (cons 1 0) (cons 0 1))))

(defun rev-gcd-lst (lst)
  (let ((sollst (reverse (reduce (lambda (tmp x)
				   (declare (integer x))
				   (if (null tmp)
				       (cons (rev-gcd 0 x) tmp)
				       (cons (rev-gcd (caar (car tmp))
						      x)
					     tmp )))
				 lst
						      :initial-value '()))))
    (labels ((g (lst)
	       (if (null lst) '()
		   (let ((head (car lst))
			 (tail (cdr lst)))
		     (cons (* (reduce (lambda (tmp x)
					(declare (integer tmp))
					(* (the integer (car (cdr x))) tmp))
				      tail
					:initial-value 1)
			      (cdr (cdr head)))
			   (g tail))))))
	   (cons (list (caar (car (last sollst)))) (g sollst)))))

;; mutually-primep

(defun mutually-primep (xs)
  (labels ((f (a lst)
	     (declare (integer a))
	     (null (remove-if (lambda (x)
				(declare (integer x))
				(= 1 (gcd a x)))
			      lst))))
    (or (null xs)
	(let ((head (the integer (car xs)))
	      (tail (cdr xs)))
	  (and (f head tail) (mutually-primep tail))))))

;; relative-prime-converter
;; take n integers and make them to be relative primes to each other

(defun convert-to-mutually-prime (xs)
  (labels ((f (lst a)
	     (declare (integer a))
	     (if (null lst) (list a)
		 (let* ((head (the integer (car lst)))
			(tail (cdr lst))
			(d (gcd a head)))
		   (declare (integer head d))
		   (cons (/ head d) (f tail (/ a d))))))
	   (g (lst xs)
	     (if (null xs) lst
		 (let ((head (car xs))
		       (tail (cdr xs)))
		   (g (f lst head) tail)))))
    (g '() xs)))

;; ***** NUMBER-GCD END *****
