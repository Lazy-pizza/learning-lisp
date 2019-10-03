(in-package :lib-num)
;; ***** NUMBER-FACTOR *****

(defun make-random-lst (max n)
  (if (= 0 n) '()
      (cons (random max) (make-random-lst max (- n 1)))))

;; Function for Rabin-Miller Test
;; For n < 3*10**(25), '(2 3 5 7 11 13 17 19 23 29 31 37 41) is the
;; sufficient witness for prove n is prime.
;; Generally we need to randomly take Rabin-Miller witness.

(defun Rabin-Miller-Test (xs n)
  (and (oddp n)
       (or (= n 2)
	   (let* ((qs (labels ((q (n i)
				 (if (evenp n) (q (/ n 2) (+ i 1))
				     (cons n i))))
			(q (- n 1) 0)))
		  (q (car qs))
		  (s (cdr qs)))
	     (labels ((f (tmp i)
			(and (not (= i s))
			     (or (= 1 tmp)
				 (= (- n 1) tmp)
				 (f (rem (* tmp tmp) n) (+ i 1)))))
		      (g (xs)
			(or (null xs)
			    (let ((head (car xs)))
			      (and (> (rem n head) 0)
				   (f (mod-nth-pow head q n) 0)
				   (g (cdr xs)))))))
	       (g xs))))))

(defun primep (n)
  (Rabin-Miller-Test (make-random-lst (- n 1) 100) n))

;; Function for prime factorization --  Pollard pho method
;; find a factor of integer n
;; for generality I use f(x) = x**(2**k) + a (mod n) to generate x_i

(defun pollard-rho-method (x0 k a n)
  (labels ((f (x)
	     (rem (+ a (stream-nth (make-mod-pow-two-stream x n) k))
		  n))
	   (g (d tmp1 tmp2)
	     (if (> d 1) d
		 (let* ((x2 (f (f tmp1)))
			(x1 (f tmp2))
			(d (gcd (- x2 x1) n)))
		   (g d x2 x1)))))
    (g 1 x0 x0)))
	       

(defun full-factor (n)
  (cond ((= n 1) '())
	((Rabin-Miller-Test '(2 3 5 7 11 13 17 19 23 29 31 37 41) n)
	 (cons n '()))
	(t (let ((ans (pollard-rho-method 3 10 1 n)))
	     (cons ans (full-factor (/ n ans)))))))
  
		   
	

;; ***** NUMBER-FACTOR END *****
