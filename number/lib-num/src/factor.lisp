;; ***** NUMBER-FACTOR *****

;; Function for Rabin-Miller Test
;; For n < 3*10**(25), '(2 3 5 7 11 13 17 19 23 29 31 37 41) is the
;; sufficient witness for prove n is prime.
;; Generally we need to randomly take Rabin-Miller witness.

(defun Rabin-Miller-Test (xs n)
  "xs is a list for Rabin-Miller witness"
  (and (oddp n)
       (or (= n 2)
	   (let* ((digit-lst (n-to-binary-lst (- n 1)))
		  (digit-len (length digit-lst)))
	     (labels ((f (x)
	       (let ((pow-two-mod-lst (stream-to-lst
				      (make-mod-pow-two-stream x n)
				      digit-len)))
		 (labels ((fi (lst)
			    (and
			     (not (null lst))
			     (let* ((head (car lst))
				    (rem-lst (mapcar '*
						     lst
						     pow-two-mod-lst))
				    (r (if (= 0 (car (last rem-lst))) 0
					   (reduce (lambda (tmp x)
						     (rem (* x tmp) n))
						   (remove 0 rem-lst)
						   :initial-value 1)))
				    (p (= (- n 1) r)))
			       (or (and (= 1 head) (or p (= 1 r)))
				   (or p (fi (cdr lst))))))))
		   (fi digit-lst))))
		      (g (xs)
			(or (null xs)
			    (let ((head (car xs)))
			      (and (> (rem n head) 0) (f head)
				   (g (cdr xs)))))))
	       (g xs))))))

;; Function for prime factorization --  Pollard pho method
;; find a factor of integer n
;; for generality I use f(x) = x**(2**k) + a (mod n) to generate x_i

(defun pollard-rho-method (x0 k a n)
  (let ((cache (list x0)))
    (labels ((f (i)
	       (if (< (length cache) (+ i 1))
		   (let ((ans (rem (+ a (car (last (stream-to-lst
						    (make-mod-pow-two-stream
						     (f (- i 1)) n)
						    (+ k 1)))))
				   n)))
		     (setq cache (append cache (list ans)) ans ans))
		   (nth i cache)))
	     (g (i d)
	       (if (= 1 d)
		   (let ((d (gcd (- (f (* 2 i)) (f i)) n)))
		     (g (+ i 1) d))
		   d)))
      (g 1 1))))
		   
	

;; ***** NUMBER-FACTOR END *****
