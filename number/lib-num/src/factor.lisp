(in-package :lib-num)
;; ***** NUMBER-FACTOR *****

(defun make-random-lst (max n)
  (if (= 0 n) '()
      (cons (random max) (make-random-lst max (- n 1)))))

;; Function for Lucas-Lehmer Test
;; Check wheter M_p = 2^p-1 is prime or not

(defun Lucas-Lehmer-Test (n)
  (labels ((f (s M i)
	     (if (= i (- n 2)) (= s 0)
		 (f (mod (- (* s s) 2) M) M (+ i 1)))))
    (f 4 (- (expt 2 n) 1) 0)))

;; Function for Miller-Rabin Test
;; For n < 3*10**(25), '(2 3 5 7 11 13 17 19 23 29 31 37 41) is the
;; sufficient witness for prove n is prime.
;; Generally we need to randomly take Rabin-Miller witness.

(defun Miller-Rabin-Test (xs n)
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

;; Baillie-PSW Test

(defun Baillie-PSW-Test (n)
  (and
   (or (= n 2) (> (mod n 2) 0))
   (Miller-Rabin-Test '(2) n)
   (let* ((D (car (stream-find-cond (lambda (x) (= -1 (jacobi-sym x n)))
			       *PSW-stream*))))
     (labels ((f (n s)
		(if (evenp n) (f (/ n 2) (+ 1 s))
		    (cons n s)))
	      (uv (u v k)
		(cond ((= k 1) (cons u v))
		      ((evenp k) (let* ((pr (uv u v (/ k 2)))
					(u (mod (car pr) n))
					(v (mod (cdr pr) n))
					(w (+ (* v v) (* D u u))))
				   (cons (mod (* u v) n)
					 (mod (/ (if (evenp w) w
						     (+ w n))
						 2)
					      n))))
			  (t (let* ((pr (uv u v (- k 1)))
				    (u (mod (car pr) n))
				    (v (mod (cdr pr) n))
				    (w (+ u v))
				    (z (+ (* D u) v)))
			       (cons (mod (/ (if (evenp w) w
						 (+ w n))
					     2)
					  n)
				     (mod (/ (if (evenp z) z
						 (+ z n))
					     2)
					  n))))))
	      (g (u v i s)
		(and (> s i)
		     (or (and (zerop i) (zerop u))
			 (zerop v)
			 (let ((w (+ (* v v) (* d u u))))
			   (g (mod (* u v) n) (mod (/ (if (evenp w) w
							  (+ w n))
						      2)
						   n)
			      (+ i 1) s))))))
       (and (= 1 (gcd d n))
	    (let* ((pra (f (+ n 1) 0))
		   (s (cdr pra))
		   (prb (uv 1 1 (car pra))))
	      (g (car prb) (cdr prb) 0 s)))))))
	      
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
