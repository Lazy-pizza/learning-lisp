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
  (and (or (= n 2) (oddp n))
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
	   (g xs)))))

;; Function for Check wheter n is Lucas strong pseudo prime or not
(defun Lucas-Strong-Probable-Prime-test (n)
  (and
   (> n (let ((k (isqrt n))) (* k k)))
   (let* ((D (car (stream-find-cond (lambda (x) (= -1 (jacobi-sym x n)))
				    *PSW-stream*)))
	  (pra (labels ((f (n s)
			  (if (evenp n) (f (/ n 2) (+ 1 s))
			      (cons n s))))
		 (f (+ n 1) 0)))
	  (s (cdr pra)))
     (labels ((g (x) (/ (if (evenp x) x
			    (+ x n))
			2))
	      (LucasPair (u v k)
		(cond ((= k 1) (cons u v))
		      ((evenp k) (let* ((pr (LucasPair u v (/ k 2)))
					(u (mod (car pr) n))
					(v (mod (cdr pr) n)))
				   (cons (mod (* u v) n)
					 (g (mod (+ (* v v) (* D u u)) n)))))
		      (t (let* ((pr (LucasPair u v (- k 1)))
				(u (mod (car pr) n))
				(v (mod (cdr pr) n)))
			   (cons (g (mod (+ u v) n)) 
				 (g (mod (+ (* D u) v) n))))))) 
	      (h (u v i)
		(and (> s i)
		     (or (and (zerop i) (zerop u))
			 (zerop v)
			 (h (mod (* u v) n)
			    (g (mod (+ (* v v) (* D u u)) n))
			    (+ i 1))))))
       (and (= (gcd (* (/ (- 1 D) 4) D) n) 1)
	    (let* ((prb (LucasPair 1 1 (car pra))))
	      (h (car prb) (cdr prb) 0)))))))

;; Baillie-PSW Test

(defun Baillie-PSW-Test (n)
  (labels ((f (xs)
	     (or (null xs)
		 (and (> (mod n (car xs)) 0)
		      (f (cdr xs))))))
    (if (member n '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))
	t
	(and
	 (f '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))
	 (Miller-Rabin-Test '(2) n)
	 (Lucas-Strong-Probable-Prime-Test n)))))

	      
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
