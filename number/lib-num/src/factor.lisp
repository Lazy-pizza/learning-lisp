(in-package :lib-num)
;; ***** NUMBER-FACTOR *****

(defun make-random-lst (max n)
  (declare (integer n))
  (if (= 0 n) '()
      (cons (random max) (make-random-lst max (- n 1)))))

;; Function for Lucas-Lehmer Test
;; Check wheter M_p = 2^p-1 is prime or not

(defun Lucas-Lehmer-Test (n)
  (declare (integer n))
  (labels ((f (s M i)
	     (declare (integer s M i))
	     (if (= i (- n 2)) (zerop s)
		 (f (mod (- (* s s) 2) M) M (+ i 1)))))
    (f 4 (- (ash 1 n) 1) 0)))

;; Function for Miller-Rabin Test
;; For n < 3*10**(25), '(2 3 5 7 11 13 17 19 23 29 31 37 41) is the
;; sufficient witness for prove n is prime.
;; Generally we need to randomly take Rabin-Miller witness.

(defun Miller-Rabin-Test (xs n)
  (declare (integer n))
  (and (or (= n 2) (oddp n))
       (let* ((qs (labels ((q (n i)
			     (declare (integer n i))
			     (if (evenp n) (q (ash n -1) (+ i 1))
				 (cons n i))))
		    (q (- n 1) 0)))
	      (q (the integer (car qs)))
	      (s (the integer (cdr qs))))
	 (labels ((f (tmp i)
		    (declare (integer tmp i))
		    (and (not (= i s))
			 (or (= 1 tmp)
			     (= (- n 1) tmp)
			     (f (rem (* tmp tmp) n) (+ i 1)))))
		  (g (xs)
		    (or (null xs)
			(let ((head (car xs)))
			  (declare (integer head))
			  (and (> (the integer (rem n head)) 0)
			       (f (the integer (mod-nth-pow head q n)) 0)
			       (g (cdr xs)))))))
	   (g xs)))))

;; Function for Check wheter n is Lucas strong pseudo prime or not
(defun Lucas-Strong-Probable-Prime-test (n)
  (declare (integer n))
  (and
   (> n (let ((k (isqrt n))) (* k k)))
   (let* ((D (the integer
		  (car (stream-find-cond (lambda (x)
					   (declare (integer x))
					   (= -1 (jacobi-sym x n)))
					 *PSW-stream*))))
	  (q (the integer (ash (- 1 D) -2)))
	  (pra (labels ((f (n s)
			  (declare (integer n s))
			  (if (evenp n) (f (ash n -1) (+ 1 s))
			      (cons n s))))
		 (f (+ n 1) 0)))
	  (s (the integer (cdr pra))))
     (labels ((g (x)
		(declare (integer x))
		(ash (if (evenp x) x
			 (+ x n))
		     -1))
	      (LucasPair (u v k)
		(declare (integer u v k))
		(cond ((= k 1) (cons u v))
		      ((evenp k) (let* ((pr (LucasPair u v (ash k -1)))
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
		(declare (integer u v i))
		(if (= s i)
		    (or (= 1 (abs q))
			(and (= v (mod (* 2 q) n))
			     (= (mod-nth-pow q (/ (+ n 1) 2))
				(mod (* q (jacobi-sym q n)) n))))
		    (and (> s i)
			 (or (and (zerop i) (zerop u))
			     (zerop v)
			     (h (mod (* u v) n)
				(g (mod (+ (* v v) (* D u u)) n))
				(+ i 1)))))))
       (and (= (gcd (* q D) n) 1)
	    (let* ((prb (LucasPair 1 1 (car pra))))
	      (h (car prb) (cdr prb) 0)))))))

;; Baillie-PSW Test

(defun Baillie-PSW-Test (n)
  (declare (integer n))
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
  (declare (integer x0 k a n))
  (labels ((f (x)
	     (declare (integer x))
	     (the integer
		  (rem (+ a (stream-nth (make-mod-pow-two-stream x n) k))
		       n)))
	   (g (d tmp1 tmp2)
	     (declare (integer d tmp1 tmp2))
	     (if (> d 1) d
		 (let* ((x2 (f (f tmp1)))
			(x1 (f tmp2))
			(d (gcd (- x2 x1) n)))
		   (g d x2 x1)))))
    (g 1 x0 x0)))
	       

(defun full-factor (n)
  (declare (integer n))
  (cond ((= n 1) '())
	((Rabin-Miller-Test '(2 3 5 7 11 13 17 19 23 29 31 37 41) n)
	 (cons n '()))
	(t (let ((ans (pollard-rho-method 3 10 1 n)))
	     (cons ans (full-factor (/ n ans)))))))


;; factor prime into gauss prime
(defun gauss-prime-factor (p)
  (declare (integer p))
  (labels ((f (v M)
	     (declare (integer v M))
	     (if (< v (ash M -1)) v
		 (- v M)))
	   (g (u v M)
	     (declare (integer u v M))
	     (if (= M 1)
		 (cons (complex u v) (complex u (- v)))
		 (let* ((up (f (mod u M) M))
			(vp (f (mod v M) M))
			(upp (abs (/ (+ (* u up) (* v vp)) M)))
			(vpp (abs (/ (- (* u vp) (* v up)) M))))
		   (g upp vpp (/ (+ (* upp upp) (* vpp vpp)) p))))))
    (cond ((= p 2) (cons (complex 1 1) (complex 1 -1)))
	  ((= (mod p 4) 3) p)
	  (t (let* ((v (f (car (quad-mod-eqn-solver -1 p)) p)))
	       (g 1 v (/ (+ 1 (* v v)) p)))))))
		   
	

;; ***** NUMBER-FACTOR END *****
