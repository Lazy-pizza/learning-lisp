(in-package :lib-num)
;; ***** NUMBER-MOD *****


;; function for calculate modular power in O(lg n) arthimetic process
;; using the stream for modular power

(defun mod-nth-pow (a n p)
  (declare (integer a n p))
  (let ((r (mod a p)))
    (labels ((f (n)
	       (declare (integer n))
	       (cond ((zerop n) 1)
		     ((evenp n) (let ((k (f (ash n -1)))) (rem (* k k) p)))
		     (t (let ((k (f (- n 1)))) (rem (* r k) p))))))
      (f n))))

;; function for solve linear modulus equation
;; ex: ax-b \equiv 0 \pmod{n} => (a b n)
;; a must be not equal to zero

(defun lin-mod-eqn-solver (a b n)
  (declare (integer a b n))
  (let* ((sols (rev-gcd a n))
	 (d (the integer (caar sols)))
	 (x (the integer (car (cdr sols)))))
    (cond ((= d 1) (mod (* b x) n))
	  ((> (mod b d) 0) nil)
	  (t (let ((tmp (/ n d))
		   (tmp2 (/ b d)))
	       (declare (integer tmp tmp2))
	       (labels ((fun (i f)
			  (declare (integer i f))
			  (if (or (> i f) (= i f)) '()
			      (cons (mod (+ (* tmp2 x) (* i tmp)) n)
				    (fun (+ i 1) f)))))
		 (sort (fun 0 d) '<)))))))


;; CRT-Solver
;; Use Chinese Reminder Theorem to solve system of lin mod eqn.
;; Currently, it is only implemented in (m_i,m_j) = 1 case.
;;  x \equiv 2 \pmod 3 x \equiv 3 \pmod 5 => '(2 3) '(3 5)
;; result => (sol . M ) it means x \equiv sol \pmod M

(defun CRT-Solver (rems mods)
  (if (mutually-primep mods)
      (let ((M (the integer (reduce '* mods :initial-value 1))))
	(labels (( f (lst)
		   (if (null lst) '()
		       (let* ((head (the integer (car lst)))
			      (tail (cdr lst))
			      (Mi (the integer (/ M head)))
			      (xi (the integer (car (cdr (rev-gcd Mi head))))))
			 (cons (mod (* Mi xi) M) (f tail))))))
	  (cons (reduce (lambda (x y)
			  (declare (integer x y))
			  (mod (+ x y) M))
			(mapcar (lambda (x y)
				  (declare (integer x y))
				  (mod (* x y) M))
				rems (f mods))
			:initial-value 0)
		M)))
      (error "Currently, CRT-Solver only implemented in (m_i,m_j) = 1.")))

;; Legendre Symbol
;; p must be a prime
(defun Legendre-sym (n p)
  (declare (integer n p))
  (mod-nth-pow n (ash (- p 1) -1) p))

;; Jacobi Symbol
;; m must be odd
(defun Jacobi-sym (n m)
  (declare (integer n m))
  (labels ((helper (n m tmp)
	     (declare (integer n m tmp))
	     (cond ((evenp m) (error "m must be odd integer"))
		   ((not (= 1 (gcd n m))) 0)
		   ((< n 0) (helper (- n) m (if (= 1 (mod m 4))
						tmp
						(- tmp))))
		   ((= n 1) tmp)
		   ((evenp n) (helper (ash n -1) m
				      (if (or (= 1 (mod m 8)) (= 7 (mod m 8)))
					  tmp
					  (- tmp))))
		   ((< n m) (helper m n (if (or (= 1 (mod n 4)) (= 1 (mod m 4)))
					    tmp
					    (- tmp))))
		   (t (helper (mod n m) m tmp)))))
    (helper n m 1)))

;; use Tonelli-Shanks Algorithm to solve quadratic modular equation
;; only implemented on prime case

(defun quad-mod-eqn-solver (n p)
  (declare (integer n p))
  (labels ((q (n s)
	     (declare (integer n s))
	     (if (evenp n) (q (/ n 2) (+ s 1))
		 (cons n s)))
	   (find-non-quad (n)
	     (declare (integer n))
	     (if (= (- p 1) (Legendre-sym n p)) n
		 (find-non-quad (random (- p 1)))))
	   (f (M c h R)
	     (declare (integer M c h R))
	     (cond ((zerop h) 0)
		   ((= h 1) R)
		   (t (let* ((i (the integer
				     (cdr (stream-find-cond
					   (lambda (x) (= x 1))
					   (make-mod-pow-two-stream
					    h
					    p)))))
			     (b (the integer
				     (stream-nth (make-mod-pow-two-stream
						  c
						  p)
						 (- M i 1))))
			     (c (rem (* b b) p)))
			(f i c (rem (* h c) p) (rem (* R b) p)))))))
    (if (= (- p 1) (Legendre-sym n p)) nil
	(let* ((pr (q (- p 1) 0))
	       (head (the integer (car pr)))
	       (tail (cdr pr))
	       (solv (the integer
			  (f tail
			     (mod-nth-pow (find-non-quad 2) head p)
			     (mod-nth-pow n head p)
			     (mod-nth-pow n (ash (+ head 1) -1) p)))))
	  (if (= 0 solv) (list solv)
	      (list solv (- p solv)))))))
					  
	      
  


;; ***** NUMBER-MOD END *****
