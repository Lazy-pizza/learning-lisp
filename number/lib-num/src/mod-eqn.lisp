;; ***** NUMBER-MOD-EQN *****

;; function for solve linear modulus equation
;; ex: ax-b \equiv 0 \pmod{n} => (a b n)
;; a must be not equal to zero

(defun lin-mod-eqn-solver (a b n)
  (let* ((sols (rev-gcd a n))
	 (d (caar sols))
	 (x (car (cdr sols))))
    (cond ((= d 1) (mod (* b x) n))
	  ((> (mod b d) 0) nil)
	  (t (let ((tmp (/ n d))
		   (tmp2 (/ b d)))
	       (labels ((fun (i f)
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
      (let ((M (reduce '* mods :initial-value 1)))
	(labels (( f (lst)
		   (if (null lst) '()
		       (let* ((head (car lst))
			      (tail (cdr lst))
			      (Mi (/ M head))
			      (xi (car (cdr (rev-gcd Mi head)))))
			 (cons (mod (* Mi xi) M) (f tail))))))
	  (cons (reduce (lambda (x y) (mod (+ x y) M))
			(mapcar (lambda (x y) (mod (* x y) M)) rems (f mods))
			:initial-value 0)
		M)))
      (error "Currently, CRT-Solver only implemented in (m_i,m_j) = 1.")))

;; Legendre Symbol
;; p must be a prime
(defun Legendre-sym (n p)
  (mod-nth-pow n (/ (- p 1) 2) p))

;; ***** NUMBER-MOD-EQN END *****
