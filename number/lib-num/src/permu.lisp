;; ***** NUMBER-PERMU *****

;; decomp-permu
;; take permutation and decomp it
;; your permutation must be look like this
;; '( '(1 . 2) '(2 . 1) '(3 . 3))

(defun decomp-permu (lst)
  (let ((permu lst))
    (labels ((f (head p)
	       (let ((flag (assoc head p)))
		 (if flag (cons head (f (cdr flag)
					(setq permu (remove flag permu))))
		     '())))
	     (g (head)
	       (if (null permu) '()
		   (cons (f head permu) (g (car (car permu)))))))
      (g (car (car permu))))))

;; sgn function for permutation

(defun sgn-permu (permu)
  (labels ((f (lst)
	     (if (null lst) 1
		 (let ((head (car lst))
		       (tail (cdr lst)))
		   (if (evenp (length head))
		       (* -1 (f tail))
		       (* 1 (f tail)))))))
    (f (decomp-permu permu))))

;; ***** NUMBER PERMU END *****
