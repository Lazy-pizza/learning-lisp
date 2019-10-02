(in-package :lib-num)
;; ***** NUMBER-STREAM ******

;; the stream for the prime number
;; Sieve of Eratosthenes
(defvar *prime-stream*
  (let ((prime-lst '()))
    (labels ((g (lst k)
	       (if (null lst) '()
		   (let ((head (car lst))
			 (tail (cdr lst)))
		     (if (> head k) '()
			 (cons head (g tail k))))))
	     (f (n)
	       (let* ((k (floor (sqrt n)))
		      (filtered-plst (remove-if (lambda (x)
						  (> (rem n x) 0))
						(g (reverse prime-lst) k))))
		 (if (null filtered-plst)
		     (cons (car (setq prime-lst (cons n prime-lst)))
			   (lambda () (f (+ n 1))))
		     (f (+ n 1))))))
      (lambda () (f 2)))))

;; function for generate a stream for modular power
;; a**(2**i) (mod p)

(defun make-mod-pow-two-stream (a p)
  "Make a**(2**i) (mod p) stream, use stream-to-lst to get elements"
  (labels ((f (n)
	     (let ((k (rem n p)))
	       (cons k (lambda () (f (* k k)))))))
    (lambda () (f (mod a p)))))

;; Make a stream for the sequence defined by linear recurrence relation
;; a_n = c_1a_{n-1} + c_2a_{n-2} + ... + c_ka_{n-k}
;; xs for the lst for inital data
;; example xs: '(a_1, a_2 , ... , a_k) cs: '(c_k, c_{k-1} , ... , c_1)
;; This stream starts to 1st element of {a_i}

(defun make-lin-recurr-seq-stream (xs cs)
  (if (not (= (length xs) (length cs)))
      (error "The length of inital data and the length of coefficients are must be equal")
      (labels ((f (i xs)
		 (if (< i (length xs)) (cons (nth i xs)
					     (lambda () (f (+ 1 i) xs)))
		     (let ((ans (reduce '+ (mapcar '* xs cs) :initial-value 0)))
		       (cons ans
			     (lambda () (f i
					   (reverse
					    (cons ans (reverse (cdr xs))))))))
			     )))
	(lambda () (f 0 xs)))))

;; functions for using stream

(defun stream-to-lst (s n)
  (let* ((pr (funcall s))
	 (v (car pr))
	 (s (cdr pr)))
    (if (= n 0) '()
	(cons v (stream-to-lst s (- n 1))))))

(defun stream-until-k (s k)
  (let* ((pr (funcall s))
	 (v (car pr))
	 (s (cdr pr)))
    (if (> v k) '()
	(cons v (stream-until-k s k)))))

(defun stream-find-cond (f s)
  (let* ((pr (funcall s))
	 (v (car pr))
	 (s (cdr pr)))
    (if (funcall f v) v
	(append '() (stream-find-cond f s)))))

(defun stream-until-cond (f s)
  (let* ((pr (funcall s))
	 (v (car pr))
	 (s (cdr pr)))
    (if (funcall f v) '()
	(cons v (stream-until-cond f s)))))

;; ***** NUMBER-STREAM END *****
