;; This lisp file provides some streams and functions for number theorist.


;; structure for store result of rev gcd: inversed Euclid Algorithm

(defstruct gcdsol
  gcd
  sol
  )

;; the stream for the prime number
;; Sieve of Eratosthenes
(defvar *prime-stream*
  (let ((prime-lst '()))
    (labels ((g (lst k)
	       (declare (optimize speed) (fixnum k))
	       (if (null lst) '()
		   (let ((head (car lst))
			 (tail (cdr lst)))
		     (declare (fixnum head))
		     (if (> head k) '()
			 (cons head (g tail k))))))
	     (f (n)
	       (declare (optimize speed) (fixnum n))
	       (let* ((k (floor (sqrt n)))
		      (filtered-plst (remove-if (lambda (x)
						  (declare (fixnum x))
						  (> (rem n x) 0))
						(g (reverse prime-lst) k))))
		 (if (null filtered-plst)
		     (cons (car (setq prime-lst (cons n prime-lst)))
			   (lambda () (f (+ n 1))))
		     (f (+ n 1))))))
      (lambda () (f 2)))))

;; functions for using stream

(defun stream-to-lst (s n)
  (declare (optimize speed) (fixnum n))
  (let* ((pr (funcall s))
	 (v (car pr))
	 (s (cdr pr)))
    (if (= n 0) '()
	(cons v (stream-to-lst s (- n 1))))))


;; functions for calculate (x_1,...x_n) satisfying
;; a_1x_1 + a_2x_2 + ... + a_nx_n = d
;; and give d = gcd(a_1,...a_n)

(defun rev-gcd (a b)
  (declare (optimize speed) (integer a) (integer b))
  (labels ((f (a b s1 s2)
	     (cond ((< a b) (f b a s2 s1))
		   ((< b 0) (f a (- b) s1 (cons (- (car s2)) (- (cdr s2)))))
		   ((= b 0) (make-gcdsol :gcd a
					 :sol s1))
		   (t (multiple-value-bind (q r) (floor a b)
			(declare (integer q) (integer r))
					  (f b
					     r
					     s2
					     (cons (- (car s1) (* q (car s2)))
						   (- (cdr s1) (* q (cdr s2))))
					     ))))))
    (f a b (cons 1 0) (cons 0 1))))

(defun rev-gcd-lst (lst)
  (declare (optimize speed))
  (let ((sollst (reverse (reduce (lambda (tmp x)
				   (declare (integer x))
				   (if (null tmp)
				       (cons (rev-gcd 0 x) tmp)
				       (cons (rev-gcd (gcdsol-gcd (car tmp))
						      x)
					     tmp )))
				 lst
						      :initial-value '()))))
    (labels ((g (lst)
	       (if (null lst) '()
		   (let ((head (car lst))
			 (tail (cdr lst)))
		     (cons (* (reduce (lambda (tmp x)
					(* (car (gcdsol-sol x)) tmp))
				      tail
					:initial-value 1)
			      (cdr (gcdsol-sol head)))
			   (g tail))))))
	   (make-gcdsol
	    :gcd (gcdsol-gcd (car (last sollst)))
	    :sol (g sollst)))))


;; function for generate a stream for modular power
;; a**(2**i) (mod p)

(defun make-mod-pow-two-stream (a p)
  "Make a**(2**i) (mod p) stream, use stream-to-lst to get elements"
  (declare (optimize speed) (integer a) (integer p))
  (labels ((f (n)
	     (declare (optimize speed) (integer n))
	     (let ((k (rem n p)))
	       (cons k (lambda () (f (* k k)))))))
    (lambda () (f a))))

;; function for change n to the binary lst
;; first binary digit of n is the first element of the result
;; n must be a positive integer!
;; example n = 2 => 10 => '(0 1)

(defun n-to-binary-lst (n)
  (declare (optimize speed) (integer n))
  (if (= n 0) '()
      (multiple-value-bind (p r) (floor n 2)
	(declare (integer p) (integer r))
	(cons r (n-to-binary-lst p)))))

;; function for calculate modular power in O(lg n) arthimetic process
;; using the stream for modular power

(defun mod-nth-pow (a n m)
  (declare (optimize speed) (integer a) (integer n) (integer m))
  (let* ((digit-lst (n-to-binary-lst n))
	 (pow-two-mod-lst (stream-to-lst
			     (make-mod-pow-two-stream a m)
			     (length digit-lst)))
	 (rem-lst (mapcar (lambda (x y)
			    (declare (integer x) (integer y))
			    (* x y))
			  digit-lst pow-two-mod-lst)))
    (if (= 0 (car (last rem-lst))) 0
	(reduce (lambda (tmp x)
		  (declare (integer tmp) (integer x))
		  (rem (* x tmp) m))
		(remove 0 rem-lst)
		:initial-value 1))))

;; Function for Rabin-Miller Test
;; For n < 3*10**(25), '(2 3 5 7 11 13 17 19 23 29 31 37 41) is the
;; sufficient witness for prove n is prime.
;; Generally we need to randomly take Rabin-Miller witness.

(defun Rabin-Miller-Test (xs n)
  "xs is a list for Rabin-Miller witness"
  (declare (optimize speed) (integer n))
  (and (oddp n)
       (or (= n 2)
	   (let* ((digit-lst (n-to-binary-lst (- n 1)))
		  (digit-len (length digit-lst)))
	     (declare (fixnum digit-len))
	     (labels ((f (x)
	       (declare (optimize speed) (integer x))
	       (let ((pow-two-mod-lst (stream-to-lst
				      (make-mod-pow-two-stream x n)
				      digit-len)))
		 (labels ((fi (lst)
			    (and
			     (not (null lst))
			     (let* ((head (car lst))
				    (rem-lst (mapcar (lambda (x y)
						       (declare
							(integer x)
							(integer y))
						       (* x y))
						     lst
						     pow-two-mod-lst))
				    (r (if (= 0 (car (last rem-lst))) 0
					   (reduce (lambda (tmp x)
						     (declare
						      (integer tmp)
						      (integer x))
						     (rem (* x tmp) n))
						   (remove 0 rem-lst)
						   :initial-value 1)))
				    (p (= (- n 1) r)))
			       (declare (integer r) (integer head) (boolean p))
			       (or (and (= 1 head) (or p (= 1 r)))
				   (or p (fi (cdr lst))))))))
		   (fi digit-lst))))
		      (g (xs)
			(or (null xs)
			    (let ((head (car xs)))
			      (declare (integer head))
			      (and (> (rem n head) 0) (f head)
				   (g (cdr xs)))))))
	       (g xs))))))
      

