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

;; the stream for the subsequent number
;; 1 2 3 4 5 ..
(defvar *subsequent-num-stream*
  (labels ((f (n)
	     (cons n (lambda () (f (+ n 1))))))
    (lambda () (f 1))))

;; function for generate a stream for modular power
;; a**(2**i) (mod p)

(defun make-mod-pow-two-stream (a p)
  "Make a**(2**i) (mod p) stream, use stream-to-lst to get elements"
  (labels ((f (n)
	     (let ((k (rem n p)))
	       (cons k (lambda () (f (* k k)))))))
    (lambda () (f a))))

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


;; functions for calculate (x_1,...x_n) satisfying
;; a_1x_1 + a_2x_2 + ... + a_nx_n = d
;; and give d = gcd(a_1,...a_n)

(defun rev-gcd (a b)
  (labels ((f (a b s1 s2)
	     (cond ((< a b) (f b a s2 s1))
		   ((< b 0) (f a (- b) s1 (cons (- (car s2)) (- (cdr s2)))))
		   ((= b 0) (make-gcdsol :gcd a
					 :sol s1))
		   (t (multiple-value-bind (q r) (floor a b)
					  (f b
					     r
					     s2
					     (cons (- (car s1) (* q (car s2)))
						   (- (cdr s1) (* q (cdr s2))))
					     ))))))
    (f a b (cons 1 0) (cons 0 1))))

(defun rev-gcd-lst (lst)
  (let ((sollst (reverse (reduce (lambda (tmp x)
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




;; function for change n to the binary lst
;; first binary digit of n is the first element of the result
;; n must be a positive integer!
;; example n = 2 => 10 => '(0 1)

(defun n-to-binary-lst (n)
  (if (= n 0) '()
      (multiple-value-bind (p r) (floor n 2)
	(cons r (n-to-binary-lst p)))))

;; function for calculate modular power in O(lg n) arthimetic process
;; using the stream for modular power

(defun mod-nth-pow (a n m)
  (let* ((digit-lst (n-to-binary-lst n))
	 (pow-two-mod-lst (stream-to-lst
			     (make-mod-pow-two-stream a m)
			     (length digit-lst)))
	 (rem-lst (mapcar '* digit-lst pow-two-mod-lst)))
    (if (= 0 (car (last rem-lst))) 0
	(reduce (lambda (tmp x)
		  (rem (* x tmp) m))
		(remove 0 rem-lst)
		:initial-value 1))))

;; function for solve linear modulus equation
;; ex: ax-b \equiv 0 \pmod{n} => (a b n)
;; a must be not equal to zero

(defun lin-mod-eqn-solver (a b n)
  (let* ((sols (rev-gcd a n))
	 (d (gcdsol-gcd sols))
	 (x (car (gcdsol-sol sols))))
    (cond ((= d 1) (mod (* b x) n))
	  ((> (mod b d) 0) nil)
	  (t (let ((tmp (/ n d))
		   (tmp2 (/ b d)))
	       (labels ((fun (i f)
			  (if (or (> i f) (= i f)) '()
			      (cons (mod (+ (* tmp2 x) (* i tmp)) n)
				    (fun (+ i 1) f)))))
		 (sort (fun 0 d) '<)))))))

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


;; This function make the Zeckendorf's representation for n
;; if n = u_{e_1} + .. + u_{e_r} where e_1 << e_2 << .. << e_r, {u_i} is
;; Fibonacci sequence
;; then it gives '(u_{e_1} u_{e_2} ... u_{e_r})

(defun zecken-rep (n)
  (let ((fibo_lst (reverse (stream-until-k
			    (make-lin-recurr-seq-stream '(1 1) '(1 1))
			    n))))
    (labels ((f (n lst)
	       (let ((head (car lst))
		     (tail (cdr lst)))
		 (cond ((= 0 n) '())
		       ((> 0 (- n head)) (f n tail))
		       (t (cons head (f (- n head) tail)))))))
      (reverse (f n fibo_lst)))))


;; Legendre Symbol
;; p must be a prime
(defun Legendre-sym (n p)
  (mod-nth-pow n (/ (- p 1) 2) p))


;; relative-prime-converter
;; take n integers and make them to be relative primes to each other

(defun convert-to-relative-prime (xs)
  (labels ((f (lst a)
	     (if (null lst) (list a)
		 (let* ((head (car lst))
			(tail (cdr lst))
			(d (gcd a head)))
		   (cons (/ head d) (f tail (/ a d))))))
	   (g (lst xs)
	     (if (null xs) lst
		 (let ((head (car xs))
		       (tail (cdr xs)))
		   (g (f lst head) tail)))))
    (g '() xs)))
	     
    
		   
			     
      

