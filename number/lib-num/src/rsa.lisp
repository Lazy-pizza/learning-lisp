(in-package :lib-num)

;; very very simple code for RSA crytosystem
;; I use sbcl's random function but you should use better one.

(defun primep (n)
  (declare (integer n))
  (Miller-Rabin-Test (make-random-lst (- n 1) 100) n))


(defun gen-prime (min max)
  (declare (integer min max))
	   (let ((n (random max)))
	     (cond ((< n min) (gen-prime min max))
		   ((primep n) n)
		   (t (gen-prime min max)))))

;; gen-key generate key
;; e,n are public key and d is private key
(defun gen-key (e size)
  (declare (integer e size))
  (let* ((p (gen-prime (ash 1 size) (ash 2 size)))
	 (q (gen-prime (ash 1 size) (ash 2 size)))
	 (n (* p q))
	 (l (/ (* (- p 1) (- q 1)) (gcd (- p 1) (- q 1))))
	 (d (mod (car (cdr (rev-gcd e l))) l)))
    (list e d n)))

;; encode message using public key
(defun rsa-encode (m e n)
  (declare (integer m e n))
  (mod-nth-pow m e n))

;; decode message using private key
(defun rsa-decode (c d n)
  (declare (integer c d n))
  (mod-nth-pow c d n))
