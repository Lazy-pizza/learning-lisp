(in-package :lib-num)

;; very very simple code for RSA crytosystem
;; I use sbcl's random function but you should use better one.

(defun gen-prime (min max)
	   (let ((n (random max)))
	     (cond ((< n min) (gen-prime min max))
		   ((primep n) n)
		   (t (gen-prime min max)))))

;; gen-key generate key
;; e,n are public key and d is private key
(defun gen-key (e size)
  (let* ((p (gen-prime (expt 2 size) (expt 2 (+ 1 size))))
	 (q (gen-prime (expt 2 size) (expt 2 (+ 1 size))))
	 (n (* p q))
	 (l (/ (* (- p 1) (- q 1)) (gcd (- p 1) (- q 1))))
	 (d (mod (car (cdr (rev-gcd e l))) l)))
    (list e d n)))

;; encode message using public key
(defun rsa-encode (m e n)
  (mod-nth-pow m e n))

;; decode message using private key
(defun rsa-decode (c d n)
  (mod-nth-pow c d n))
