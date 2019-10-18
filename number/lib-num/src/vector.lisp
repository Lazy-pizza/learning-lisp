;; implement Vector on F_2

(defun make-vector-Z2 (xs)
  (labels ((f (xs)
	     (if (null xs) '()
		 (cons (mod (the integer (car xs)) 2) (f (cdr xs))))))
    (coerce (f xs) 'bit-vector)))

(defun add-vector-Z2 (v1 v2)
  (declare (bit-vector v1 v2))
  (map 'bit-vector #'logxor v1 v2))
  



