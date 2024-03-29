(defun collatz (n)
  (labels ((f (i tmp)
	     (cond ((= tmp 1) i)
		   ((evenp tmp) (f (+ i 1) (/ tmp 2)))
		   (t (f (+ i 1) (+ (* 3 tmp) 1))))))
    (f 1 n)))

(loop for n from 1 to 100000000
     do (collatz n))
