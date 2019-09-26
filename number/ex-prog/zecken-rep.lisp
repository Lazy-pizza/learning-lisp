;; This is an example lisp program for Zeckendorf's representation

(require 'asdf)
(asdf:load-system "lib-num")

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

(prog ((a (read))) (return (if (numberp a)
			      (let ((b (zecken-rep a)))
				(pprint b))
			      (pprint "Alert! You should give a number")
			      )))
	       
	     
  


	     
    
		   
			     
      

