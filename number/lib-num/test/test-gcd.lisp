(require :asdf)
(asdf:load-system 'lib-num)
(import 'lib-num:rev-gcd)
(import 'lib-num:rev-gcd-lst)


(defun test-rev-gcd (a b)
  (let ((d (gcd a b))
	(pr (rev-gcd a b)))
    (and (= d (car (car pr)))
	 (= d
	    (+ (* a (car (cdr pr)))
	       (* b (cdr (cdr pr))))))))

(defun test-rev-gcd-lst (xs)
  (let ((d (reduce #'gcd xs))
	(pr (rev-gcd-lst xs)))
    (and (= d (car (car pr)))
	 (= d
	    (reduce #'+ (mapcar #'* xs (cdr pr)))))))
