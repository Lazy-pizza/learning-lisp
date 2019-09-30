(in-package :lib-num)
;; ***** NUMBER-MOD-POW *****

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

;; ***** NUMBER-MOD-POW END *****
