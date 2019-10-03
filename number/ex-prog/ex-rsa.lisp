(require 'asdf)
(asdf:load-system 'lib-num)

(import 'lib-num:gen-key)
(import 'lib-num:rsa-encode)
(import 'lib-num:rsa-decode)

(defun str-to-num (str)
  (let* ((cs (coerce (remove '#\' str) 'list)))
    (reduce (lambda (tmp x) (+ (* 100 tmp) (- (char-code x) 26)))
	    cs :initial-value 0)))

(defun num-to-str (num)
  (labels ((f (n)
	     (if (= n 0) '()
		 (multiple-value-bind (p r) (floor n 100)
		   (cons r (f p))))))
    (coerce
     (reduce (lambda (tmp x) (cons (code-char (+ x 26)) tmp))
	     (f num)
	     :initial-value '())
     'string)))


(defun main ()
  (let* ((keylst (gen-key 65537 200))
	 (e (car keylst))
	 (d (car (cdr keylst)))
	 (n (car (cdr (cdr keylst))))
	 (s (read)))
    (if (stringp s)
	(write (num-to-str (rsa-decode
			    (str-to-num
			     (num-to-str (rsa-encode (str-to-num s) e n)))
			    d
			    n)))
	(write "Error!"))))

(main)
