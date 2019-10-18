(require 'asdf)
(asdf:load-system 'lib-num)

(import 'lib-num:gen-key)
(import 'lib-num:rsa-encode)
(import 'lib-num:rsa-decode)

(setf *random-state* (make-random-state t))

(defun str-to-num (str)
  (let* ((cs (coerce str 'list)))
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
  (let* ((keylst (gen-key 65537 100))
	 (e (car keylst))
	 (d (car (cdr keylst)))
	 (n (car (cdr (cdr keylst))))
	 (s (read))
	 (text nil))
    (if (stringp s)
	(or
	 (format t "Public keys: (~D,~D)~%" e n)
	 (format t "Private keys: (~D)~%" d)
	 (or (format t "Encoded plain text:~% ~S~%"
		      (setq text (num-to-str
				  (rsa-encode
				   (str-to-num s)
				   e
				   n))))
	      (format t "Decoded plain text:~% ~S"
		      (setq text (num-to-str
				  (rsa-decode
				   (str-to-num text)
				   d
				   n))))))		
	(write "Error!"))))

(main)
