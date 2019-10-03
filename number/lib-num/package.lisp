(defpackage #:lib-num
  (:use #:cl)
  (:export #:*prime-stream*
           #:make-mod-pow-two-stream
	   #:make-lin-recurr-seq-stream
	   #:stream-nth
	   #:stream-to-lst
	   #:stream-until-k
	   #:stream-find-cond
	   #:stream-until-cond
	   #:rev-gcd
	   #:rev-gcd-lst
	   #:mutually-primep
	   #:convert-to-mutually-prime
	   #:mod-nth-pow
	   #:lin-mod-eqn-solver
	   #:CRT-Solver
	   #:Legendre-sym
	   #:quad-mod-eqn-solver
	   #:Rabin-Miller-Test
	   #:primep
	   #:pollard-rho-method
	   #:full-factor
	   #:decomp-permu
	   #:sgn-permu
	   #:gen-prime
	   #:gen-key
	   #:rsa-encode
	   #:rsa-decode))
	   
