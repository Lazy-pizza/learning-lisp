(defpackage #:lib-num
  (:use #:cl)
  (:export #:*prime-stream*
           #:make-mod-pow-two-stream
	   #:make-lin-recurr-seq-stream
	   #:stream-nth
	   #:stream-to-lst
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
	   #:Jacobi-sym
	   #:quad-mod-eqn-solver
	   #:Lucas-Lehmer-Test
	   #:Miller-Rabin-Test
	   #:Lucas-Strong-Probable-Prime-Test
	   #:Baillie-PSW-Test
	   #:primep
	   #:pollard-rho-method
	   #:full-factor
	   #:decomp-permu
	   #:sgn-permu
	   #:gen-prime
	   #:gen-key
	   #:rsa-encode
	   #:rsa-decode))
	   
