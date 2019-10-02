(defpackage #:lib-num
  (:use #:cl)
  (:export #:*prime-stream*
           #:make-mod-pow-two-stream
	   #:make-lin-recurr-seq-stream
	   #:stream-to-lst
	   #:stream-until-k
	   #:stream-find-cond
	   #:stream-until-cond)
  (:export #:rev-gcd
	   #:rev-gcd-lst
	   #:mutually-primep
	   #:convert-to-mutually-prime)
  (:export #:mod-nth-pow
	   #:lin-mod-eqn-solver
	   #:CRT-Solver
	   #:Legendre-sym
	   #:quad-mod-eqn-solver)
  (:export #:Rabin-Miller-Test
	   #:pollard-rho-method
	   #:full-factor)
  (:export #:decomp-permu
	   #:sgn-permu))
	   
