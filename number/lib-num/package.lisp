(defpackage "lib-num"
  (:nicknames "libnum" "lib-num")
  (:use "COMMON-LISP")
  (:export "*prime-stream*"
	   "make-mod-pow-two-stream"
	   "make-lin-recurr-seq-stream"
	   "stream-to-lst"
	   "stream-until-k"
	   "rev-gcd"
	   "rev-gcd-lst"
	   "mutually-primep"
	   "convert-to-mutually-prime"
	   "mod-nth-pow"
	   "lin-mod-eqn-solver"
	   "CRT-Solver"
	   "Legendre-sym"
	   "Rabin-Miller-Test"
	   "pollard-rho-method"
	   "decomp-permu"
	   "sgn-permu"))
	   
