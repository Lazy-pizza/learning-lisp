(defsystem "lib-num"
  :description "lib-num: a simple lisp library for the number theorists"
  :version "0.0.2"
  :author "Lazy-pizza <https://github.com/Lazy-pizza>"
  :licence "Public Domain"
  :components ((:file "package")
	       (:module "src"
			:depends-on ("package")
			:serial t
			:components ((:file "stream")
				     (:file "gcd")
				     (:file "mod")
				     (:file "factor")
				     (:file "rsa")
				     (:file "permu")))))
