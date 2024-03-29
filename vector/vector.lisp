(defun vector-add (v1 v2)
  (map 'vector #'+ v1 v2))

(defun scalar-mult (a v)
  (map 'vector #'* (make-array (length v) :initial-element a) v))
  
(defun inner-prod (v1 v2)
  (reduce #'+ (map 'vector (lambda (x1 x2) (* x1 (conjugate x2))) v1 v2)))

(defun zero-vectorp (v)
  (reduce (lambda (tmp x) (and tmp (zerop x))) v))

;; gram-schmidt make orthogonal basis for V = span \{vs\}
(defun gram-schmidt (vs)
  (labels ((f (basis lst)
	     (if (null lst) basis
		 (let ((head (car lst))
		       (tail (cdr lst)))
		   (if (null basis) (f (if (zero-vectorp head)
					   basis
					   (append basis (list head)))
				       tail)
		       (let ((pj (reduce (lambda (tmp u)
					   (vector-add tmp
					       (scalar-mult
							(- (/ (inner-prod
							       head
							       u)
							      (inner-prod
							       u
							       u)))
							u)))
					 basis :initial-value head)))
			 (f (if (zero-vectorp pj)
				basis
				(append basis (list pj)))
			    tail)))))))
    (f '() vs)))

						
			      
	    



  
