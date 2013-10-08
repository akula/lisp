(defmacro mywhen (condition &rest body)
  `(if ,condition (progn ,@body)))


(defun primep (number)
  (when (> number 1)
	(loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))


(defun next-prime (number)
  (loop for n from number when (primep n ) return n ))

(defun group (src n)
  (if (zerop n )(error "The Snd parameter should not be Zero")
	  (let ((rest (nthcdr src )))
		(if (consp rest)
		   
