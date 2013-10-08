(defun longer (x y)
  (labels ((compare (x y)
		    (and (consp x)
			 (or ( null y)
			     (compare (cdr x ) ( cdr y))))))))
 

(defun filter (fn lst)
  (let (( acc nil))
    (dolit (x lst)
	   (let ((val (funcall fn x)))
	     (if val (push val acc))))
    (nreverse acc)))


(defun group (source n)
  (if (zerop n ) (error "zero length"))
  (labels ( (rec (source acc)
		(let ((rest (nthcdr n souce)))
		  (if (consp rest)
		      (rec rest (cons (subseq source 0 n) acc))
		    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))




  
		    
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr ))
      x))


(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n)
		  (compr next 1 (cdr lst)))))))


(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))



(defun bin-serach (obj vec)
  (let (( len (length vec)))
    (and (not (zerop len))
	 (finder obj vec 0 (- len 1)))))


(defun finder (obj vec start end)
  (progn
    (format t "~A~%" (subseq vec start (+ end 1)))
   (let (( range (- end start)))
    (if (zerop range)
	(if (eql obj (aref vec start))
	    obj
	    nil)
	(let ((mid (+ start (round (/ range 2 )))))
	  (let (( obj2 (aref vec mid)))
	    (if ( < obj obj2)
		(finder obj vec start (- mid 1))
		(if ( > obj obj2)
		    (finder obj vec (+ mid 1) end)
		    obj))))))))




