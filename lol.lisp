(defparameter *max-label-length* 30)

(defun dot-name (exp)
  (substitute #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defun dot-label(exp)
  (if exp
	  (let (( s (write-to-string exp :pretty nil)))
	  (if (> (length s) *max-label-length*)
		  (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "..." )
		  s))
""))



(defun nodes->dot (nodes)
  (mapc (lambda (node)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "[label=\"")
		  (princ (dot-label node))
		  (princ "\"];"))
		nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
		  (mapc (lambda (edge)
				  (fresh-line)
				  (princ (dot-name (car node)))
				  (princ "->")
				  (princ (dot-name (car edge)))
				  (princ "[label=\"")
				  (princ (dot-label (cdr edge)))
				  (princ "\"];"))
				(cdr node)))
		edges))


(defun dot->png (fname thunk)
  (with-open-file (*stand-output*
				   fname
				   :direction :output
				   :if-exists :supersede)
	(funcall thunk))
  (ext:shell (concatename 'string "dot-Tpng -O " fname )))





;;;;;; orc battle game


(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "you have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))




   (defun game-loop ()
     (unless (or (player-dead)(monsters-dead))
       (show-player)
       (dotimes (k (1+ (truncate (/ (max 0 *paly-agility*) 15))))
	 (unless (monster-dead)
	   (show-monsters)
	   (player-attack)))
       (fresh-line)
       (map 'list
	    (lambda (m)
	      (or (master-dead m)(monster-attact m)))
	    *monsters*)
       (game-loop)))
		 
