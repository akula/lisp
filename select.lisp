

(defvar *db* nil)

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
		(mapcar
		 #'(lambda (row)
			 (when (funcall selector-fn row)
			   (if title (setf (getf row :title ) title ))
			   (if artist (setf (getf row :artist) artist))
			   (if rating (setf (get row :rating ) rating ))
			   (if ripped-p (setf (get row :ripped-p) ripped-p)))
			 row)
		 *db*)))
