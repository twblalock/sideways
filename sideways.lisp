(let ((file (open (second *posix-argv*))))
  (let ((maxlen 0))
    (let ((lines nil))
      (loop for line = (read-line file nil)
	    while line do
	    (setf maxlen (max maxlen (length line)))
	    (cond ;; There must be a better way
	     ((null lines) (setf lines (list line)))
	     ((null (cdr lines)) (setf (cdr lines) (list line)))
	     (t (setf (cdr (last lines)) (list line)))))
      (close file)
      (loop while (>= maxlen 0) do
	    (loop for line in lines do
		  (if (> (length line) maxlen)
		      (princ (elt line maxlen))
		    (princ " ")))
	    (format t "~%" nil) ;; TODO there must be a better way, but princ doesn't work...
	    (decf maxlen)))))
