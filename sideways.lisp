(defun readfile (filename)
  (let ((lines nil) (file (open filename)))
    (loop for line = (read-line file nil)
	  while line do
	  (cond ;; There must be a better way
	   ((null lines) (setf lines (list line)))
	   ((null (cdr lines)) (setf (cdr lines) (list line)))
	   (t (setf (cdr (last lines)) (list line)))))
    (close file)
    lines))

(defun maxlength (lines)
  (reduce #'max (mapcar #'(lambda (x) (length x)) lines)))

(defun sideways (lines)
  (if (= (length lines) 0)
      nil
    (loop for i from (- (maxlength lines) 1) downto 0 do
	  (loop for line in lines do
		(if (> (length line) i)
		    (princ (elt line i))
		  (princ " ")))
	  (format t "~%" nil)))) ;; TODO there must be a better way, but princ doesn't work...

(sideways (readfile (second *posix-argv*)))
