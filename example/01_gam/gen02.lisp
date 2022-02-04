(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-r-generator"))

(in-package :cl-r-generator)

(progn
  ;; the following code needs inverted readtable, otherwise symbols
  ;; and filenames may have the wrong case and everything breaks in
  ;; horrible ways
  (assert (eq :invert
	      (readtable-case *readtable*)))
  (defparameter *repo-dir-on-host* "/home/martin/stage/cl-r-generator")
  (defparameter *repo-dir-on-github* "https://github.com/plops/cl-r-generator/tree/master/")
  (defparameter *example-subdir* "example/01_gam")
  (defparameter *path* (format nil "~a/~a" *repo-dir-on-host* *example-subdir*))
  (let ((show-counter 1))
    (defun show (name code)
      (prog1
	  `(do0
	    (png (string ,(format nil "~2,'0d_~a.png" show-counter name)))
	    ,code
	    (dev.off))
	(incf show-counter))))
  (write-source (format nil "~a/source/run02_piecewise_linear" *path*)
		`(do0
		  (comments "Simon Wood Generalized Additive Models, Ch4 Introducing GAMs, p.165")
		  (do0
		   (require gamair)
		   (data engine)
		   (attach engine)
		   (defun tf (x xj j)
		     (comments "jth tent function from sequence of knots xj")
		     (setf dj (* xj 0)
			   (aref dj j) 1)
		     ($ (approx xj dj x) y))
		   (defun tf.X (x xj)
		     (comments "tent function basis matrix given data x and knot sequence xj")
		     (setf nk (length xj)
			   n (length x)
			   X (matrix NA n nk))
		     (for (j (slice 1 nk))
			  (setf (aref X "" j)
				(tf x xj j)))
		     X)
		   #+nil
		   (do0
		    (setf sj (seq (min size)
				  (max size)
				  :length 6)
			  X (tf.X size sj)
			  b (lm (~ wear (- X 1)))
			  s (seq (min size)
				 (max size)
				 :length 200)
			  Xp (tf.X s sj)
			  )
		    (plot size wear)
		    (lines s (%*% Xp (coef b))))
		   (do0
		    (defun prs.fit (y x xj sp)
		      (setf X (tf.X x xj)
			    D (diff (diag (length xj))
				    :differences 2)
			    X (rbind X (* (sqrt sp)
					  D))
			    y (c y (rep 0 (nrow D))))
		      (lm (~ y (- X 1))))
		    (do0 
		     (setf sj (seq (min size)
				   (max size)
				   :length 20)
			   sp 2
			   b (prs.fit wear
				      size sj sp)
			   )
		     (plot size wear)
		     (setf Xp (tf.X s sj))
		     (lines s (%*% Xp (coef b))))

		    )
		   ))))




