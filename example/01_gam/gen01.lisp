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
  (write-source (format nil "~a/source/run01" *path*)
		`(do0
		  (comments "Simon Wood Generalized Additive Models, Ch7 GAMs in Practice, p. 325")
		  #+nil (do0 (library mgcv)
		       (library MASS)
		       (setf sm (aref (smoothCon (s times :k 10)
						 :data mcycle :knots NULL)
				      (list 1))
			     beta (coef (lm (~ mcycle$accel (- sm$X 1)))))

		       ,(show "cycle"
			      `(do0
				(with mcycle (plot times accel))
				(do0 (setf times (seq 0 60 :length 200)
					   Xp (PredictMat sm (data.frame :times times)))
				     (lines times (%*% Xp beta))))))
		  (do0 (library mgcv)
		       (library gamair)
		       (setf sm (aref (smoothCon (s times :k 10)
						 :data mcycle :knots NULL)
				      (list 1))
			     beta (coef (lm (~ mcycle$accel (- sm$X 1)))))

		       ,(show "cycle"
			      `(do0
				(with mcycle (plot times accel))
				(do0 (setf times (seq 0 60 :length 200)
					   Xp (PredictMat sm (data.frame :times times)))
				     (lines times (%*% Xp beta))))))

		  )))




