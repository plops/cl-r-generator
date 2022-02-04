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
		       (data brain)
		       (setf brain (aref brain (< 5e-3 brain$medFPQ) ""))
		       )
		  (do0
		   (do0 (comments "gaussian model is insufficient:")
			(setf m0 (gam (~ medFPQ (s Y X :k 100))
				      :data brain))
			(gam.check m0)
			(comments "plots show that variance increases with mean"))
		   (do0
		    (comments "fit beta to the variance var(y_i) propto mu_i^beta")
		    (setf e (residuals m0)
			  fv (fitted m0))
		    (lm (~ (log (^ e 2))
			   (log fv)))
		    (comments "beta is around 2, i.e variance increases with the square of the mean."
			      "suggests gamma distribution (with link log to ensure predicted FPQ values are positive)"
			      "- log transform (extreme)"
			      "- or 4th root transform (less extreme)"
			      )))
		  (do0
		   (setf m1 (gam (~ (^ medFPQ .25)
				    (s Y X :k 100))
				 :data brain))
		   (gam.check m1))
		  (do0
		   (setf m1 (gam (~ (^ medFPQ .25)
				    (s Y X :k 100))
				 :data brain))
		   (gam.check m1))
		  (do0
		   (setf m2 (gam (~ medFPQ (s Y X :k 100))
				 :data brain
				 :family (Gamma :link log)))
		   (gam.check m2))
		  (do0
		   (comments "biasedness on different scales, m1 underestimates")
		   ,@(loop for e in `((^ (fitted m1) 4)
				      (fitted m2)
				      brain$medFPQ)
			   collect
			   `(mean ,e)))
		  (do0
		   (comments "look more at m2")
		   (vis.gam m2 :plot.type (string "contour")
			    :too.far .03
			    :color (string "gray")
			    :n.grid 60
			    :zlim (c -1 2)))
		  (do0
		   (comments "interaction smooth effect, constructed so that functions of the form f(Y)+g(X) are excluded from its basis")
		   (setf tm1 (gam (~ medFPQ (+ (s Y :k 10 :bs (string "cr"))
					       (s X :k 10 :bs (string "cr"))
					       (ti X Y :k 10)))
				  :data brain
				  :family (Gamma :link log)))
		   (AIC m2 tm1)
		   (comments "note that penalty structure is different for m2 and tm1")
		   (summary tm1)
		   (anova tm1)
		   (comments "small p-value for ti indicates that ti is significantly non zero and interaction is needed")
		   )


		  )))




