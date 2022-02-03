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
  (defparameter *path* (format nil "~a/~a" *repo-dir-on-host* *example-subdir*) )
  (write-source (format nil "~a/source/run01" *path*)
		`(do0
		  (comments "Simon Wood Generalized Additive Models, Ch7 GAMs in Practice, p. 325")
		  (library mgcv)
		  (setf sm (aref (smoothCon (s times :k 10)
					    :data mcycle :knots NULL)
				 (list 1))))))




