(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-r-generator"))

(in-package :cl-r-generator)

(progn
  
  (defparameter *repo-dir-on-host* "/home/martin/stage/cl-r-generator")
  (defparameter *repo-dir-on-github* "https://github.com/plops/cl-r-generator/tree/master/")
  (defparameter *example-subdir* "example/06_color")
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
		  (do0
		   (library colorscience)
		   (library ggplot))
		  (comments "load illuminant data and observer data")
		  (setf d65_data colorscience--illuminantD65
			cie1931_data colorscience--ciexyz31
			)
		  
		  ,@(loop for e in `(wavelength
				     x y z)
			  and e-i from 0
			  collect
			  `(setf 
			    ,e
			    (aref cie1931_data "" ,e-i)))
		  (comments "combine illuminant and chromaticity into single data frame")
		  (setf combined_data (data.frame :Wavelength wavelength
						  :X x
						  :Y y
						  :Z z)
			d65_data_merged (merge combined_data
					       d65_data
					       :by.x (string "Wavelength")
					       :by.y (string "wlnm")
					       :all.x TRUE)
			(aref d65_data_merged$intensity (is.na d65_data_merged$intensity)) 0)

		  (do0
		   (comments "plot illuminant and chromaticity")
		   ,(show "input"
			  `(+
			    (ggplot d65_data_merged)
			    ,@(loop for e in `((X) (Y) (Z) (intensity D65))
				    collect
				    (destructuring-bind (color &optional (y color)) e
					`(geom_line (aes :x Wavelength
							:y ,y :color
							(string ,color)))))
			    (scale_color_manual
			     :values (space c (paren (= (string "X")
							(string "red"))
						     (= (string "Y")
							(string "green"))
						     (= (string "Z")
							(string "blue"))
						     (= (string "D65")
							(string "purple"))
						     )))
			    (labs :x (string "Wavelength (nm)")
				  :y (string "Value")
				  :title (string "CIE 1931 Chromaticity Coordinates and D65 Illuminant vs. Wavelength")
				  :color (string "Data"))
			    (theme_minimal))))
		  )))




