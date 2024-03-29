(in-package :map-grid-utils)

#+use-instead-of-grids-*default-grid-type*
(defparameter *array-type*
  #+clisp 'array
  #-clisp 'grid:foreign-array
  "Default grid array type

Can be one of 'grid:foreign-array or 'array")

#+use-instead-of-grids-*default-element-type*
(defparameter *default-element-type*
  #+clisp 'float
  #-clisp 'double-float
  "Default float type

Calls to the function are coerced to *default-element-type*

Valid values are 'single-float or 'double-float")


(defconstant *v0*
  (make-grid `((,*default-grid-type* 4) ,*default-element-type*)
	     :initial-contents '(0d0 1d0 2d0 3d0)))

(defconstant *v1*
  (make-grid `((,*default-grid-type* 4) ,*default-element-type*)
	     :initial-contents '(10d0 11d0 12d0 13d0)))

(defconstant *v2*
  (make-grid `((,*default-grid-type* 3) ,*default-element-type*)
	     :initial-contents '(0d0 1d0 2d0)))
