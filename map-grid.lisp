
(in-package :map-grid-utils)

(export '(mapgrid mapg))

(define-test map-one-grid
  (assert-numerical-equal
   #(0d0 2d0 4d0 6d0)
   (map-one-grid (lambda (arg)
		   (* 2 arg))
		 *v0*)))

(defun map-one-grid (function grid)
  "Element-wise map FUNCTION over GRID, returning a grid

*default-grid-type* and *default-element-type* determine the result
 type

gmap specializes map-grid to use only the :element-function
keyword"
  (map-grid
   :source grid
   :element-function function))

(define-test map-several-grids
  (assert-numerical-equal
   #(10d0 12d0 14d0 16d0)
   (map-several-grids #'+ *v0* *v1*)))

(defun map-several-grids (function &rest grids)
  "Element-wise Map `function' over `grids'

*default-grid-type* and *default-float-type* determine the result type

gsmap specializes map-n-grids to use only
the :combination-function keyword"
  (let ((affis (mapcar #'grid::affi grids)))
    (assert (affis-congruent-p affis) ()
	    "Grids don't have congruent AFFI's")
    (map-n-grids
     :sources  (mapcar #'list grids affis)
     :combination-function #'(lambda (&rest args)
			       (apply function args))
     :destination-specification `((,*default-grid-type* ,@(dimensions (first grids)))
				  ,*default-element-type*))))

(defun mapgrid (function grid &rest more-grids)
  "Apply FUNCTION to successive sets of arguments in which one
argument is obtained from each GRID

The value returned is a grid of the results of successive calls to
FUNCTION"
  (if more-grids
      (apply #'map-several-grids function grid more-grids)
      (map-one-grid function grid)))


(define-test mapg
  (let (acc)
    (assert-numerical-equal
     *v0*
     (mapg #'(lambda (arg1 arg2)
	       (declare (ignore arg2))
	       (push arg1 acc))
	   *v0* *v1*))
    (assert-numerical-equal '(3 2 1 0) acc)))




(defgeneric mapg (function grid &rest more-grids)
  (:documentation
   "Apply function to successive sets of arguments in which one
argument is obtained from each grid.  The GRID argument is returned.

This map is used for the function's side effects.  It is modeled after
MAPC

We use MAP-N-GRIDS for mapping.  This will produce a result that we
discard.  I specify the result as a CL array of default element type
T")
  (:method ((function function) (vector vector)
	    &rest more-vectors)
    (let* ((vectors (cons vector more-vectors))
	   (affis (mapcar #'grid::affi vectors)))
      (assert (affis-congruent-p affis) ()
	      "Grids don't have congruent AFFI's")
      (map-n-grids :sources (mapcar #'list vectors affis)
		   :combination-function #'(lambda (&rest args)
					     (apply function args)
					     nil)
		   :destination-specification `((cl:array ,@(dimensions vector))
						t))
      vector)))
