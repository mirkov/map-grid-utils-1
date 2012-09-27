(in-package :map-grid-utils)


(define-test map-one-grid
  (assert-numerical-equal
   #+clisp #(0d0 2d0 4d0 6d0)
   #-clisp #m(0d0 2d0 4d0 6d0)
   (map-one-grid (lambda (arg)
		   (* 2 arg))
		 *v0*)))

(defun map-one-grid (function grid)
  "Element-wise map FUNCTION over GRID, returning a grid

*array-type* and float-type* determine the result type

gmap specializes map-grid to use only the :element-function
keyword"
  (map-grid
   :source grid
   :element-function function))

(define-test map-several-grids
  (assert-numerical-equal
   #+clisp #(10d0 12d0 14d0 16d0)
   #-clisp #m(10d0 12d0 14d0 16d0)
   (map-several-grids #'+ *v0* *v1*)))

(defun map-several-grids (function &rest grids)
  "Element-wise Map `function' over `grids'

*array-type* and float-type* determine the result type

gsmap specializes map-n-grids to use only
the :combination-function keyword"
  (let ((affis (mapcar #'grid::affi grids)))
    (assert (affis-congruent-p affis) ()
	    "Grids don't have congruent AFFI's")
    (map-n-grids
     :sources  (mapcar #'list grids affis)
     :combination-function #'(lambda (&rest args)
			       (apply function args))
     :destination-specification `((,*array-type* ,@(dimensions (first grids)))
				  ,*float-type*))))

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
	       (declare (ingore arg2))
	       (push arg1 acc))
	   *v0* *v1*))
    (assert-numerical-equal '(3 2 1 0) acc)))




(defgeneric mapg (function grid &rest more-grids)
  (:documentation
   "Apply function to successive sets of arguments in which one
argument is obtained from each grid.  The GRID argument is returned.

This map is used for the function's side effects.  It is modeled after
MAPC")
  (:method ((function function) (vector #+clisp vector #+sbcl mvector)
	    &rest more-vectors)
    (let* ((vectors (cons vector more-vectors))
	   (affis (mapcar #'grid::affi vectors)))
      (assert (affis-congruent-p affis) ()
	      "Grids don't have congruent AFFI's")
      (map-n-grids :sources (mapcar #'list vectors affis)
		   :combination-function #'(lambda (&rest args)
					     (apply function args)
					     nil)
		   :destination-specification `((array ,@(dimensions vector))
						t))
      vector)))