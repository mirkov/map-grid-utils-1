(in-package #:map-grid-utils)

(export '(mapgrid-ma mapgrid-ma-1 mapgrid-man mapgrid-man-1))


(define-test mcurry
 (assert-expands
  '(lambda (arg-x) (pow arg-x arg-y arg-z))
  (mcurry pow @!arg-x arg-y arg-z))
 (assert-expands
  '(lambda (arg-y) (pow arg-x arg-y arg-z))
  (mcurry pow arg-x @!arg-y arg-z))
 (assert-expands
  '(lambda (arg-z) (pow arg-x arg-y arg-z))
  (mcurry pow arg-x arg-y @!arg-z)))
                   
(defmacro mcurry (fun &rest args)
  "Create lambda expression of one argument.  The argument is derived
from the only marked arg.  The function calls fun on all the args.

 (mcurry pow x @!y z)  expands into
                |
          +-----+---+
          |         |
 (lambda (y) (pow x y z))"
  (let (clean-args marked-arg)
    (mapc #'(lambda (arg)
	      (push (if (@!-symbol-p arg)
			(progn
			  (and marked-arg
			       (error "Found more than one marked symbol"))
			  (let ((cleaned-sym
				 (intern (subseq (symbol-name arg) 2))))
			    (setf marked-arg cleaned-sym)
			    cleaned-sym))
			arg)
		    clean-args))
	  args)
    `(lambda (,marked-arg)
       (,fun ,@(nreverse clean-args)))))

#+obsolete--gcmap-not-used-anymore(define-test gcmap
    (assert-expands 
     '(gmap (mcurry pow @!x y) vector)
     (gcmap (pow @!x y) vector)))

(defmacro mmm ((function &rest args) vector)
  "Map a function over arguments, all scalar, except one that is a
  vector.  Return a vector

FUNCTION - a function
ARGS - symbol list of arguments. One of the symbols must start with @!
VECTOR - an expression that evaluates into a vector

During the macro expansion, the @!-tagged argument will be
mapped-over.  For example, a call (gcmap (fun x y @!q z) vector)
expands into

 (gmap #'(lambda (q) (fun x y q z)) vector)
"
  `(gmap (mcurry ,function ,@args) ,vector))

(defun pow (x y)
  (expt x y))

#+obsolete--pow-curry-not-used-anymore
(define-test pow-curry
  ;; Loop over first variable
  (let ((y 2))
    ;; Sanity check for explicit mapping
    (assert-numerical-equal
     #(0 1 4)
     (copy-to (gmap #'(lambda (x)
			(pow x y)) (indgen 3))))
    ;; Explicit vs. curry notation
    (assert-numerical-equal
     (copy-to (gmap #'(lambda (x)
			(pow x y)) (indgen 3)))
     (copy-to (gcmap (pow @!x y) (indgen 3)))))
  ;; Loop over second variable
  (let ((x 2))
    ;; Sanity check for explicit mapping
    (assert-numerical-equal
     #(1 2 4 8)
     (copy-to (gmap #'(lambda (y)
			(pow x y)) (indgen 4))))
    ;; Explicit vs. curry notation
    (assert-numerical-equal
     (copy-to (gmap #'(lambda (y)
			(pow x y)) (indgen 4)))
     (copy-to (gcmap (pow x @!y) (indgen 4))))))


(define-test mapgrid-ma
  (assert-expands
   '(MAP-N-GRIDS :SOURCES (LIST (LIST X NIL) (LIST Y NIL)) :COMBINATION-FUNCTION
     (LAMBDA (X% Y%) (FUNCALL #'EXPT X% Y%)))
   (mapgrid-ma #'expt @!x @!y))
  (assert-numerical-equal
   (mapgrid #'expt *v0* *v1*)
   (mapgrid-ma #'expt @!*v0* @!*v1*))
  (assert-expands
   '(MAP-N-GRIDS :SOURCES (LIST (LIST X NIL) (LIST Z NIL)) :COMBINATION-FUNCTION
     (LAMBDA (X% Z%) (FUNCALL #'EXPT X% Y Z%)))
   (mapgrid-ma #'expt @!x y @!z))
#|  (assert-expands
   '(MAP-N-GRIDS :SOURCES (LIST (LIST (lseq 1 2 2) NIL) (LIST (lseq 1 3 2) NIL))
     :COMBINATION-FUNCTION (LAMBDA (X Y) (FUNCALL #'EXPT X Y)))
   (mapgrid-1 (expt @!x @!y) (lseq 1 2 2) (lseq 1 3 2)))
  (assert-expands
   '(MAP-N-GRIDS :SOURCES (LIST (LIST (lseq 1 2 2) NIL) (LIST (lseq 1 3 2) NIL))
     :COMBINATION-FUNCTION (LAMBDA (X Y) (FUNCALL #'FOO X Y Z)))
   (mapgrid-1 (foo @!x @!y z) (lseq 1 2 2) (lseq 1 3 2)))
  (assert-expands
   '(MAP-N-GRIDS :SOURCES (LIST (LIST (lseq 1 2 2) NIL) (LIST (lseq 1 3 2) NIL))
     :COMBINATION-FUNCTION (LAMBDA (X Y) (FUNCALL #'FOO Z X Y)))
   (mapgrid-1 (foo z @!x @!y) (lseq 1 2 2) (lseq 1 3 2)))
  (assert-expands
   '(MAP-N-GRIDS :SOURCES (LIST (LIST (lseq 1 2 2) NIL) (LIST (lseq 1 3 2) NIL))
     :COMBINATION-FUNCTION (LAMBDA (X Y) (FUNCALL #'FOO X Z Y)))
   (mapgrid-1 (foo @!x z @!y) (lseq 1 2 2) (lseq 1 3 2)))|#)

(defmacro mapgrid-ma (function &rest mixed-arguments)
  "Map FUNCTION over it's MIXED-ARGUMENTS, some of which are vectorss.

MIXED-ARGUMENTS is a mixture of scalars and grid vectors.
MIXED-ARGUMENTS cannot contain s-expressions.

The FUNCTION call arguments are build from MIXED-ARGUMENTS as follows:
- If an element of MIXED-ARGUMENTS is a scalar, it is used in function arguments
- If an element of MIXED-ARGUMENTS is a vector, its successive elements are used in 
  function arguments

The macro does not do any analysis of MIXED-ARGUMENT elements.  Vector
elements of MIXED-ARGUMENTS must be flagged by prepending @! to their
name.

The vectors can be CL arrays or foreign-arrays

For example, A call to (mapgrid-xa2 #'fun @!vx y @!vz)

expands into
 (map-n-grids :sources (list (list vx nil) (list vz nil))
              :combination-function #'(lambda (vx% vz%)
                                           (fun vx% y vz%)
"
  (let (dummy-args
	vector-args
	cleaned-arg-list)
    (mapc #'(lambda (arg)
	      (if (@!-symbol-p arg)
		  (let* ((cleaned-sym
			  (intern (subseq (symbol-name arg) 2)))
			 (dummy-sym (symbolicate cleaned-sym "%")))
		    (push cleaned-sym vector-args)
		    (push dummy-sym cleaned-arg-list)
		    (push dummy-sym dummy-args))
		  (progn
		    (push arg cleaned-arg-list))))
	  mixed-arguments)
    (setf vector-args (nreverse vector-args)
	  dummy-args (nreverse dummy-args))
    (let ((sources `(list ,@(mapcar (lambda (vector)
				      `(list ,vector
					     nil))
				    vector-args)))
	  (combination-function
	   `(lambda (,@dummy-args)
	      (funcall ,function ,@(nreverse cleaned-arg-list)))))
      `(map-n-grids :sources ,sources 
		    :combination-function ,combination-function))))



(define-test mapgrid-ma-1
  ;; basic test of expansion
  (assert-expands
   '(MAP-N-GRIDS
     :SOURCES (LIST (LIST VX NIL) (LIST VY NIL))
     :COMBINATION-FUNCTION (LAMBDA (A% B%) (FUNCALL #'EXPT A% B%)))
   (mapgrid-ma-1 (expt @!a @!b) vx vy))
  ;; test numerical correctness of expansion values
  (assert-numerical-equal
   (mapgrid #'expt *v0* *v1*)
   (mapgrid-ma-1 (expt @!a @!b) *v0* *v1* ))
  ;; test expansion with mixed arguments
  (assert-expands
   '(MAP-N-GRIDS
     :SOURCES (LIST (LIST VX NIL) (LIST VZ NIL))
     :COMBINATION-FUNCTION (LAMBDA (X% Z%) (FUNCALL #'EXPT X% Y Z%)))
   (mapgrid-ma-1 (expt @!x y @!z) vx vz))
  ;; test expansion when a vector argument is an s-expression (same as
  ;; for *v1*)
  (assert-expands
   '(MAP-N-GRIDS
     :SOURCES
     (LIST
      (LIST *V0* NIL)
      (LIST (MAKE-GRID `((,*DEFAULT-GRID-TYPE* 4) ,*DEFAULT-ELEMENT-TYPE*)
		       :INITIAL-CONTENTS (QUOTE (10D0 11D0 12D0 13D0)))
       NIL))
     :COMBINATION-FUNCTION
     (LAMBDA (X% Z%) (FUNCALL #'EXPT X% Y Z%)))
   (mapgrid-ma-1 (expt @!x y @!z) *v0* (make-grid `((,*default-grid-type* 4) ,*default-element-type*)
					       :initial-contents '(10d0 11d0 12d0 13d0))))
  ;; test numerical correctness when a vector argument is an
  ;; s-expression (same as for *v1*)
  (assert-numerical-equal
   (mapgrid #'expt *v0* *v1*)
   (mapgrid-ma-1 (expt @!x @!z) *v0* (make-grid `((,*default-grid-type* 4) ,*default-element-type*)
					       :initial-contents '(10d0 11d0 12d0 13d0)))))

(defmacro mapgrid-ma-1 ((function &rest arguments) &rest vector-arguments)
  "Map function over VECTOR-ARGUMENTS, building the argument list from
ARGUMENTS and VECTOR-ARGUMENTS.

ARGUMENTS can be either scalars or dummy symbols that start with @!.
Each dummy symbol correspond to one of the VECTOR-ARGUMENTS. Their
values are taken from successive places in VECTOR-ARGUMENTS.

The vectors can be CL arrays or foreign-arrays

For example, a call to (mapgrid-ma-1 (fun @!vx y @!vz) va vb)

expands into
 (map-n-grids :sources (list (list va nil) (list vb nil))
              :combination-function #'(lambda (vx% vz%)
                                           (fun vx% y vz%)
"
  (let (dummy-args
	cleaned-arg-list)
    (mapc #'(lambda (arg)
	      (if (@!-symbol-p arg)
		  (let* ((cleaned-sym
			  (intern (subseq (symbol-name arg) 2)))
			 (dummy-sym (symbolicate cleaned-sym "%")))
		    (push dummy-sym cleaned-arg-list)
		    (push dummy-sym dummy-args))
		  (progn
		    (push arg cleaned-arg-list))))
	  arguments)
    (setf dummy-args (nreverse dummy-args))
    (let ((sources `(list ,@(mapcar (lambda (vector)
				      `(list ,vector
					     nil))
				    vector-arguments)))
	  (combination-function
	   `(lambda (,@dummy-args)
	      (funcall #',function ,@(nreverse cleaned-arg-list)))))
      `(map-n-grids :sources ,sources 
		    :combination-function ,combination-function))))


(define-test mapgrid-man-1
  ;; test expansion
  (assert-expands
   '(let ((arg0% x)
	  (arg1% y))
     (let ((d0 (dim0 arg0%))
	   (d1 (dim0 arg1%)))
       (let ((res (make-grid (list (list *default-grid-type* d0 d1)
				   *default-element-type*))))
	 (iter:iter
	   (iter:for row :matrix-row-index res)
	   (iter:iter
	     (iter:for column  :matrix-column-index res)
	     (setf (aref  res row column)
		   (funcall #'fun
		    (aref arg0% row)
		    (aref arg1% column)
		    z))))
	 res)))
   (mapgrid-man #'fun @0x @1y z))
  ;; test expansion with reversed argument order
  (assert-expands
   '(let ((arg0% y)
	  (arg1% x))
     (let ((d0 (dim0 arg0%))
	   (d1 (dim0 arg1%)))
       (let ((res (make-grid (list (list *default-grid-type* d0 d1)
				   *default-element-type*))))
	 (iter:iter
	   (iter:for row :matrix-row-index res)
	   (iter:iter
	     (iter:for column  :matrix-column-index res)
	     (setf (aref  res row column)
		   (funcall #'fun
		    (aref arg1% column)
		    (aref arg0% row)
		    z))))
	 res)))
   (mapgrid-man #'fun @1x @0y z))
  ;; test numerical values of expansion
  (assert-numerical-equal
   #2A((1.0d0 10.0d0 100.0d0 1000.0d0)
	       (1.0d0 11.0d0 121.0d0 1331.0d0)
	       (1.0d0 12.0d0 144.0d0 1728.0d0)
	       (1.0d0 13.0d0 169.0d0 2197.0d0))
   (mapgrid-man #'expt @0*v1* @1*v0*))
  ;; test numerical values of expansion with reversed arguments
  (assert-numerical-equal
   #2A((1.0d0 1.0d0 1.0d0 1.0d0)
	       (10.0d0 11.0d0 12.0d0 13.0d0)
	       (100.0d0 121.0d0 144.0d0 169.0d0)
	       (1000.0d0 1331.0d0 1728.0d0 2197.0d0))
   (mapgrid-man #'expt @1*v1* @0*v0*)))

(defmacro mapgrid-man (fun &rest args)
  (let (funcall-args arg0 arg1)
    (mapc
     #'(lambda (arg)
	 (cond
	   ((@0-symbol-p arg)
	    (assert (not arg0) ()
		 (error "arg0, ~a already defined" arg0))
	    (let ((cleaned-sym
		   (intern (subseq (symbol-name arg) 2))))
	      (setf arg0 cleaned-sym)
	      (push '(aref arg0% row) funcall-args)))
	   ((@1-symbol-p arg)
	    (assert (not arg1) ()
		 (error "arg1, ~a already defined" arg1))
	    (let ((cleaned-sym
		   (intern (subseq (symbol-name arg) 2))))
	      (setf arg1 cleaned-sym)
	      (push '(aref arg1% column) funcall-args)))
	   (t (push arg funcall-args))))
	  args)
    (setf funcall-args (nreverse funcall-args))
    (assert arg0 () "Vector 0 is undefined")
    (assert arg1 () "Vector 1 is undefined")
    `(let ((arg0% ,arg0)
	   (arg1% ,arg1))
       (let ((d0 (dim0 arg0%))
	     (d1 (dim0 arg1%)))
	 (let ((res (make-grid (list (list *default-grid-type* d0 d1)
				     *default-element-type*))))
	   (iter:iter
	    (iter:for row :matrix-row-index res)
	    (iter:iter
	     (iter:for column  :matrix-column-index res)
	       (setf (aref res row column)
		     (funcall ,fun ,@funcall-args))))
	   res)))))


(define-test mapgrid-man-1
  (assert-expands
   '(let ((x x)
	  (y y))
     (let ((d0 (dim0 x))
	   (d1 (dim0 y)))
       (let ((res (make-grid (list (list *default-grid-type* d0 d1)
				   *default-element-type*))))
	 (iter:iter
	   (iter:for row :matrix-row-index res)
	   (iter:iter
	     (iter:for column  :matrix-column-index res)
	     (setf (aref  res row column)
		   (fun
		    (aref x row)
		    (aref y column)
		    z))))
	 res)))
   (mapgrid-man-1 (fun @0x @1y z) x y))
  (assert-numerical-equal
   #2A((1 1 1) (2 4 8))
   (mapgrid-man-1 (expt @0b @1x) #(1 2) #(1 2 3)))
  (labels ((fun (x y z)
	     (+ z (expt x y))))
    (assert-numerical-equal
     #2A((0 0 0) (1 3 7))
     (mapgrid-man-1 (fun @0b @1x -1) #(1 2) #(1 2 3)))))



(defmacro mapgrid-man-1 ((fun &rest args) vec0 vec1)
  "Return a matrix of dimensions (dim0 vec0) (dim0 vec1) filled by
applying `fun' to `args'.

`args' must have two arguments whose names start by @0 and @1.  In the
calls to `fun' the elements of arguments @0 and @1 are accessed using
the row and column indices respectively

The other arguments are passed unevaluated"
  (let (funcall-args arg0 arg1)
    (mapc
     ;; loop over args and create a cleaned up argument list by
     ;; stripping the @ tags.  At the same time, process the @-tagged
     ;; args as follows:
     ;; - create a (aref ...) snippet that will be used to access
     ;;   their elements
     ;; - Make sure there is only one of @0 and @1 tags present.  This
     ;;   is done by examining contents of arg0 and arg1 which store
     ;;   the cleaned up @0 and @1 args.
     #'(lambda (arg)
	 (cond
	   ((@0-symbol-p arg)
	    (assert (not arg0) ()
		 (error "arg0, ~a already defined" arg0))
	    (let ((cleaned-sym
		   (intern (subseq (symbol-name arg) 2))))
	      (setf arg0 cleaned-sym)
	      (push `(aref ,cleaned-sym row) funcall-args)))
	   ((@1-symbol-p arg)
	    (assert (not arg1) ()
		 (error "arg1, ~a already defined" arg1))
	    (let ((cleaned-sym
		   (intern (subseq (symbol-name arg) 2))))
	      (setf arg1 cleaned-sym)
	      (push `(aref ,cleaned-sym column) funcall-args)))
	   (t (push arg funcall-args))))
	  args)
    (assert arg0 () "Vector 0 is undefined")
    (assert arg1 () "Vector 1 is undefined")
    `(let ((,arg0 ,vec0)
	   (,arg1 ,vec1))
       (let ((d0 (dim0 ,arg0))
	     (d1 (dim0 ,arg1)))
	 (let ((res (make-grid (list (list *default-grid-type* d0 d1)
				     *default-element-type*))))
	   (iter:iter
	    (iter:for row :matrix-row-index res)
	    (iter:iter
	     (iter:for column  :matrix-column-index res)
	       (setf (aref res row column)
		     (,fun ,@(nreverse funcall-args)))))
	   res)))))
