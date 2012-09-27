;;;; map-grid-utils.asd

(asdf:defsystem #:map-grid-utils
  :serial t
  :description "Functions and macros that facilitate mapping of functions over 1D grids (vectors)"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :license "not for re-use as yet"
  :depends-on ("alexandria"
	       "iterate"
	       "lisp-unit"
               "foreign-array"
               "grid"
	       "symbol-name-queries")
  :components ((:file "map-grid-utils-package-def")
	       (:file "setup")
	       (:file "utilities")
               (:file "map-grid")
	       (:file "map-grid-mixed-args")))

