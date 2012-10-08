;;;; map-grid-utils.asd

(asdf:defsystem #:map-grid-utils
  :serial t
  :description "Describe map-grid-utils here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on ("alexandria"
	       "iterate"
	       "lisp-unit"
               #+foreign-array-included-in-antik "foreign-array"
               "antik"
	       "symbol-name-queries")
  :components ((:file "map-grid-utils-package-def")
	       (:file "setup")
	       (:file "utilities")
               (:file "map-grid")
	       (:file "map-grid-mixed-args")))

