;;;; package.lisp
(in-package :cl-user)

(defpackage #:map-grid-utils
  (:use #:cl #:lisp-unit #:iterate
	#:grid #:symbol-name-queries)
  (:import-from #:alexandria
		:symbolicate)
  (:shadow #:lisp-unit :norm)
  (:shadow #:iterate :in))

