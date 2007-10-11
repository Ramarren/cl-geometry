(defpackage :2d-geometry-system (:use :cl :asdf))

(in-package :2d-geometry-system)

(defsystem :geometry
  :description "Twodimensional geometry."
  :version "0.0.1"
  :components ((:file "package" :depends-on ("heap"))
	       (:file "trivial-geometry" :depends-on ("package"))
	       (:file "basic-point" :depends-on ("package"))
	       (:file "bounding-box" :depends-on ("package"))
	       (:file "basic-line" :depends-on ("bounding-box" "package"))
	       (:file "representation" :depends-on ("package" "basic-point" "basic-line"))
	       (:file "basic-polygon" :depends-on ("basic-line"))
	       (:file "triangulation" :depends-on ("basic-line" "trivial-geometry" "basic-polygon"))
	       (:file "decomposition" :depends-on ("basic-line" "basic-polygon" "triangulation"))
	       (:file "heap")
	       (:file "bentley-ottmann" :depends-on ("heap" "triangulation")))
  :depends-on (:iterate :trees))

