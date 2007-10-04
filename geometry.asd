(defpackage :2d-geometry-system (:use :cl :asdf))

(in-package :2d-geometry-system)

(defsystem :geometry
  :description "Twodimnsional geometry."
  :version "0.0.1"
  :components ((:file "package")
	       (:file "trivial-geometry" :depends-on ("package"))
	       (:file "bounding-box" :depends-on ("package"))
	       (:file "basic-line" :depends-on ("bounding-box" "package")))
  :depends-op (:iterate))
  