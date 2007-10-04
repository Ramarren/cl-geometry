(defpackage :2d-geometry-system (:use :cl :asdf))

(in-package :2d-geometry-system)

(defsystem :geometry
  :description "Twodimnsional geometry."
  :version "0.0.1"
  :components ((:file "package"))
  :depends-op (:iterate))
  