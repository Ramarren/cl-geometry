(asdf:defsystem :cl-geometry
  :description "Library for two dimensional geometry."
  :version "0.0.3"
  :licence "BSD-style"
  :author "Jakub Higersberger <ramarren@gmail.com>"
  :components ((:file "package" :depends-on ("heap"))
               (:file "trivial-geometry" :depends-on ("package"))
               (:file "basic-point" :depends-on ("package"))
               (:file "bounding-box" :depends-on ("package"))
               (:file "basic-line" :depends-on ("bounding-box" "package"))
               (:file "representations" :depends-on ("package" "basic-point" "basic-line"))
               (:file "polygon-class" :depends-on ("package" "representations"))
               (:file "basic-polygon" :depends-on ("basic-point" "polygon-class" "basic-line"))
               (:file "triangulation" :depends-on ("basic-line" "trivial-geometry" "basic-polygon" "representations" "polygon-class"))
               (:file "decomposition" :depends-on ("basic-line" "basic-polygon" "triangulation" "representations" "polygon-class"))
               (:file "heap")
               (:file "bentley-ottmann" :depends-on ("heap" "representations" "polygon-class"))
               (:file "trapezoidation" :depends-on ("bentley-ottmann" "polygon-class"))
               (:file "polygon" :depends-on ("basic-polygon" "polygon-class" "triangulation" "decomposition" "trapezoidation"))
               (:file "polygon-binary" :depends-on ("polygon" "polygon-class")))
  :depends-on (:iterate :trees))
