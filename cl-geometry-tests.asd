(asdf:defsystem :cl-geometry-tests
  :description "Tests for cl-geometry."
  :version "0"
  :licence "BSD"
  :components ((:file "test-geometry"))
  :depends-on (:cl-geometry :vecto :iterate))