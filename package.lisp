(defpackage :2d-geometry
  (:nicknames :geometry)
  (:use :common-lisp :iterate))

(in-package :2d-geometry)

(defstruct dlist
  val next prev)
