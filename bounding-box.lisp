(in-package :2d-geometry)

(defclass bounding-box ()
  ((x-min :accessor x-min :initarg :x-min)
   (x-max :accessor x-max :initarg :x-max)
   (y-min :accessor y-min :initarg :y-min)
   (y-max :accessor y-max :initarg :y-max))
  (:documentation "A bounding box."))

(defgeneric construct-bounding-box (object)
  (:documentation "Construct a bounding box of a given object."))