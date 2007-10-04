(in-package :2d-geometry)

(defclass bounding-box ()
  ((x-min :accessor x-min :initarg :x-min)
   (x-max :accessor x-max :initarg :x-max)
   (y-min :accessor y-min :initarg :y-min)
   (y-max :accessor y-max :initarg :y-max))
  (:documentation "A bounding box."))

(defgeneric construct-bounding-box (object)
  (:documentation "Construct a bounding box of a given object."))

(defun bounding-boxes-intersect-p (box1 box2)
  "Check if two bounding boxes intersect."
  (not (or (< (x-max box1)(x-min box2))
	   (< (x-max box2)(x-min box1))
	   (< (y-max box1)(y-min box2))
	   (< (y-max box2)(y-min box1)))))

(defun point-in-box-exclusive (point box)
  "Check if point is contained inside a bounding box."
  (and (> (x point)(x-min box))
       (> (y point)(y-min box))
       (< (x point)(x-max box))
       (< (y point)(y-max box))))

(defun point-in-box-inclusive (point box)
  "Check if point is contained inside a bounding box."
  (and (>= (x point)(x-min box))
       (>= (y point)(y-min box))
       (<= (x point)(x-max box))
       (<= (y point)(y-max box))))