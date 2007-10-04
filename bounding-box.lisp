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

(defun point-in-box-exclusive (point box &key (include-in-degenerate-dimension nil))
  "Check if point is contained inside a bounding box."
  (or (and (> (x point)(x-min box))
	   (> (y point)(y-min box))
	   (< (x point)(x-max box))
	   (< (y point)(y-max box)))
      (when include-in-degenerate-dimension
	(or (and (= (x-min box)(x-max box)(x point))
		 (> (y point)(y-min box))
		 (< (y point)(y-max box)))
	    (and (= (y-min box)(y-max box)(y point))
		 (> (x point)(x-min box))
		 (< (x point)(x-max box)))))))

(defun point-in-box-inclusive (point box)
  "Check if point is contained inside or directly on a bounding box."
  (and (>= (x point)(x-min box))
       (>= (y point)(y-min box))
       (<= (x point)(x-max box))
       (<= (y point)(y-max box))))

(defun intersect-boxes (box1 box2)
  "Return bounding box common to both boxes."
  (when (bounding-boxes-intersect-p box1 box2)
    (make-instance 'bounding-box
		   :x-min (max (x-min box1)
			       (x-min box2))
		   :x-max (min (x-max box1)
			       (x-max box2))
		   :y-min (max (y-min box1)
			       (y-min box2))
		   :y-max (min (y-max box1)
			       (y-max box2)))))