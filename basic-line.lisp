(in-package :2d-geometry)

;;;; This files defines basic functions for lines and line segments (geometric vectors).

(defclass point ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0))
  (:documentation "A point on a plane, with cartesian coordinates."))

(defclass line-segment ()
  ((start :accessor start :initarg :start :initform (make-instance 'point))
   (end :accessor end :initarg :end :initform (make-instance 'point)))
  (:documentation "A directed line segment defined by two points."))

(defclass line ()
  ((A :accessor A :initarg :A)
   (B :accessor B :initarg :B)
   (C :accessor C :initarg :C :initform 0))
  (:documentation "A line with an equation Ax+By+C=0."))

(defun line-from-segment (line-segment)
  "Calculate line from line segment."
  (check-type line-segment 'line-segment)
  (with-accessors (start end) line-segment
    (let ((x1 (x start))
	  (y1 (y start))
	  (x2 (x end))
	  (y2 (y end)))
      (cond
	((and (= x1 x2)(= y1 y2)) (error "Degenerate line segment."))
	((= x1 x2) (make-instance 'line :B 0 :A 1 :C (- x1)));vertical
	((= y1 y2) (make-instance 'line :A 0 :B 1 :C (- y1)))
	(t (make-instance 'line :A 1 :B (- (/ (- x2 x1)(- y2 y1))) :C (/ (- (* x1 y2) (* y1 x2))
									 (- y2 y1))))))))