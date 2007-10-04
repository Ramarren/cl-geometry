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

(defmethod construct-bounding-box ((object line-segment))
  (with-accessors (start end) line-segment
    (make-instance 'bounding-box
		   :x-min (min (x start) (x end))
		   :y-min (min (y start) (y end))
		   :x-max (max (x start) (x end))
		   :y-max (max (y start) (y end)))))

(defun line-segment-length (line-segment)
  "Calculate length of a segment."
  (check-type line-segment 'line-segment)
  (with-accessors (start end) line-segment
    (distance (x start)(y start)(x end)(y end))))

(defun lines-parralel-p (line1 line2)
  "Check if two lines are parrallel."
  (check-type line1 'line)
  (check-type line2 'line)
  (cond
    ((and (zerop (A line1))
	  (zerop (A line2)));both horizontal
     t)
    ((and (zerop (B line1))
	  (zerop (B line2)));both vertical
     t)
    ((or (zerop (A line1))
	 (zerop (A line2))
	 (zerop (B line1))
	 (zerop (B line2)));one horizontal or vertical and other not
     nil)
    ;this eliminates all special cases that could cause division by zero
    (t (let ((tan-1 (- (/ (A line1)(B line1))))
	     (tan-2 (- (/ (A line2)(B line2)))))
	 (= tan-1 tan-2)))))

(defun lines-intersection-point (line1 line2)
  "Find point of intersection of two lines. Returns nil if lines are parallel and point instance otherwise."
  (check-type line1 'line)
  (check-type line2 'line)
  (if (lines-parralel-p line1 line2)
      nil;parallel lines have no intersection point, this is a purely euclidan geometry library
      (make-instance 'point
		     :x (/ (- (* (B line2)(C line1))(* (B line1)(C line2)))
			   (- (* (A line2)(B line1))(* (A line1)(B line2))))
		     :y (- (/ (- (* (A line2)(C line1))(* (A line1)(C line2)))
			      (- (* (A line2)(B line1))(* (A line1)(B line2))))))))

(defun line-segments-intersection-point (line-segment1 line-segment2)
  "Find point of intersection of two segments. Returns nil if they do not intersect and point instance otherwise."
  (check-type line-segment1 'line-segment)
  (check-type line-segment2 'line-segment)
  (let ((box1 (construct-bounding-box line-segment1))
	(box2 (construct-bounding-box line-segment2)))
    (when (bounding-boxes-intersect-p box1 box2)
      (let ((line1 (line-from-segment line-segment1))
	    (line2 (line-from-segment line-segment2)))
	(let ((intersection-point (lines-intersection-point line1 line2)))
	  (when (and (point-in-box intersection-point box1)
		     (point-in-box intersection-point box2))
	    intersection-point))))))