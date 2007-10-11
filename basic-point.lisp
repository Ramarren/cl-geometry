(in-package :2d-geometry)

;;;; This file implements basic functions operating on points. A point is any object with methods (x
;;;; object) (y object).

(defclass point ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0))
  (:documentation "A point on a plane, with cartesian coordinates."))

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a,~a" (x object) (y object))))

(defmethod x ((object poly-ring-node))
  (x (val object))
  (:documentation "Access x coordinate of a point-like object."))

(defmethod y ((object poly-ring-node))
  (y (val object))
  (:documentation "Access y coordinate of a point-like object."))

(defun make-point (x y &optional (point-type 'point))
  (make-instance point-type :x x :y y))

(defun point-equal-p (point1 point2)
  "Checks if two points are geometrically equal."
  (and (= (x point1)(x point2))
       (= (y point1)(y point2))))

(defun coords-to-points (coord-list)
  "Coordinate list (x1 y1 x2 y2 ... xn yn) to point list"
  (assert (zerop (mod (length coord-list) 2)))
  (labels ((recurse-list (coord-list acc)
	     (if (null coord-list)
		 (nreverse acc)
		 (recurse-list (cddr coord-list)
			       (cons (make-point (car coord-list) (cadr coord-list))
				     acc)))))
    (recurse-list coord-list nil)))

(defun left-p (a b c)
  "Is c to the left of the oriented line defined by a->b?"
  (> (area-triangle-vertices (x a)(y a)(x b)(y b) (x c)(y c)) 0))

(defun left-on-p (a b c)
  "Is c to the left or on the oriented line defined by a->b?"
  (>= (area-triangle-vertices (x a)(y a)(x b)(y b) (x c)(y c)) 0))

(defun colinear-p (a b c)
  "Is c on the line defined by a->b?"
  (zerop (area-triangle-vertices (x a)(y a)(x b)(y b) (x c)(y c))))

(defun between-p (a b c)
  "Is c colinear with a->b and lies between them?"
  (when (colinear-p a b c)
    (if (= (x a)(x b))
	(or (and (>= (x c)(x a))
		 (<= (x c)(x b)))
	    (and (>= (x c)(x b))
		 (<= (x c)(x a))))
	(or (and (>= (y c)(y a))
		 (<= (y c)(y b)))
	    (and (>= (y c)(y b))
		 (>= (y c)(y a)))))))

(defun xor (p q)
  "Exlusive or logical operation."
  (if (or (and p q)
	  (and (not p)(not q)))
      nil
      t))

(defun intersect-proper-p (a b c d)
  "Do segments a->b and c->d intersect?"
  (unless (or (colinear-p a b c)
	      (colinear-p a b d)
	      (colinear-p c d a)
	      (colinear-p c d b))
    (and (xor (left-p a b c)
	      (left-p a b d))
	 (xor (left-p c d a)
	      (left-p c d b)))))

(defun intersect-p (a b c d)
  "Do segments a->b and c->d intersect?"
  (if (intersect-proper-p a b c d)
      t
      (or (between-p a b c)
	  (between-p a b d)
	  (between-p c d a)
	  (between-p c d b))))

(defun point-sort-fun (point1 point2)
  "Order points by increasing x then y."
  (if (= (x point1)(x point2))
      (if (= (y point1)(y point2))
	  (if (typep point1 'event-endpoint)
	      (eql (direction point1) 'right))
	  (< (y point1)(y point2)))
      (< (x point1)(x point2))))
