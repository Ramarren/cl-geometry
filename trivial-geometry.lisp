(in-package :2d-geometry)

;;;; This file implements basic formulas for two dimensional geometry.

(defun distance (x1 y1 x2 y2)
  "Distance between two points on a plane."
  (let ((x-dist (- x2 x1))
	(y-dist (- y2 y1)))
    (sqrt (+ (* x-dist x-dist)
	     (* y-dist y-dist)))))

;;circle
(defun circumference-circle (r)
  "Circumference of a circle."
  (* 2 pi r))

(defun area-circle (r)
  "Area of a circle."
  (* pi r r))

;;ellipse
(defun area-ellipse-axes (a b)
  "Area of an ellipse given semimajor and semiminor axes."
  (* pi a b))

(defun circumference-ellipse-axes (a b)
  "Circumference of an ellipse given semimajor and semiminro axes using Ramanujan's approximation."
  (* pi (- (* 3 (+ a b))
	   (sqrt (* (+ (* 3 a) b)
		    (+ a (* 3 b)))))))

;;triangle
(defun perimeter-triangle (a b c)
  "Perimeter of a triangle given length of edges."
  (+ a b c))

(defun area-triangle-edge-edge-angle (a b alpha)
  "Area of a triangle given length of two edges and an angle between them."
  (/ (* a b (sin alpha)) 2))

(defun area-triangle-edges (a b c)
  "Area of a triangle given length of edges using Heron's formula. Numerically unstable for triangles with very small angles."
  (let ((p (/ (+ a b c) 2)))
    (sqrt (* p (- p a) (- p b) (- p c)))))

(defun area-triangle-edges-small-angles (a b c)
  "Area of a triangle given length of edges using numerically stabilized Heron's formula."
  (let ((edge-list (list a b c)))
    (destructuring-bind (sa sb sb) (sort edge-list #'>)
      (/ (sqrt (+ a (+ b c)) (- c (- a b))(+ c (- a b))(+ a (- b c))) 4))))

(defun area-triangle-vertices (xa ya xb yb xc yc)
  "Area of a triangle given positions of vertices."
  (/ (- (* (- xb xa) (- yc ya))(* (- yb ya) (- xc xa)))) 2)

;;rectangle
(defun area-rectangle (a b)
  "Area of a rectangle given length of edges."
  (* a b))

(defun perimeter-rectangle (a b)
  "Perimeter of a rectangle given length of edges."
  (+ a a b b))

;;square
(defun area-square (a)
  "Area of a square given length of a side."
  (* a a))

(defun perimeter-square (a)
  "Perimeter of a square given length of a side."
  (* 4 a))
