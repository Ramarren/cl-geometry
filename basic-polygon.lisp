(in-package :2d-geometry)

;;;; This file defines basic polygon functions.

;;;; Express polygon a simple list of points.

;;;or a list od edges
(defun edge-list-from-point-list (polygon)
  "Change polygon represented as a list of points into a list of edges (line segments)."
  (let ((vertex-zero (car polygon)))
    (maplist #'(lambda (lst)
		 (if (null (cadr lst))
		     (make-instance 'line-segment :start (car lst) :end vertex-zero)
		     (make-instance 'line-segment :start (car lst) :end (cadr lst))))
	     polygon)))

;;; or double-linked ring
(defclass poly-ring-node ()
  ((val :accessor val :initarg :val)
   (next :accessor next-node :initarg :next)
   (prev :accessor prev-node :initarg :prev))
  (:documentation "Double linked ring node."))

(defmethod print-object ((object poly-ring-node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "NODE: ~a" (val object))))

(defun double-linked-ring-from-point-list (polygon)
  "Change polygon representation from list of points to double linked ring of points."
  (let ((head (make-instance 'poly-ring-node)))
    (let ((tail head))
      (dolist (tk polygon)
	(setf (val tail) tk
	      (next-node tail) (make-instance 'poly-ring-node)
	      (prev-node (next-node tail)) tail
	      tail (next-node tail)))
      (setf (prev-node head) (prev-node tail)
	    (next-node (prev-node tail)) head))
    head))

(defmethod construct-bounding-box ((object list));assumes all list are polygons...
  (iterate (for vertex in object)
	   (minimizing (x vertex) into x-min)
	   (minimizing (y vertex) into y-min)
	   (maximizing (x vertex) into x-max)
	   (maximizing (y vertex) into y-max)
	   (finally (return (make-instance 'bounding-box
					   :x-min x-min
					   :x-max x-max
					   :y-min y-min
					   :y-max y-max)))))

(defun notany-symmetric-test (testfun lst)
  "Return t if test is nil for every combination of elements of lst, assuming test is symmetric."
  (labels ((recurse-list (lst1 lst2)
	     (if (null lst1)
		 t
		 (if (null lst2)
		     (recurse-list (cdr lst1)(cddr lst1))
		     (if (not (null (funcall testfun (car lst1) (car lst2))))
			 nil
			 (recurse-list lst1 (cdr lst2)))))))
    (recurse-list lst (cdr lst))))

(defun frustrated-polygon-p (polygon)
  "Check if any two colinear edges intersect."
  (let ((edge-list (edge-list-from-point-list polygon)))
    (not (notany-symmetric-test #'line-segments-intersection-segment edge-list))))

(defun simple-polygon-p (polygon)
  "Check if polygon is simple, ie. if no two edges intersect, assuming only point intersections are possible. This uses brute force, comparing each edge to every other edge."
  (let ((edge-list (edge-list-from-point-list polygon)))
    (notany-symmetric-test #'(lambda (x y)
			       (line-segments-intersection-point x y :exclude-endpoints t))
			   edge-list)))


(defun polygon-orientation (polygon)
  "Return 1 if polygon is counterclockwise and -1 if it is oriented clockwise. Assumes simple polygon."
  (let ((poly-ring (double-linked-ring-from-point-list polygon)))
    ;find rightmost lowest vertex
    (let ((lowest-rightmost-node (do ((node poly-ring (next-node node))
				      (min-node nil)
				      (min-val nil))
				     ((and (eq poly-ring node) min-val) min-node)
				   (when (or (null min-val)
					     (and (<= (y (val node)) (y min-val))
						  (> (x (val node)) (x min-val))))
				     (setf min-node node
					   min-val (val node))))))
      (let ((end-of-leaving-edge (val (next-node lowest-rightmost-node)))
	    (start-of-entering-edge (val (prev-node lowest-rightmost-node))))
	(let ((line-entering (line-from-segment (make-instance 'line-segment
							       :start start-of-entering-edge
							       :end (val lowest-rightmost-node)))))
	  (let ((is-end-left (point-line-position end-of-leaving-edge line-entering)))
	    (cond
	      ((> is-end-left 0) 1)
	      ((< is-end-left 0) -1)
	      ((zerop is-end-left) (warn "Degenerate polygon")))))))))

(defun area-simple-polygon (polygon)
  "Calculate an area of a simple polygon."
  (* 1/2
     (polygon-orientation polygon)
     (reduce #'+ (maplist #'(lambda (lst)
			      (let ((v1 (car lst))
				    (v2 (if (cdr lst)
					    (cadr lst)
					    (car polygon))))
				(- (* (x v1)(y v2))(* (x v2)(y v1)))))
			  polygon))))

(defun filter-ray-intersection (point edge)
  "Return t if edge does not intersect ray from point properly."
  (let ((line (line-from-segment edge)))
    (or (zerop (A line));line is horizontal
	(let ((max-y (max (y (start edge))
			  (y (end edge))))
	      (min-y (min (y (start edge))
			  (y (end edge)))))
	  (or (<= max-y (y point));line if below or at the ray
	      (> min-y (y point))));line is above the ray
	(>= (point-line-position point line) 0))));edge is to the left of the point

(defun point-in-polygon-crossing-p (point polygon)
  "Determine if a point belongs to a polygon using crossing (oddeven) rule."
  (let ((edge-list (edge-list-from-point-list polygon)))
    (oddp (count-if-not #'(lambda (edge)
			    (filter-ray-intersection point edge))
			edge-list))))

(defun point-in-polygon-winding-number (point polygon)
  "Determine if point is inside polygon using winding rule."
  (let ((edge-list (edge-list-from-point-list polygon)))
    (let ((intersecting-edges (remove-if #'(lambda (edge)
					     (filter-ray-intersection point edge))
					 edge-list)))
      (reduce #'+ (mapcar #'(lambda (edge)
			      (if (> (y (start edge))
				     (y (end edge)))
				  1
				  -1))
			  intersecting-edges)))))

(defun point-in-polygon-winding-p (point polygon)
  (not (zerop (point-in-polygon-winding-number point polygon))))

