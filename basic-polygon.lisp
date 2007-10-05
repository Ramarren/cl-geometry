(in-package :2d-geometry)

;;;; This file defines basic polygon functions.

;;;; Express polygon a simple list of points.

(defun edge-list-from-point-list (polygon)
  "Change polygon represented as a list of points into a list of edges (line segments)."
  (let ((vertex-zero (car polygon)))
    (maplist #'(lambda (lst)
		 (if (null (cadr lst))
		     (make-instance 'line-segment :start (car lst) :end vertex-zero)
		     (make-instance 'line-segment :start (car lst) :end (cadr lst))))
	     polygon)))

(defstruct dlist
  val next prev)

(defmethod print-object ((object dlist) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "NODE: ~a" (dlist-val object))))

(defun double-linked-ring-from-point-list (polygon)
  "Change polygon representation from list of points to double linked ring of points."
  (let ((head (make-dlist)))
    (let ((tail head))
      (dolist (tk polygon)
	(setf (dlist-val tail) tk
	      (dlist-next tail) (make-dlist)
	      (dlist-prev (dlist-next tail)) tail
	      tail (dlist-next tail)))
      (setf (dlist-prev head) (dlist-prev tail)
	    (dlist-next (dlist-prev tail)) head))
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
  "Check if any two edges intersect linearly."
  (let ((edge-list (edge-list-from-point-list polygon)))
    (not (notany-symmetric-test #'line-segments-intersection-segment edge-list))))

(defun simple-polygon-p (polygon)
  "Check if polygon is simple, ie. if no two edges intersect, assuming only point intersection are possible."
  (let ((edge-list (edge-list-from-point-list polygon)))
    (notany-symmetric-test #'(lambda (x y)
			       (line-segments-intersection-point x y :exclude-endpoints t))
			   edge-list)))


(defun polygon-orientation (polygon)
  "Return 1 if polygon is counterclockwise and -1 if it is oriented clockwise. Assumes simple polygon."
  (let ((poly-ring (double-linked-ring-from-point-list polygon)))
    ;find rightmost lowest vertex
    (let ((lowest-rightmost-node (do ((node poly-ring (dlist-next node))
				      (min-node nil)
				      (min-val nil))
				     ((and (eq poly-ring node) min-val) min-node)
				   (when (or (null min-val)
					     (and (<= (y (dlist-val node)) (y min-val))
						  (> (x (dlist-val node)) (x min-val))))
				     (setf min-node node
					   min-val (dlist-val node))))))
      (let ((end-of-leaving-edge (dlist-val (dlist-next lowest-rightmost-node)))
	    (start-of-entering-edge (dlist-val (dlist-prev lowest-rightmost-node))))
	(let ((line-entering (line-from-segment (make-instance 'line-segment
							       :start start-of-entering-edge
							       :end (dlist-val lowest-rightmost-node)))))
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

(defun filter-ray-intersections (point edge)
  "Return t if edge does not intersect ray from point properly."
  (let ((line (line-from-segment edge)))
    (or (zerop (A line));line is horizontal
	(let ((max-y (max (y (start edge))
			  (y (end edge))))
	      (min-y (min (y (start edge))
			  (y (end edge)))))
	  (or (<= max-y (y point));line if below or at the ray
	      (> min-y (y point))));line is above the ray
	(<= (point-line-position point line)))));edge is to the left of the point

(defun point-in-polygon-crossing (point polygon)
  "Determine if a point belongs to a polygon using crossing (oddeven) rule."
  (let ((edge-list (edge-list-from-point-list polygon)))
    (oddp (count-if-not #'filter-ray-intersection edge-list))))
    

(defun point-in-polygon-winding (point polygon)
  "Determine if point is inside polygon using winding rule."
  ;for every edge point is to the left of, check if it passes the horizontal ray, and in what direction
  )