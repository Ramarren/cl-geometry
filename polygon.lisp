(in-package :2d-geometry)

;;;; This files contains top level polygon manipulating functions.

(defun decompose-complex-polygon-bentley-ottmann (polygon)
  "Decompose polygon using bentley-ottmann, hopefully in something close to quadratic time."
  (if (simple-polygon-sh-p polygon)
      (list polygon)
      (let ((ring-index (collect-ring-nodes
			 (double-linked-ring-from-point-list polygon))))
	(let ((ring-edges (edge-list-from-point-list ring-index 'taint-segment)))
	  (let ((in-points (bentley-ottmann ring-edges))
		(simple-polys nil))
	    (dolist (tk in-points)
	      (let ((edge1 (edge1 tk))
		    (edge2 (edge2 tk)))
		(unless (or (taint edge1)
			    (taint edge2));vertex surgery will invalidate edges
		  (let ((in1 (start edge1))
			(out1 (end edge1))
			(in2 (start edge2))
			(out2 (end edge2)))
		    (let ((v1 (make-instance 'poly-ring-node
					     :val tk
					     :prev in1
					     :next out2))
			  (v2 (make-instance 'poly-ring-node
					     :val tk
					     :prev in2
					     :next out1)))
		      (push v1 ring-index)
		      (push v2 ring-index)
		      (setf (taint edge1) t
			    (taint edge2) t)
		      (setf (next-node in1) v1)
		      (setf (prev-node out1) v2)
		      (setf (next-node in2) v2)
		      (setf (prev-node out2) v1))))))
	    (iterate (while ring-index)
		     (push (collect-ring-nodes (car ring-index)) simple-polys)
		     (setf ring-index (set-difference ring-index (car simple-polys))))
	    (reduce #'append
		    (mapcar #'decompose-complex-polygon-bentley-ottmann ;due to tainting the polygon might not have been completely decomposed
			    (mapcar #'(lambda (poly)
					(mapcar #'val poly))
				    simple-polys))))))))

(defun simple-polygon-sh-p (polygon)
  "Check if polygon is simple using Shamos-Hoey algorithm."
  (not (shamos-hoey (edge-list-from-point-list polygon))))

(defun trapezoids-to-triangles (trapez)
  "Convert list of trapezoids to list of triangles."
  (let ((triangles nil))
    (dolist (tk trapez)
      (let ((ctrap (collapse-trapezoid tk)))
	(cond
	  ((= (length ctrap) 3)
	   (push ctrap triangles))
	  ((= (length ctrap) 4)
	   (destructuring-bind (tr1 . tr2) (split-trapezoid ctrap)
	     (push tr1 triangles)
	     (push tr2 triangles))))))
    triangles))

(defun triangle-center-point (triangle)
  "Return a central point of triangle."
  (destructuring-bind (a b c) triangle
    (make-instance 'point
		   :x (/ (+ (x a)(x b)(x c)) 3)
		   :y (/ (+ (y a)(y b)(y c)) 3))))

(defun decompose-complex-polygon-triangles (polygon &key (in-test 'point-in-polygon-winding-p))
  "Decomposes a complex polygon into triangles. Returns a list of triangles inside polygon according to :in-test, which is a function taking a point and a polygon."
  (let ((trapez (trapezoidize-edges (edge-list-from-point-list polygon))))
    (let ((triangles (trapezoids-to-triangles trapez)))
      (remove-if-not #'(lambda (x)
			 (funcall in-test (triangle-center-point x) polygon))
		     triangles))))
