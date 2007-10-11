(in-package :2d-geometry)

;;;; This file contains functions to decompose a complex polygons into simple ones.

;;; Brute force decomposition into non-disjoint simple polygons

;;; The main problem is efficient finding of intersections. Bentley-Ottmann algorithm can do this in
;;; O((n+k)logn), but it is  somewhat complicated. As it is, brute force  should be quadratic, maybe
;;; n^3 if  I create one  loop too many... actually,  I will be  glad if it's subexponential  at the
;;; moment.


(defun find-intersection (edge-list)
  (labels ((recurse-edge-list (edge-list-1 edge-list-2)
	     (if (null edge-list-1)
		 nil
		 (if (null edge-list-2)
		     (recurse-edge-list (cdr edge-list-1) (cddr edge-list-1))
		     (let ((edge1 (car edge-list-1))
			   (edge2 (car edge-list-2)))
		       (if (intersect-proper-p (start edge1)(end edge1)
					       (start edge2)(end edge2))
			   (list edge1 edge2 (line-segments-intersection-point edge1 edge2))
			   (recurse-edge-list edge-list-1 (cdr edge-list-2))))))))
    (recurse-edge-list edge-list (cdr edge-list))))

(defun decompose-complex-polygon-nondisjoint (polygon)
  "Decomposes a complex polygon into a set of simple ones, possibly some entirely contained in others."
  (let ((ring-head (double-linked-ring-from-point-list polygon))
	(simple-polys nil))
    (let ((ring-index (collect-ring-nodes ring-head))
	  (edge-list (ring-to-list-of-edges ring-head)))
      (iterate (while ring-index)
	       (iterate (for inters next (find-intersection edge-list))
			(while inters)
			(destructuring-bind (edge1 edge2 i-point) inters
			  (let ((v1 (make-instance 'poly-ring-node
						   :val i-point
						   :prev (start edge1)
						   :next (end edge2)))
				(v2 (make-instance 'poly-ring-node
						   :val i-point
						   :prev (start edge2)
						   :next (end edge1))))
			    (push v1 ring-index)
			    (push v2 ring-index)
			    (setf (next-node (start edge1)) v1)
			    (setf (prev-node (end edge1)) v2)
			    (setf (next-node (start edge2)) v2)
			    (setf (prev-node (end edge2)) v1)
			    (setf edge-list (append
					     (list (make-instance 'line-segment
								  :start (start edge1)
								  :end v1)
						   (make-instance 'line-segment
								  :start v1
								  :end (end edge2))
						   (make-instance 'line-segment
								  :start (start edge2)
								  :end v2)
						   (make-instance 'line-segment
								  :start v2
								  :end (end edge1)))
					     (remove-if #'(lambda (e)
							    (or (eql e edge1)
								(eql e edge2)))
							edge-list))))))
	       (push (point-list-from-ring ring-head) simple-polys)
	       (setf ring-index (set-difference ring-index
						(collect-ring-nodes ring-head)
						:test #'point-equal-p))
	       (setf ring-head (car ring-index))))
    simple-polys))