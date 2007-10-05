(in-package :2d-geometry)

;;;; This file contains functions to decompose a complex polygons into simple ones.

;;; Brute force decomposition into non-disjoint simple polygons

;;; The main problem is efficient finding of intersections. Bentley-Ottmann algorithm can do this in
;;; O((n+k)logn), but it is  somewhat complicated. As it is, brute force  should be quadratic, maybe
;;; n^3 if I create one loop too many...

(defun collect-ring-nodes (ring)
  "Construct a list of all nodes in a ring."
  (labels ((recurse-ring (node head acc)
	     (if (eq node head)
		 (nreverse acc)
		 (recurse-ring (next-node node) head (cons node acc)))))
    (cons ring (recurse-ring (next-node ring) ring nil))))

(defmethod x ((object poly-ring-node))
  (x (val object)))

(defmethod y ((object poly-ring-node))
  (y (val object)))

(defun ring-to-list-of-edges (ring)
  "Construct a list of edges attached to vertexes."
  (labels ((recurse-ring (node head acc)
	     (if (eq node head)
		 acc
		 (recurse-ring (next-node node) head (cons (make-instance 'line-segment
									  :start (prev-node node)
									  :end node)
							   acc)))))
    (nreverse (cons (make-instance 'line-segment
				  :start (prev-node ring)
				  :end ring)
		    (recurse-ring (next-node ring) ring nil)))))

(defun decompose-complex-polygon-nondisjoint (polygon)
  "Decomposes a complex polygon into a set of simple ones, possibly some entirely contained in others."
  (let ((ring-head (double-linked-ring-from-point-list polygon)))
    (let ((ring-index (collect-ring-nodes ring-head)))
      