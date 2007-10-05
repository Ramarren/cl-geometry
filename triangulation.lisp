(in-package :2d-geometry)

;;;; This file implements triangulation.

;;;auxiliary functions
;;;a bit chaotic... can't decide on proper representation

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

(defun possible-diagonal-p (diag edge-list)
  "Checks if diag does not intersect any edge in edge list."
  (notany #'(lambda (e)
	      (and (not (point-equal-p (start diag) (start e)))
		   (not (point-equal-p (start diag) (end e)))
		   (not (point-equal-p (end diag) (start e)))
		   (not (point-equal-p (end diag) (end e)))
		   (intersect-p (start diag)(end diag)(start e)(end e))))
	  edge-list))

(defun in-cone-p (ring-node b)
  "Is line segment ring-node->b in cone defined by angle with vertex defined by ring-node?"
  (let ((a- (val (prev-node ring-node)))
	(a (val ring-node))
	(a+ (val (next-node ring-node))))
    (if (left-on-p a a+ a-)
	(and (left-p a b a-)
	     (left-p b a a+))
	(not (and (left-on-p a b a+)
		  (left-on-p b a a-))))))

(defun diagonal-p (ring-node-a ring-node-b edge-list)
  "Is a line segment between two nodes a diagonal of polygon with edges edge-list?"
  (and (in-cone-p ring-node-a (val ring-node-b))
       (in-cone-p ring-node-b (val ring-node-a))
       (possible-diagonal-p (make-instance 'line-segment
					   :start (val ring-node-a)
					   :end (val ring-node-b))
			    edge-list)))

;;; ear removal method - O(2)

(defclass ear-ring-node (poly-ring-node)
  ((ear :accessor ear :initarg :ear))
  (:documentation "Ring node with ear information."))

(defun ear-init (polygon)
  "Takes a list of points and creates a ring initialized with ear data."
  (let ((edge-list (edge-list-from-point-list polygon))
	(ring-head (double-linked-ring-from-point-list polygon 'ear-ring-node)))
    (iterate (for node initially ring-head then (next-node node))
	     (until (and (eq node ring-head)
			 (not (first-iteration-p))))
	     (setf (ear node) (diagonal-p (prev-node node)
					  (next-node node)
					  edge-list)))
    (values ring-head edge-list)))

(defun remove-ear (node edge-list)
  (let ((v2 node))
    (let ((v1 (prev-node v2))
	  (v3 (next-node v2)))
      (let ((v0 (prev-node v1))
	    (v4 (next-node v3)))
	(let ((ear (list (val v2)(val v1)(val v3))))
	  (let ((new-edge-list (cons (make-instance 'line-segment
						    :start (val v1)
						    :end (val v3))
				     (remove-if #'(lambda (edge)
						    (or (point-equal-p (val v2) (start edge))
							(point-equal-p (val v2) (end edge))))
						edge-list))))
	    (setf (next-node v1) v3
		  (prev-node v3) v1
		  (ear v1) (diagonal-p v0 v3 new-edge-list)
		  (ear v3) (diagonal-p v1 v4 new-edge-list))
	    (values v3 ear new-edge-list)))))))

(defun triangulate (polygon)
  "Triangulate polygon. Returns list of triangles."
  (let ((num-vertices (length polygon))
	(ear-list))
    (multiple-value-bind (ring-head edge-list) (ear-init polygon)
      (iterate (while (> num-vertices 3))
	       (with node = ring-head)
	       (if (ear node)
		   (multiple-value-bind (new-node ear new-edge-list) (remove-ear node edge-list)
		       (setf node new-node
			     edge-list new-edge-list)
		       (decf num-vertices)
		       (push ear ear-list))
		   (setf node (next-node node)))
	       (finally (push (point-list-from-ring node) ear-list))))
    ear-list))