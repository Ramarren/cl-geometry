(in-package :2d-geometry)

;;;; This file defines functions manipulating representation of geometric data.

(defun edge-list-from-point-list (polygon &optional (edge-type 'line-segment))
  "Change polygon represented as a list of points into a list of edges (line segments)."
  (let ((vertex-zero (car polygon)))
    (maplist #'(lambda (lst)
		 (if (null (cadr lst))
		     (make-instance edge-type :start (car lst) :end vertex-zero)
		     (make-instance edge-type :start (car lst) :end (cadr lst))))
	     polygon)))

(defclass poly-ring-node ()
  ((val :accessor val :initarg :val)
   (next :accessor next-node :initarg :next)
   (prev :accessor prev-node :initarg :prev))
  (:documentation "Double linked ring node."))

(defmethod print-object ((object poly-ring-node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "NODE: ~a" (val object))))

(defun double-linked-ring-from-point-list (polygon &optional (ring-type 'poly-ring-node))
  "Change polygon representation from list of points to double linked ring of points."
  (let ((head (make-instance ring-type)))
    (let ((tail head))
      (dolist (tk polygon)
	(setf (val tail) tk
	      (next-node tail) (make-instance ring-type)
	      (prev-node (next-node tail)) tail
	      tail (next-node tail)))
      (setf (prev-node head) (prev-node tail)
	    (next-node (prev-node tail)) head))
    head))

(defun collect-ring-nodes (ring)
  "Construct a list of all nodes in a ring."
  (labels ((recurse-ring (node head acc)
	     (if (eq node head)
		 (nreverse acc)
		 (recurse-ring (next-node node) head (cons node acc)))))
    (cons ring (recurse-ring (next-node ring) ring nil))))

(defun point-list-from-ring (ring-node)
  "Return a list of points in a ring."
  (let ((all-nodes (collect-ring-nodes ring-node)))
    (mapcar #'val all-nodes)))

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
