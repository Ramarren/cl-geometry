(in-package :2d-geometry)

;;;; This file implements triangulation.


(defun possible-diagonal-p (ring-diag)
  "Checks if ring-diag does not intersect any edge in a ring."
  (not (iterate (for node initially (start ring-diag) then (next-node node))
                (until (and (eql node (start ring-diag))(not (first-iteration-p))))
                (reducing (and (not (eql (start ring-diag) node))
                               (not (eql (end ring-diag) node))
                               (not (eql (start ring-diag) (next-node node)))
                               (not (eql (end ring-diag) (next-node node)))
                               (intersect-p (start ring-diag)(end ring-diag) node (next-node node)))
                          by #'or initial-value nil))))

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

(defun diagonal-p (ring-node-a ring-node-b)
  "Is a line segment between two nodes a diagonal of polygon with edges edge-list?"
  (and (in-cone-p ring-node-a (val ring-node-b))
       (in-cone-p ring-node-b (val ring-node-a))
       (possible-diagonal-p (make-instance 'line-segment
                                           :start ring-node-a
                                           :end ring-node-b))))

;;; ear removal method - O(2)

(defclass ear-ring-node (poly-ring-node)
  ((ear :accessor ear :initarg :ear))
  (:documentation "Ring node with ear information."))

(defun ear-init (point-list)
  "Takes a list of points and creates a ring initialized with ear data."
  (let ((ring-head (double-linked-ring-from-point-list point-list 'ear-ring-node)))
    (iterate (for node initially ring-head then (next-node node))
             (until (and (eq node ring-head)
                         (not (first-iteration-p))))
             (setf (ear node) (diagonal-p (prev-node node)
                                          (next-node node))))
    ring-head))

(defun remove-ear (node)
  "Remove an ear centered on node from ring, returning new node, the removed ear and new edge list."
  (let ((v2 node))
    (let ((v1 (prev-node v2))
          (v3 (next-node v2)))
      (let ((v0 (prev-node v1))
            (v4 (next-node v3)))
        (let ((ear (list (val v2)(val v1)(val v3))))
          (setf (next-node v1) v3
                (prev-node v3) v1
                (ear v1) (diagonal-p v0 v3)
                (ear v3) (diagonal-p v1 v4))
          (values v3 ear))))))

(defun triangulate (polygon)
  "Triangulate polygon. Returns list of triangles."
  (let ((point-list (point-list polygon)))
   (let ((num-vertices (length point-list))
         (ear-list))
     (let ((ring-head (ear-init point-list)))
       (iterate (while (> num-vertices 3))
                (with node = ring-head)
                (if (ear node)
                    (multiple-value-bind (new-node ear) (remove-ear node)
                      (setf node new-node)
                      (decf num-vertices)
                      (push ear ear-list))
                    (setf node (next-node node)))
                (finally (push (point-list-from-ring node) ear-list))))
     (mapcar #'make-polygon-from-point-list ear-list))))