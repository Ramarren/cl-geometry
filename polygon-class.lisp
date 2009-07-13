(in-package :2d-geometry)

;;; Treat polygons as immutable object, construct all representations

(defclass polygon ()
  ((point-list :reader point-list :initarg :point-list)
   (edge-list :reader edge-list :initarg :edge-list)
   (point-ring :reader point-ring :initarg :point-ring)))

(defmethod print-object ((object polygon) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "[~a]" (length (edge-list object)))))

(defun make-polygon-from-point-list (point-list)
  (make-instance 'polygon
                 :point-list (copy-seq point-list)
                 :edge-list (edge-list-from-point-list point-list)
                 :point-ring (double-linked-ring-from-point-list point-list)))

(defun make-polygon-from-coords (&rest coord-list)
  (make-polygon-from-point-list (apply #'coords-to-points coord-list)))

(defun make-polygon-from-point-ring (point-ring)
  (let ((point-list (point-list-from-ring point-ring)))
    (make-polygon-from-point-list point-list)))
