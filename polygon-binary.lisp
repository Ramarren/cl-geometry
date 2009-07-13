(in-package :2d-geometry)

;;;; This file implements union, intersection and difference of polygons using triangulation of edge sets.

(defun merge-line-segment-into (ls1 ls2)
  "If two segments are colinear and intersect, extends the first one to include the second. Reorients the first edge to the left."
  (if (line-segments-intersection-segment ls1 ls2)
      (let ((left-ls1 (left-endpoint ls1))
            (left-ls2 (left-endpoint ls2))
            (right-ls1 (right-endpoint ls1))
            (right-ls2 (right-endpoint ls2)))
        (setf (start ls1) (if (point-sort-fun left-ls1 left-ls2)
                              left-ls1
                              left-ls2)
              (end ls1) (if (point-sort-fun right-ls1 right-ls2)
                            right-ls2
                            right-ls1))
        t)
      nil))

(defun sanitize-edges (edge-list acc)
  "Drop zero length edges and merge all segment intersecting edges."
  (if (null edge-list)
      (nreverse acc)
      (let ((head (car edge-list))
            (rst (cdr edge-list))
            (racc nil))
        (if (point-equal-p (start head) (end head))
            (sanitize-edges rst acc)
            (progn
              (dolist (tk rst)
                (unless (merge-line-segment-into head tk)
                  (push tk racc)))
              (sanitize-edges racc (cons head acc)))))))

(defun polygon-binary (polygon1 polygon2 triangle-test)
  "Return all triangles fulfilling triangle-test from triangulation of all edges of two polygons."
  (let ((edge-list (sanitize-edges (append (edge-list-from-point-list polygon1)
                                           (edge-list-from-point-list polygon2))
                                   nil)))
    (let ((trapez (trapezoidize-edges edge-list)))
        (let ((triangles (trapezoids-to-triangles trapez)))
          (remove-if-not triangle-test triangles)))))

(defun polygon-union (polygon1 polygon2 &key (in-test 'point-in-polygon-winding-p) (in-test-1 nil) (in-test-2 nil))
  "Return triangles of an union of two polygons."
  (let ((in-1 (if in-test-1 in-test-1 in-test))
        (in-2 (if in-test-2 in-test-2 in-test)))
    (polygon-binary polygon1 polygon2 #'(lambda (x)
                                          (or (funcall in-1 (triangle-center-point x) polygon1)
                                              (funcall in-2 (triangle-center-point x) polygon2))))))

(defun polygon-intersection (polygon1 polygon2 &key (in-test 'point-in-polygon-winding-p) (in-test-1 nil) (in-test-2 nil))
  "Return triangles of an intersection of two polygons."
  (let ((in-1 (if in-test-1 in-test-1 in-test))
        (in-2 (if in-test-2 in-test-2 in-test)))
    (polygon-binary polygon1 polygon2 #'(lambda (x)
                                          (and (funcall in-1 (triangle-center-point x) polygon1)
                                               (funcall in-2 (triangle-center-point x) polygon2))))))

(defun polygon-difference (polygon1 polygon2 &key (in-test 'point-in-polygon-winding-p) (in-test-1 nil) (in-test-2 nil))
  "Return triangles of polygon1 minus polygon2."
  (let ((in-1 (if in-test-1 in-test-1 in-test))
        (in-2 (if in-test-2 in-test-2 in-test)))
    (polygon-binary polygon1 polygon2 #'(lambda (x)
                                          (and (funcall in-1 (triangle-center-point x) polygon1)
                                               (not (funcall in-2 (triangle-center-point x) polygon2)))))))

(defun polygon-difference-nary (polygon &rest holes &key (in-test 'point-in-polygon-winding-p))
  "Return triangles of polygon with some holes."
  (let ((edge-list (sanitize-edges (append (edge-list-from-point-list polygon)
                                           (reduce #'append
                                                   (mapcar #'edge-list-from-point-list
                                                           holes))) nil)))
    (let ((trapez (trapezoidize-edges edge-list)))
      (let ((triangles (trapezoids-to-triangles trapez)))
        (remove-if-not #'(lambda (x)
                           (let ((center-point (triangle-center-point x)))
                             (and (funcall in-test center-point polygon)
                                  (every #'(lambda (hole)
                                             (not (funcall in-test center-point hole)))
                                         holes))))
                       triangles)))))
