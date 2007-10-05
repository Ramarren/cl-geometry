(in-package :2d-geometry)

;;;; This file implements triangulation.

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
  (if (intersect-proper-p a b c d)
      t
      (or (between-p a b c)
	  (between-p a b d)
	  (between-p c d a)
	  (between-p c d b))))

(defun possible-diagonal-p (diag edge-list)
  (notany #'(lambda (e)
	      (or (point-equal-p (start diag) (start e))
		  (point-equal-p (start diag) (end e))
		  (point-equal-p (end diag) (start e))
		  (point-equal-p (end diag) (end e))
		  (intersect-p (start diag)(end diag)(start e)(end e))))
	  edge-list))

(defun in-cone-p (ring-node b)
  "Is line segment ring-node->b in cone defined by angle with vertex defined by ring-node?"
  (let ((a- (dlist-val (dlist-prev ring-node)))
	(a (dlist-val ring-node))
	(a+ (dlist-val (dlist-next ring-node))))
    (if (left-on-p a a+ a-)
	(and (left-p a b a-)
	     (left-p b a a+))
	(not (and (left-on-p a b a+)
		  (left-on-p b a a-))))))