(in-package :2d-geometry)

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

(defun possible-diagonal-p (diag edge-list)
  (notany #'(lambda (d e)
	      (and (not (or (point-equal-p (start d) (start e))
			    (point-equal-p (start d) (end e))
			    (point-equal-p (end d) (start e))
			    (point-equal-p (end d) (end e))))
		   (line-segments-intersection-point d e)))
	  edge-list))