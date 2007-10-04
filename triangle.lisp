(in-package :ximage-triangle)

(defun triangle-area (x1 y1 x2 y2 x3 y3)
  (* 0.5 (abs (+ (- (* x2 y1) (* x1 y2))
		 (- (* x3 y2) (* x2 y3))
		 (- (* x1 y3) (* x3 y1))))))

(defun quadrilateral-area (x1 y1 x2 y2 x3 y3 x4 y4)
  (+ (triangle-area x1 y1 x2 y2 x3 y3)
     (triangle-area x1 y1 x3 y3 x4 y4)))

(defun intersect-x (x x1 y1 x2 y2)
  (+ y1 (/ (* (- x x1) (- y2 y1)) (- x2 x1))))

(defun intersect-y (y x1 y1 x2 y2)
  (+ x1 (/ (* (- y y1) (- x2 x1)) (- y2 y1))))

(defun add-area-to-pixel (x y area)
  (format t "x: ~a y:~a value: ~a~%" x y area))

;;; add the area indicated by the corners of an
;;; arbitrary triangle within a pixel to that pixel 
(defun triangle-in-pixel (x y x1 y1 x2 y2 x3 y3 fun)
  (funcall fun
   x y (triangle-area x1 y1 x2 y2 x3 y3)))

(defun quadrilateral-in-pixel (x y x1 y1 x2 y2 x3 y3 x4 y4 fun)
  (funcall fun
   x y (quadrilateral-area x1 y1 x2 y2 x3 y3 x4 y4)))

;;; render an area defined by the left and right edges of a scan line
;;; and by two nonintersecting lines both running from the left edge to
;;; the right edge of the scan line
;;; left-base is the base of the trapezoid at the left edge
;;; right-base is the base of the trapezoid at the left edge
(defun trapezoid-within-scanline (minx maxx y left-base right-base fun)
  (when (= minx maxx)
    (return-from trapezoid-within-scanline))
  ;; divide by two to avoid doing that in the loop
  (assert (>= left-base 0))
  (assert (>= right-base 0))
  (setf left-base (* 0.5 left-base))
  (setf right-base (* 0.5 right-base))
  (let ((db (/ (- right-base left-base) (- maxx minx))))
    (loop for x from minx below maxx
	  for lb = left-base then (+ lb db)
	  for rb = (+ left-base db) then (+ rb db)
	  do (funcall fun x y (+ lb rb)))))

(defun render-triangle-three-points-in-scanline (y x1 y1 x2 y2 x3 y3 fun)
  (let ((minx (floor (min x1 x2 x3)))
	(maxx (1- (ceiling (max x1 x2 x3)))))
    (if (= minx maxx)
	;; all three points are in a single pixel
	(triangle-in-pixel minx y x1 y1 x2 y2 x3 y3 fun)
	(progn
	  ;; sort the points by x coordinate
	  (when (< x2 x1)
	    (rotatef x1 x2)
	    (rotatef y1 y2))
	  (when (< x3 x1)
	    (rotatef x1 x3)
	    (rotatef y1 y3))
	  (when (< x3 x2)
	    (rotatef x2 x3)
	    (rotatef y2 y3))
	  (cond ((<= x2 (1+ minx))
		 ;; there are two corners in the first pixel
		 ;; (and one in the last pixel)
		 (let ((yyl1 (intersect-x (1+ minx) x1 y1 x3 y3))
		       (yyl2 (intersect-x (1+ minx) x2 y2 x3 y3))
		       (yyr1 (intersect-x maxx x1 y1 x3 y3))
		       (yyr2 (intersect-x maxx x2 y2 x3 y3)))
		   (quadrilateral-in-pixel
		    minx y x1 y1 (1+ minx) yyl1 (1+ minx) yyl2 x2 y2 fun)
		   (trapezoid-within-scanline
		    (1+ minx) maxx y (abs (- yyl1 yyl2)) (abs (- yyr1 yyr2)) fun)
		   (triangle-in-pixel maxx y maxx yyr1 maxx yyr2 x3 y3 fun)))
		((>= x2 maxx)
		 ;; there are two corners in the last pixel
		 ;; (and one in the first pixel)
		 (let ((yyl2 (intersect-x (1+ minx) x1 y1 x2 y2))
		       (yyl3 (intersect-x (1+ minx) x1 y1 x3 y3))
		       (yyr2 (intersect-x maxx x1 y1 x2 y2))
		       (yyr3 (intersect-x maxx x1 y1 x3 y3)))
		   (triangle-in-pixel minx y x1 y1 (1+ minx) yyl2 (1+ minx) yyl3 fun)
		   (trapezoid-within-scanline
		    (1+ minx) maxx y (abs (- yyl2 yyl3)) (abs (- yyr2 yyr3)) fun)
		   (quadrilateral-in-pixel
		    maxx y maxx yyr2 x2 y2 x3 y3 maxx yyr3 fun)))
		(t
		 ;; there is one corner in the first pixel,
		 ;; one corner in the last pixel,
		 ;; and a third corner somewhere in the middle
		 ;; divide the triangle into two parts
		 (let ((yy (intersect-x x2 x1 y1 x3 y3)))
		   (render-triangle-three-points-in-scanline y x1 y1 x2 y2 x2 yy fun)
		   (render-triangle-three-points-in-scanline y x3 y3 x2 y2 x2 yy fun))))))))

(defun render-triangle-two-points-in-scanline (y x1 y1 x2 y2 x3 y3 fun)
  ;; sort points by y coordinate
  (when (< y2 y1)
    (rotatef x1 x2)
    (rotatef y1 y2))
  (when (< y3 y1)
    (rotatef x1 x3)
    (rotatef y1 y3))
  (when (< y3 y2)
    (rotatef x2 x3)
    (rotatef y2 y3))
  ;; now it is either (x1,y1) or (x3,y3) that is outside the scan line
  (if (< y1 (floor y2))
      ;; (x1,y1) is outside
      (let ((xx2 (intersect-y y x1 y1 x2 y2))
	    (xx3 (intersect-y y x1 y1 x3 y3)))
	(render-triangle-three-points-in-scanline
	 y xx2 y x3 y3 x2 y2 fun)
	(render-triangle-three-points-in-scanline
	 y xx3 y xx2 y x3 y3 fun))
      ;; (x3,y3) is outside
      (let ((xx1 (intersect-y (1+ y) x3 y3 x1 y1))
	    (xx2 (intersect-y (1+ y) x3 y3 x2 y2)))
	(render-triangle-three-points-in-scanline
	 y xx1 (1+ y) x1 y1 x2 y2 fun)
	(render-triangle-three-points-in-scanline
	 y xx1 (1+ y) xx2 (1+ y) x2 y2 fun))))

(defun render-triangle-one-point-in-scanline (y x1 y1 x2 y2 x3 y3 fun)
  ;; sort points by y coordinate
  (when (< y2 y1)
    (rotatef x1 x2)
    (rotatef y1 y2))
  (when (< y3 y1)
    (rotatef x1 x3)
    (rotatef y1 y3))
  (when (< y3 y2)
    (rotatef x2 x3)
    (rotatef y2 y3))
  (cond ((>= y1 y)
	 ;; (x1,y1) is in the scanline
	 (let ((xx2 (intersect-y (1+ y) x1 y1 x2 y2))
	       (xx3 (intersect-y (1+ y) x1 y1 x3 y3)))
	   (render-triangle-three-points-in-scanline
	    y x1 y1 xx2 (1+ y) xx3 (1+ y) fun)))
	((>= y2 y)
	 ;; (x2,y2) is in the scanline
	 (let ((x12 (intersect-y y x1 y1 x2 y2))
	       (x23 (intersect-y (1+ y) x2 y2 x3 y3))
	       (xu (intersect-y y x1 y1 x3 y3))
	       (xl (intersect-y (1+ y) x1 y1 x3 y3)))
	   (render-triangle-three-points-in-scanline
	    y x2 y2 x12 y x23 (1+ y) fun)
	   (render-triangle-three-points-in-scanline
	    y x12 y xu y x23 (1+ y) fun)
	   (render-triangle-three-points-in-scanline
	    y x23 (1+ y) xu y xl (1+ y) fun)))
	(t
	 ;; (x3,y3) is in the scanline
	 (let ((xx1 (intersect-y y x1 y1 x3 y3))
	       (xx2 (intersect-y y x2 y2 x3 y3)))
	   (render-triangle-three-points-in-scanline
	    y x3 y3 xx1 y xx2 y fun)))))

(defun render-triangle-no-points-in-scanline (y x1 y1 x2 y2 x3 y3 fun)
  ;; sort points by y coordinate
  (when (< y2 y1)
    (rotatef x1 x2)
    (rotatef y1 y2))
  (when (< y3 y1)
    (rotatef x1 x3)
    (rotatef y1 y3))
  (when (< y3 y2)
    (rotatef x2 x3)
    (rotatef y2 y3))
  (if (< y2 y)
      ;; points 1 and 2 above, point 3 below
      (let ((x13u (intersect-y y x1 y1 x3 y3))
	    (x13l (intersect-y (1+ y) x1 y1 x3 y3))
	    (x23u (intersect-y y x2 y2 x3 y3))	    
	    (x23l (intersect-y (1+ y) x2 y2 x3 y3)))
	(render-triangle-three-points-in-scanline
	 y x13u y x23u y x23l (1+ y) fun)
	(render-triangle-three-points-in-scanline
	 y x13u y x23l (1+ y) x13l (1+ y) fun))
      ;; point 1 above, points 2 and 3 below
      (let ((x12u (intersect-y y x1 y1 x2 y2))
	    (x12l (intersect-y (1+ y) x1 y1 x2 y2))
	    (x13u (intersect-y y x1 y1 x3 y3))	    
	    (x13l (intersect-y (1+ y) x1 y1 x3 y3)))
	(render-triangle-three-points-in-scanline
	 y x12u y x13u y x12l (1+ y) fun)
	(render-triangle-three-points-in-scanline
	 y x13u y x12l (1+ y) x13l (1+ y) fun))))

(defun number-of-points-in-scanline (y y1 y2 y3)
  (let ((result 0))
    (when (<= y y1 (1+ y)) (incf result))
    (when (<= y y2 (1+ y)) (incf result))
    (when (<= y y3 (1+ y)) (incf result))
    result))

(defun render-triangle-scanline (y x1 y1 x2 y2 x3 y3 fun)
  (ecase (number-of-points-in-scanline y y1 y2 y3)
    (0 (render-triangle-no-points-in-scanline y x1 y1 x2 y2 x3 y3 fun))
    (1 (render-triangle-one-point-in-scanline y x1 y1 x2 y2 x3 y3 fun))
    (2 (render-triangle-two-points-in-scanline y x1 y1 x2 y2 x3 y3 fun))))

(defun points-are-aligned-p (x1 y1 x2 y2 x3 y3)
  (= (* (- y2 y1) (- x3 x1))
     (* (- y3 y1) (- x2 x1))))

(defun render-triangle (x1 y1 x2 y2 x3 y3 &optional (fun #'add-area-to-pixel))
  (unless (points-are-aligned-p x1 y1 x2 y2 x3 y3)
    (let ((miny (floor (min y1 y2 y3)))
	  (maxy (ceiling (1- (max y1 y2 y3)))))
      (if (= miny maxy)
	  (render-triangle-three-points-in-scanline miny x1 y1 x2 y2 x3 y3 fun)
	  (loop for y from miny to maxy
		do (render-triangle-scanline y x1 y1 x2 y2 x3 y3 fun))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests
;;;
;;; the tests seems to indicate significant differences for very small
;;; triangles, but not for large ones.


(defun test-render-triangle-three-points-in-scanline (n)
  (loop repeat n
	do (let ((y (random 10))
		 (total-area 0))
	     (flet ((add-area-to-pixel (x y area)
		      (declare (ignore x y))
		      (incf total-area area)))
	       (let ((y1 (+ y (random 1.0)))
		     (y2 (+ y (random 1.0)))
		     (y3 (+ y (random 1.0)))
		     (x1 (random 10.0))
		     (x2 (random 10.0))
		     (x3 (random 10.0)))
		 (render-triangle x1 y1 x2 y2 x3 y3 #'add-area-to-pixel)
		 (unless (<= 0.99 (/ (triangle-area x1 y1 x2 y2 x3 y3) total-area) 1.01)
		   (format t "area: ~s  sum of areas: ~s~%"
			   (triangle-area x1 y1 x2 y2 x3 y3)
			   total-area)))))))

(defun test-render-triangle (n)
  (loop repeat n
	do (let ((total-area 0))
	     (flet ((add-area-to-pixel (x y area)
		      (declare (ignore x y))
		      (incf total-area area)))
	       (let ((y1 (random 10.0))
		     (y2 (random 10.0))
		     (y3 (random 10.0))
		     (x1 (random 10.0))
		     (x2 (random 10.0))
		     (x3 (random 10.0)))
		 (render-triangle x1 y1 x2 y2 x3 y3 #'add-area-to-pixel)
		 (unless (<= 0.99 (/ (triangle-area x1 y1 x2 y2 x3 y3) total-area) 1.01)
		   (format t "area: ~s  sum of areas: ~s~%"
			   (triangle-area x1 y1 x2 y2 x3 y3)
			   total-area)))))))
