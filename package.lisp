(defpackage :2d-geometry
  (:nicknames :geometry)
  (:use :common-lisp :iterate :ramarren-util)
  (:export #:distance
	   #:circumference-circle
	   #:area-circle
	   #:area-ellipse-axes
	   #:circumference-ellipse-axes
	   #:perimeter-triangle
	   #:area-triangle-edge-edge-angle
	   #:area-triangle-edges
	   #:area-triangle-edges-small-angles
	   #:area-triangle-vertices
	   #:area-rectangle
	   #:perimeter-rectangle
	   #:area-square
	   #:perimeter-square
	   #:point
	    #:x #:y
	    #:point-equal-p
	   #:line-segment
	    #:start
	    #:end
	   #:line
	    #:A #:B #:C
	   #:line-y-at-x #:line-x-at-y
	   #:line-from-segment
	   #:line-segment-length
	   #:lines-parralel-p
	   #:lines-equal-p
	   #:lines-intersection-point
	   #:line-segments-intersection-segment
	   #:line-segments-intersection-segment
	   #:line-segments-intersection-point
	   #:polygon-orientation
	   #:area-simple-polygon
	   #:point-in-polygon-crossing-p
	   #:point-in-polygon-winding-number
	   #:point-in-polygon-winding-p
	   #:triangulate
	   #:colinear-p
	   #:decompose-complex-polygon-nondisjoint
	   #:simple-polygon-p
	   #:simple-polygon-sh-p
	   #:frustrated-polygon-p
	   #:shamos-hoey))

