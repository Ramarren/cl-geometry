# cl-geometry

This is a system for two dimensional computational geometry for Common Lisp. 

## Notes

The system assumes exact rational arithmetic, so no floating point coordinates are allowed. This is not checked when creating geometric objects.

The system was not heavily tested or used. Comments and/or patches welcome.

## Classes

- point
- line-segment (ie. an ordered set of two points)
- line (an infinite geometric object given by equation Ax+By+C=0)
- polygon (an ordered set of n points)

## Decomposition

Most functions are explained by their docstrings. The most complex and somewhat chaotic are decomposition/triangulation functions.

`triangulate polygon`

Returns a set of triangles obtained using ear-removal method. This works only on simple polygons (ie. nonself-intersecting), and is quadratic in the number of edges.

`decompose-complex-polygon-nondisjoint polygon`

Returns a set of possibly intersecting simple polygons from a complex one. This is quadratic in the number of edges at least. The only guarantee is that the union of edges of returned polygons form original polygon, in particular, this does no form of interior testing.

`shamos-hoey edge-list`

Takes a list of edges (line segments) and returns true if there is at least one intersection.

`bentley-ottmann edge-list`

Takes a list of edges and returns all intersection points.

`decompose-complex-polygon-bentley-ottmann polygon`

Returns a set of possibly intersecting simple polygons. This is similar to `decompose-complex-polygon-nondisjoint` above but should be faster.

`decompose-complex-polygon-triangles polygon &key (in-test 'point-in-polygon-winding-p)`

Returns a set of triangles forming the, possibly complex, polygon which fulfill the `in-test`, which is a function of two arguments, a center point of a given triangle and a polygon. By default winding number interior test is used. The other standard test is even-odd rule implemented by `point-in-polygon-crossing-p`. Using `(constantly t)` will return all triangles forming the polygon and its bounding box.

    polygon-union polygon1 polygon2 
      &key (in-test 'point-in-polygon-winding-p) (in-test-1 nil) (in-test-2 nil)
    polygon-intersection polygon1 polygon2 
      &key (in-test 'point-in-polygon-winding-p) (in-test-1 nil) (in-test-2 nil)
    polygon-difference polygon1 polygon2 
      &key (in-test 'point-in-polygon-winding-p) (in-test-1 nil) (in-test-2 nil)

Returns a set of triangles which form two polygons (and their bounding box) and fulfill appropriate interior condition. If `in-test-1` or `in-test-2` are null, `in-test` is used. If both have value, `in-test` is ignored. Tests are function as above. The triangles returned are: for union, that fulfill either test, for intersection that fulfill both tests and difference that succeed first test but fail the second.

`polygon-difference-nary polygon &rest holes &key (in-test 'point-in-polygon-winding-p)`

Returns a set of triangles which succeed `in-test` with `polygon`, but fail with all `holes`.