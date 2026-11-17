package rizoma
package domain

import io.circe.parser.decode

class GeometryRelationsSuite extends munit.FunSuite:
  private def geometry(kind: String, coordinates: String): Geometry =
    decode[Geometry](s"""{"type":"$kind","coordinates":$coordinates}""")
      .fold(error => fail(error.getMessage), identity)

  private def point(x: Double, y: Double): Geometry = geometry("Point", s"[$x,$y]")

  private def collection(members: Geometry*): Geometry =
    GeometryCollection.from(members.toList).getOrElse(fail("Invalid geometry collection"))

  private val square = geometry("Polygon", "[[[0,0],[10,0],[10,10],[0,10],[0,0]]]")
  private val inside = geometry("Polygon", "[[[2,2],[4,2],[4,4],[2,4],[2,2]]]")
  private val outside = geometry("Polygon", "[[[20,20],[30,20],[30,30],[20,30],[20,20]]]")
  private val withHole = geometry(
    "Polygon",
    "[[[0,0],[10,0],[10,10],[0,10],[0,0]],[[3,3],[3,7],[7,7],[7,3],[3,3]]]"
  )

  test("contains accepts an interior point and polygon, but is directional"):
    assert(square.contains(point(5, 5)))
    assert(square.contains(inside))
    assert(!inside.contains(square))
    assert(square.intersects(inside))
    assert(inside.intersects(square))

  test("disjoint geometries neither contain nor intersect one another"):
    assert(!square.contains(outside))
    assert(!square.intersects(outside))
    assert(!outside.intersects(square))

  test("overlapping polygons intersect without containing each other"):
    val overlapping = geometry("Polygon", "[[[5,5],[15,5],[15,15],[5,15],[5,5]]]")
    assert(square.intersects(overlapping))
    assert(overlapping.intersects(square))
    assert(!square.contains(overlapping))
    assert(!overlapping.contains(square))

  test("points on a polygon edge or vertex intersect but are not contained"):
    List(point(0, 5), point(0, 0)).foreach { boundary =>
      assert(!square.contains(boundary))
      assert(square.intersects(boundary))
      assert(boundary.intersects(square))
    }

  test("polygons touching at an edge or a vertex intersect without containment"):
    List(
      geometry("Polygon", "[[[10,0],[20,0],[20,10],[10,10],[10,0]]]"),
      geometry("Polygon", "[[[10,10],[20,10],[20,20],[10,20],[10,10]]]")
    ).foreach { touching =>
      assert(square.intersects(touching))
      assert(!square.contains(touching))
    }

  test("a line on a polygon boundary intersects but is not contained"):
    val edge = geometry("LineString", "[[0,2],[0,8]]")
    assert(square.intersects(edge))
    assert(!square.contains(edge))

  test("a line with boundary endpoints and an interior segment is contained"):
    val crossingInterior = geometry("LineString", "[[0,5],[10,5]]")
    assert(square.contains(crossingInterior))
    assert(square.intersects(crossingInterior))

  test("polygon holes exclude points and polygons from containment and intersection"):
    List(
      point(5, 5),
      geometry("Polygon", "[[[4,4],[6,4],[6,6],[4,6],[4,4]]]")
    ).foreach { inHole =>
      assert(!withHole.contains(inHole))
      assert(!withHole.intersects(inHole))
      assert(!inHole.intersects(withHole))
    }
    assert(withHole.contains(point(1, 1)))

  test("a hole boundary intersects but is not contained"):
    assert(withHole.intersects(point(3, 5)))
    assert(!withHole.contains(point(3, 5)))

  test("a polygon spanning a hole is not contained even when all its vertices are inside"):
    val spanning = geometry("Polygon", "[[[2,2],[8,2],[8,8],[2,8],[2,2]]]")
    assert(!withHole.contains(spanning))
    assert(withHole.intersects(spanning))

  test("containment checks whole segments rather than just their endpoints"):
    val concave = geometry("Polygon", "[[[0,0],[6,0],[6,6],[4,6],[4,2],[2,2],[2,6],[0,6],[0,0]]]")
    val bridging = geometry("LineString", "[[1,5],[5,5]]")
    assert(concave.contains(point(1, 5)))
    assert(concave.contains(point(5, 5)))
    assert(!concave.contains(bridging))
    assert(concave.intersects(bridging))

  test("overlapping bounding boxes alone do not imply an intersection"):
    val triangle = geometry("Polygon", "[[[0,0],[10,0],[0,10],[0,0]]]")
    assert(!triangle.intersects(point(8, 8)))
    assert(!triangle.contains(point(8, 8)))

  test("crossing lines intersect even when none of their endpoints coincide"):
    val horizontal = geometry("LineString", "[[0,5],[10,5]]")
    val vertical = geometry("LineString", "[[5,0],[5,10]]")
    assert(horizontal.intersects(vertical))
    assert(vertical.intersects(horizontal))
    assert(!horizontal.contains(vertical))

  test("collinear lines distinguish containment, overlap, and touching endpoints"):
    val line = geometry("LineString", "[[0,0],[10,0]]")
    val segment = geometry("LineString", "[[2,0],[8,0]]")
    val overlap = geometry("LineString", "[[5,0],[15,0]]")
    val touching = geometry("LineString", "[[10,0],[15,0]]")
    assert(line.contains(segment))
    assert(line.intersects(overlap))
    assert(!line.contains(overlap))
    assert(line.intersects(touching))
    assert(!line.contains(touching))
    assert(!line.contains(point(0, 0)))
    assert(line.contains(point(5, 0)))

  test("equal points contain and intersect each other, while distinct points do not"):
    val first = point(1, 2)
    assert(first.contains(point(1, 2)))
    assert(first.intersects(point(1, 2)))
    assert(!first.contains(point(2, 1)))
    assert(!first.intersects(point(2, 1)))

  test("spatial predicates ignore altitude"):
    val lower = geometry("Point", "[1,2,0]")
    val upper = geometry("Point", "[1,2,100]")
    assert(lower.contains(upper))
    assert(lower.intersects(upper))

  test("longitude and latitude are treated as planar coordinates without antimeridian wrapping"):
    val line = geometry("LineString", "[[179,0],[-179,0]]")
    assert(line.contains(point(0, 0)))
    assert(!line.intersects(point(180, 0)))

  test("zero-length lines are treated as points"):
    val collapsed = geometry("LineString", "[[1,2],[1,2]]")
    assert(collapsed.contains(point(1, 2)))
    assert(point(1, 2).contains(collapsed))
    assert(collapsed.intersects(point(1, 2)))
    assert(!collapsed.intersects(point(1, 3)))

  test("multipoints require all points for containment and any point for intersection"):
    val allInside = geometry("MultiPoint", "[[1,1],[2,2]]")
    val partlyInside = geometry("MultiPoint", "[[1,1],[20,20]]")
    assert(square.contains(allInside))
    assert(!square.contains(partlyInside))
    assert(square.intersects(partlyInside))
    assert(partlyInside.intersects(square))
    assert(allInside.contains(point(1, 1)))

  test("multilines require all segments for containment and any segment for intersection"):
    val allInside = geometry("MultiLineString", "[[[1,1],[2,2]],[[3,3],[4,4]]]")
    val partlyInside = geometry("MultiLineString", "[[[1,1],[2,2]],[[20,20],[30,30]]]")
    assert(square.contains(allInside))
    assert(!square.contains(partlyInside))
    assert(square.intersects(partlyInside))
    assert(partlyInside.intersects(square))
    assert(allInside.contains(point(1.5, 1.5)))

  test("multipolygons consider every polygon and preserve gaps between them"):
    val multi = geometry(
      "MultiPolygon",
      "[[[[0,0],[4,0],[4,4],[0,4],[0,0]]],[[[6,0],[10,0],[10,4],[6,4],[6,0]]]]"
    )
    assert(multi.contains(geometry("MultiPoint", "[[2,2],[8,2]]")))
    assert(multi.intersects(point(8, 2)))
    assert(!multi.intersects(point(5, 2)))
    assert(!multi.contains(geometry("LineString", "[[2,2],[8,2]]")))
    assert(square.intersects(multi))

  test("nested mixed geometry collections work as both predicate operands"):
    val mixed =
      collection(point(20, 20), collection(inside, geometry("LineString", "[[30,30],[40,40]]")))
    assert(mixed.contains(inside))
    assert(mixed.intersects(square))
    assert(square.intersects(mixed))
    assert(!square.contains(mixed))
    assert(!mixed.contains(square))

  test(
    "geometry collections use union semantics rather than requiring one member to contain the target"
  ):
    val left = geometry("Polygon", "[[[0,0],[6,0],[6,10],[0,10],[0,0]]]")
    val right = geometry("Polygon", "[[[4,0],[10,0],[10,10],[4,10],[4,0]]]")
    val combined = collection(left, collection(right))
    val spanning = geometry("LineString", "[[2,5],[8,5]]")
    assert(!left.contains(spanning))
    assert(!right.contains(spanning))
    assert(combined.contains(spanning))
    assert(combined.intersects(spanning))
    assert(square.contains(combined))

  test("unrelated points inside polygon holes do not prevent collection containment"):
    val mixed = collection(withHole, point(5, 5), geometry("MultiPoint", "[[4,4],[20,20]]"))
    assert(mixed.contains(withHole))
    assert(!mixed.contains(square))
    assert(mixed.contains(point(5, 5)))

  test("containment retains point components required by a mixed target collection"):
    val source = collection(square, geometry("MultiPoint", "[[20,20],[30,30]]"))
    assert(source.contains(collection(inside, point(20, 20))))
    assert(!source.contains(collection(inside, point(40, 40))))

  test("empty geometries neither contain nor intersect anything, including other empty geometries"):
    val empties = List(
      geometry("MultiPoint", "[]"),
      geometry("MultiLineString", "[]"),
      geometry("Polygon", "[]"),
      geometry("MultiPolygon", "[]"),
      geometry("MultiPolygon", "[[]]"),
      collection(),
      collection(collection(), geometry("Polygon", "[]"))
    )
    empties.foreach { empty =>
      assert(!empty.contains(square))
      assert(!square.contains(empty))
      assert(!empty.intersects(square))
      assert(!square.intersects(empty))
      empties.foreach { other =>
        assert(!empty.contains(other))
        assert(!empty.intersects(other))
      }
    }

  test("nonempty geometries contain and intersect themselves"):
    List(
      point(1, 1),
      square,
      withHole,
      geometry("LineString", "[[0,0],[10,10]]"),
      geometry("MultiPoint", "[[1,1],[2,2]]"),
      geometry("MultiLineString", "[[[0,0],[1,1]],[[2,2],[3,3]]]"),
      geometry("MultiPolygon", "[[[[0,0],[1,0],[1,1],[0,1],[0,0]]]]"),
      collection(square, point(20, 20))
    ).foreach { value =>
      assert(value.contains(value))
      assert(value.intersects(value))
    }

  test("neighbors share a boundary without intersecting interiors"):
    val edge = geometry("Polygon", "[[[10,0],[20,0],[20,10],[10,10],[10,0]]]")
    val vertex = geometry("Polygon", "[[[10,10],[20,10],[20,20],[10,20],[10,10]]]")
    val overlap = geometry("Polygon", "[[[5,5],[15,5],[15,15],[5,15],[5,5]]]")
    List(edge, vertex, point(0, 5), geometry("LineString", "[[0,2],[0,8]]")).foreach { other =>
      assert(square.isNeighbor(other))
      assert(other.isNeighbor(square))
    }
    List(inside, overlap, outside, square, geometry("Polygon", "[]")).foreach { other =>
      assert(!square.isNeighbor(other))
      assert(!other.isNeighbor(square))
    }
    assert(withHole.isNeighbor(point(3, 5)))
    assert(!point(1, 1).isNeighbor(point(1, 1)))
    assert(!collection(edge, inside).isNeighbor(square))
