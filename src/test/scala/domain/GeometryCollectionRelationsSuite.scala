package rizoma
package domain

class GeometryCollectionRelationsSuite extends GeometryRelationsFixture:
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
