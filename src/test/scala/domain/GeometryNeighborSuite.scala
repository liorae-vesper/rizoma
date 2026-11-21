package rizoma
package domain

class GeometryNeighborSuite extends GeometryRelationsFixture:
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
