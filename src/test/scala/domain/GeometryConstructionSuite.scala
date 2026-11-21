package rizoma
package domain

class GeometryConstructionSuite extends GeometryFixture:
  test("smart constructors reject short lines and invalid rings"):
    val result = for (position, ring) <- fixture yield
      assert(LineString.from(Nil).isEmpty)
      assert(LineString.from(List(position)).isEmpty)
      assert(MultiLineString.from(List(List(position))).isEmpty)
      assert(Polygon.from(ring.dropRight(1)).isEmpty)
      assert(Polygon.from(List(position, position, position)).isEmpty)
      assert(Polygon.from(ring, List(ring.dropRight(1))).isEmpty)
      assert(Polygon.from(Nil, List(ring)).isEmpty)
    assertEquals(result, Some(()))

  test("positions reject non-finite altitude"):
    List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach { altitude =>
      assert(Position.from(longitude = 10, latitude = 20, altitude = Some(altitude)).isEmpty)
    }

  test("geometry constructors cannot be called directly"):
    assert(compileErrors("new Point(null)").nonEmpty)
    assert(compileErrors("Point(null)").nonEmpty)
    assert(compileErrors("new MultiPoint(Nil)").nonEmpty)
    assert(compileErrors("MultiPoint(Nil)").nonEmpty)
    assert(compileErrors("new LineString(Nil)").nonEmpty)
    assert(compileErrors("LineString(Nil)").nonEmpty)
    assert(compileErrors("new MultiLineString(Nil)").nonEmpty)
    assert(compileErrors("MultiLineString(Nil)").nonEmpty)
    assert(compileErrors("new Polygon(Nil, Nil)").nonEmpty)
    assert(compileErrors("Polygon(Nil, Nil)").nonEmpty)
    assert(compileErrors("new MultiPolygon(Nil)").nonEmpty)
    assert(compileErrors("MultiPolygon(Nil)").nonEmpty)
    assert(compileErrors("new GeometryCollection(Nil)").nonEmpty)
    assert(compileErrors("GeometryCollection(Nil)").nonEmpty)
    assert(
      compileErrors(
        "for { lon <- Coordinates.Longitude.from(0); lat <- Coordinates.Latitude.from(0) } yield new Position(lon, lat, None)"
      ).nonEmpty
    )
    assert(
      compileErrors(
        "for { lon <- Coordinates.Longitude.from(0); lat <- Coordinates.Latitude.from(0) } yield Position(lon, lat, None)"
      ).nonEmpty
    )

  test("copy cannot bypass validation"):
    assert(
      compileErrors("for line <- LineString.from(Nil) yield line.copy(coordinates = Nil)").nonEmpty
    )
    assert(
      compileErrors(
        "for polygon <- Polygon.from(Nil) yield polygon.copy(holes = List(Nil))"
      ).nonEmpty
    )
    assert(
      compileErrors(
        "for position <- Position.from(longitude = 0, latitude = 0) yield position.copy(altitude = Some(Double.NaN))"
      ).nonEmpty
    )
