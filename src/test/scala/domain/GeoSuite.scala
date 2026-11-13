package rizoma
package domain

class GeoSuite extends munit.FunSuite:
  test("latitude accepts the poles"):
    assert(Coordinates.Latitude.from(-90.0).isDefined)
    assert(Coordinates.Latitude.from(90.0).isDefined)

  test("latitude rejects values outside the poles"):
    assert(Coordinates.Latitude.from(-90.0001).isEmpty)
    assert(Coordinates.Latitude.from(90.0001).isEmpty)

  test("latitude rejects non-finite values"):
    List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)
      .foreach(value => assert(Coordinates.Latitude.from(value).isEmpty))

  test("longitude accepts the antimeridian"):
    assert(Coordinates.Longitude.from(-180.0).isDefined)
    assert(Coordinates.Longitude.from(180.0).isDefined)

  test("longitude rejects values outside the antimeridian"):
    assert(Coordinates.Longitude.from(-180.0001).isEmpty)
    assert(Coordinates.Longitude.from(180.0001).isEmpty)

  test("longitude rejects non-finite values"):
    List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)
      .foreach(value => assert(Coordinates.Longitude.from(value).isEmpty))

  test("a geo point is created from valid coordinates"):
    assert(Position.from(-90.0, 180.0).isDefined)

  test("a geo point is rejected when its latitude is invalid"):
    assertEquals(Position.from(90.1, 0.0), None)

  test("a geo point is rejected when its longitude is invalid"):
    assertEquals(Position.from(0.0, -180.1), None)

  private val position = Position.from(20, 10).get
  private val ring = List(position, Position.from(21, 11).get, Position.from(20, 12).get, position)

  test("all geometry types encode their GeoJSON structure and round trip"):
    import io.circe.syntax.*
    val examples: List[(Geometry, String)] = List(
      Point.from(position).get -> """{"type":"Point","coordinates":[10,20]}""",
      MultiPoint.from(List(position)).get -> """{"type":"MultiPoint","coordinates":[[10,20]]}""",
      LineString
        .from(
          List(position, position)
        )
        .get -> """{"type":"LineString","coordinates":[[10,20],[10,20]]}""",
      MultiLineString
        .from(
          List(List(position, position))
        )
        .get -> """{"type":"MultiLineString","coordinates":[[[10,20],[10,20]]]}""",
      Polygon
        .from(
          ring,
          List(ring)
        )
        .get -> """{"type":"Polygon","coordinates":[[[10,20],[11,21],[12,20],[10,20]],[[10,20],[11,21],[12,20],[10,20]]]}""",
      MultiPolygon
        .from(
          List(Polygon.from(ring).get)
        )
        .get -> """{"type":"MultiPolygon","coordinates":[[[[10,20],[11,21],[12,20],[10,20]]]]}""",
      GeometryCollection
        .from(
          List(Point.from(position).get, GeometryCollection.from(Nil).get)
        )
        .get -> """{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[10,20]},{"type":"GeometryCollection","geometries":[]}]}"""
    )
    examples.foreach { (geometry, json) =>
      assertEquals(geometry.asJson, io.circe.parser.parse(json).toOption.get)
      assertEquals(io.circe.parser.decode[Geometry](json), Right(geometry))
    }

  test("concrete codecs reject mismatched geometry types"):
    assert(io.circe.parser.decode[Point]("""{"type":"MultiPoint","coordinates":[10,20]}""").isLeft)

  test("decoders reject malformed coordinates and geometries"):
    List(
      """{"type":"Point","coordinates":[10]}""",
      """{"type":"Point","coordinates":[10,91]}""",
      """{"type":"Point","coordinates":["10",20]}""",
      """{"type":"LineString","coordinates":[[10,20]]}""",
      """{"type":"Polygon","coordinates":[[[0,0],[1,0],[1,1],[0,1]]]}""",
      """{"type":"Polygon","coordinates":[[[0,0],[1,0],[0,0]]]}""",
      """{"type":"Polygon","coordinates":[[]]}""",
      """{"type":"MultiPolygon","coordinates":[[[]]]}""",
      """{"type":"Unknown","coordinates":[]}""",
      """{"type":"GeometryCollection","geometries":[null]}""",
      """{"type":"Point"}"""
    ).foreach(json => assert(io.circe.parser.decode[Geometry](json).isLeft, json))

  test("positions preserve optional altitude"):
    import io.circe.syntax.*
    val json = """{"type":"Point","coordinates":[10,20,30]}"""
    val point = io.circe.parser.decode[Point](json).toOption.get
    assertEquals(point.coordinates.altitude, Some(30.0))
    assertEquals(point.asJson, io.circe.parser.parse(json).toOption.get)

  test("each concrete geometry has a round-trip codec"):
    import io.circe.{Decoder, Encoder}
    import io.circe.syntax.*
    def roundTrip[A: Encoder: Decoder](value: A): Unit =
      assertEquals(value.asJson.as[A], Right(value))
    roundTrip(Point.from(position).get)
    roundTrip(MultiPoint.from(Nil).get)
    roundTrip(LineString.from(List(position, position)).get)
    roundTrip(MultiLineString.from(Nil).get)
    roundTrip(Polygon.from(ring).get)
    roundTrip(Polygon.from(Nil).get)
    roundTrip(MultiPolygon.from(List(Polygon.from(ring).get)).get)
    roundTrip(MultiPolygon.from(Nil).get)
    roundTrip(GeometryCollection.from(Nil).get)

  test("smart constructors reject short lines and invalid rings"):
    assert(LineString.from(Nil).isEmpty)
    assert(LineString.from(List(position)).isEmpty)
    assert(MultiLineString.from(List(List(position))).isEmpty)
    assert(Polygon.from(ring.dropRight(1)).isEmpty)
    assert(Polygon.from(List(position, position, position)).isEmpty)
    assert(Polygon.from(ring, List(ring.dropRight(1))).isEmpty)
    assert(Polygon.from(Nil, List(ring)).isEmpty)

  test("positions reject non-finite altitude"):
    List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach { altitude =>
      assert(Position.from(20, 10, Some(altitude)).isEmpty)
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
        "new Position(Coordinates.Latitude.from(0).get, Coordinates.Longitude.from(0).get, None)"
      ).nonEmpty
    )
    assert(
      compileErrors(
        "Position(Coordinates.Latitude.from(0).get, Coordinates.Longitude.from(0).get, None)"
      ).nonEmpty
    )

  test("copy cannot bypass validation"):
    assert(compileErrors("LineString.from(Nil).get.copy(coordinates = Nil)").nonEmpty)
    assert(compileErrors("Polygon.from(Nil).get.copy(holes = List(Nil))").nonEmpty)
    assert(compileErrors("Position.from(0, 0).get.copy(altitude = Some(Double.NaN))").nonEmpty)
