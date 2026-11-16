package rizoma
package domain

import io.circe.parser
import io.circe.parser.parse

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
    assert(Position.from(longitude = 180.0, latitude = -90.0).isDefined)

  test("a geo point is rejected when its latitude is invalid"):
    assertEquals(Position.from(longitude = 0.0, latitude = 90.1), None)

  test("a geo point is rejected when its longitude is invalid"):
    assertEquals(Position.from(longitude = -180.1, latitude = 0.0), None)

  test("position construction uses longitude then latitude, matching GeoJSON"):
    import io.circe.syntax.*
    val result = for point <- Position.from(120.0, -30.0) yield
      assertEquals(point.longitude.toDouble, 120.0)
      assertEquals(point.latitude.toDouble, -30.0)
      assertEquals(point.altitude, None)
      assertEquals(point.asJson, io.circe.Json.arr(120.0.asJson, (-30.0).asJson))
      assertEquals(point.asJson.as[Position], Right(point))
    assertEquals(result, Some(()))

  test("position construction with altitude uses GeoJSON order without swapping valid ordinates"):
    import io.circe.syntax.*
    val result = for point <- Position.from(10.0, 20.0, Some(30.0)) yield
      assertEquals(point.longitude.toDouble, 10.0)
      assertEquals(point.latitude.toDouble, 20.0)
      assertEquals(point.altitude, Some(30.0))
      assertEquals(point.asJson, io.circe.Json.arr(10.0.asJson, 20.0.asJson, 30.0.asJson))
      assertEquals(point.asJson.as[Position], Right(point))
    assertEquals(result, Some(()))

  private val fixture = for
    position <- Position.from(longitude = 10, latitude = 20)
    second <- Position.from(longitude = 11, latitude = 21)
    third <- Position.from(longitude = 12, latitude = 20)
  yield (position, List(position, second, third, position))

  test("all geometry types encode their GeoJSON structure and round trip"):
    import io.circe.syntax.*
    val result = for
      (position, ring) <- fixture
      point <- Point.from(position)
      multiPoint <- MultiPoint.from(List(position))
      line <- LineString.from(List(position, position))
      multiLine <- MultiLineString.from(List(List(position, position)))
      polygon <- Polygon.from(ring, List(ring))
      exterior <- Polygon.from(ring)
      multiPolygon <- MultiPolygon.from(List(exterior))
      emptyCollection <- GeometryCollection.from(Nil)
      collection <- GeometryCollection.from(List(point, emptyCollection))
    yield
      val examples: List[(Geometry, String)] = List(
        point -> """{"type":"Point","coordinates":[10,20]}""",
        multiPoint -> """{"type":"MultiPoint","coordinates":[[10,20]]}""",
        line -> """{"type":"LineString","coordinates":[[10,20],[10,20]]}""",
        multiLine -> """{"type":"MultiLineString","coordinates":[[[10,20],[10,20]]]}""",
        polygon -> """{"type":"Polygon","coordinates":[[[10,20],[11,21],[12,20],[10,20]],[[10,20],[11,21],[12,20],[10,20]]]}""",
        multiPolygon -> """{"type":"MultiPolygon","coordinates":[[[[10,20],[11,21],[12,20],[10,20]]]]}""",
        collection -> """{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[10,20]},{"type":"GeometryCollection","geometries":[]}]}"""
      )
      examples.foreach { (geometry, json) =>
        assertEquals(Right(geometry.asJson), parse(json))
        assertEquals(parser.decode[Geometry](json), Right(geometry))
      }
    assertEquals(result, Some(()))

  test("concrete codecs reject mismatched geometry types"):
    assert(parser.decode[Point]("""{"type":"MultiPoint","coordinates":[10,20]}""").isLeft)

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
    ).foreach(json => assert(parser.decode[Geometry](json).isLeft, json))

  test("positions preserve optional altitude"):
    import io.circe.syntax.*
    val json = """{"type":"Point","coordinates":[10,20,30]}"""
    val result = for
      point <- parser.decode[Point](json)
      expected <- parse(json)
    yield
      assertEquals(point.coordinates.altitude, Some(30.0))
      assertEquals(point.asJson, expected)
    assertEquals(result, Right(()))

  test("each concrete geometry has a round-trip codec"):
    import io.circe.{Decoder, Encoder}
    import io.circe.syntax.*
    def roundTrip[A: Encoder: Decoder](value: A): Unit =
      assertEquals(value.asJson.as[A], Right(value))
    val result = for
      (position, ring) <- fixture
      point <- Point.from(position)
      multiPoint <- MultiPoint.from(Nil)
      line <- LineString.from(List(position, position))
      multiLine <- MultiLineString.from(Nil)
      polygon <- Polygon.from(ring)
      emptyPolygon <- Polygon.from(Nil)
      multiPolygon <- MultiPolygon.from(List(polygon))
      emptyMultiPolygon <- MultiPolygon.from(Nil)
      collection <- GeometryCollection.from(Nil)
    yield
      roundTrip(point)
      roundTrip(multiPoint)
      roundTrip(line)
      roundTrip(multiLine)
      roundTrip(polygon)
      roundTrip(emptyPolygon)
      roundTrip(multiPolygon)
      roundTrip(emptyMultiPolygon)
      roundTrip(collection)
    assertEquals(result, Some(()))

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
