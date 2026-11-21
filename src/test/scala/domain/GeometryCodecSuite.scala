package rizoma
package domain

import io.circe.parser
import io.circe.parser.parse

class GeometryCodecSuite extends GeometryFixture:
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
