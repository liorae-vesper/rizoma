package rizoma
package domain

class CoordinateSuite extends munit.FunSuite:
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
