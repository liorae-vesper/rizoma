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
    assert(GeoPoint.from(-90.0, 180.0).isDefined)

  test("a geo point is rejected when its latitude is invalid"):
    assertEquals(GeoPoint.from(90.1, 0.0), None)

  test("a geo point is rejected when its longitude is invalid"):
    assertEquals(GeoPoint.from(0.0, -180.1), None)
