package rizoma
package domain

class AreaSuite extends munit.FunSuite:
  test("areas reject zero and negative values"):
    List(0.0, -1.0).foreach { value =>
      assertEquals(Area.from(value), Left(AreaError.AreaNotGreaterThanZero))
    }

  test("areas reject non-finite values"):
    List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach { value =>
      assertEquals(Area.from(value), Left(AreaError.AreaNotFinite))
    }
