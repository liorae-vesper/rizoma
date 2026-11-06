package rizoma
package domain

class FarmsSuite extends munit.FunSuite:
  test("farm names are trimmed"):
    assertEquals(FarmName.from("  Cedar Grove  "), FarmName.from("Cedar Grove"))

  test("farm names reject empty or whitespace-only input"):
    assertEquals(FarmName.from(" \t\n "), Left(FarmName.Error.Empty))

  test("farm names accept the maximum allowed length"):
    assert(FarmName.from("a" * 100).isRight)

  test("farm names reject values longer than the maximum"):
    assertEquals(FarmName.from("a" * 101), Left(FarmName.Error.TooLong(100)))

  test("farm registration maps invalid coordinates to InvalidLocation"):
    assertEquals(
      RegisterFarm.from("Cedar Grove", 91.0, 0.0, 12.5),
      Left(RegisterFarmError.InvalidLocation)
    )

  test("farm registration maps invalid names to InvalidName"):
    assertEquals(
      RegisterFarm.from("   ", 45.0, 90.0, 12.5),
      Left(RegisterFarmError.InvalidName(FarmName.Error.Empty))
    )

  test("farm registration succeeds for a valid name and location"):
    assert(RegisterFarm.from("Cedar Grove", 45.0, 90.0, 12.5).isRight)

  test("farm registration rejects non-positive area"):
    List(0.0, -1.0).foreach { area =>
      assertEquals(
        RegisterFarm.from("Cedar Grove", 45.0, 90.0, area),
        Left(RegisterFarmError.InvalidArea)
      )
    }

  test("farm registration rejects non-finite area"):
    List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach { area =>
      assertEquals(
        RegisterFarm.from("Cedar Grove", 45.0, 90.0, area),
        Left(RegisterFarmError.InvalidArea)
      )
    }
