package rizoma
package domain

import org.scalacheck.Prop.forAll

class FarmsSuite extends munit.ScalaCheckSuite:
  import DomainGenerators.*

  property("farm names accept valid input and normalize surrounding whitespace"):
    forAll(names, padding, padding) { (name, before, after) =>
      val result = for
        canonical <- Farm.Name.from(name)
        padded <- Farm.Name.from(before + name + after)
      yield
        assertEquals(canonical.toString, name)
        assertEquals(padded, canonical)
      assertEquals(result, Right(()))
    }

  property("farm names reject empty and whitespace-only input"):
    forAll(padding) { name =>
      assertEquals(Farm.Name.from(name), Left(Farm.Name.Error.Empty))
    }

  property("farm names reject excessive length after trimming"):
    forAll(longNames, padding, padding) { (name, before, after) =>
      assertEquals(Farm.Name.from(before + name + after), Left(Farm.Name.Error.TooLong(100)))
    }

  test("farm name length boundaries apply after trimming"):
    assert(Farm.Name.from("a").isRight)
    assert(Farm.Name.from(" \t" + "a" * 100 + "\n ").isRight)
    assertEquals(Farm.Name.from("a" * 101), Left(Farm.Name.Error.TooLong(100)))
    assert(Farm.Name.from("🌱" * 50).isRight)
    assertEquals(Farm.Name.from("🌱" * 51), Left(Farm.Name.Error.TooLong(100)))

  test("farm names preserve internal whitespace and Unicode"):
    val raw = "Sítio  São João 田"
    val result = for
      name <- Farm.Name.from(raw)
      padded <- Farm.Name.from(" \t" + raw + "\n")
      collapsed <- Farm.Name.from("Sítio São João 田")
    yield
      assertEquals(name.toString, raw)
      assertEquals(padded, name)
      assertNotEquals(name, collapsed)
    assertEquals(result, Right(()))

  property("farm registration normalizes the name and retains validated inputs"):
    forAll(names, positions, areas, padding) { (name, position, area, whitespace) =>
      val result = for
        registration <- Farm.Register.from(whitespace + name + whitespace, position, area)
        canonical <- Farm.Name.from(name)
      yield
        assertEquals(registration.name, canonical)
        assertEquals(registration.location, position)
        assertEquals(registration.area, area)
      assertEquals(result, Right(()))
    }

  property("farm registration maps either name error into its constraints"):
    forAll(invalidNames, positions, areas) { (name, position, area) =>
      val expected =
        if name.trim.isEmpty then Farm.Name.Error.Empty else Farm.Name.Error.TooLong(100)
      assertEquals(
        Farm.Register.from(name, position, area),
        Left(Farm.Constraints.InvalidName(expected))
      )
    }

  property("farm patches replace the values and retain the farm identity"):
    forAll(ids, names, positions, areas, names, positions, areas, padding) {
      (id, oldName, oldPosition, oldArea, name, position, area, whitespace) =>
        val result = for
          original <- Farm.Name.from(oldName)
          canonical <- Farm.Name.from(name)
          patch <- Farm
            .View(id, original, oldPosition, oldArea)
            .patch(whitespace + name + whitespace, position, area)
        yield
          assertEquals(patch.id, id)
          assertEquals(patch.name, canonical)
          assertEquals(patch.location, position)
          assertEquals(patch.area, area)
        assertEquals(result, Right(()))
    }

  property("farm patches map either name error into their constraints"):
    forAll(ids, names, positions, areas, invalidNames) { (id, oldName, position, area, name) =>
      val result = for original <- Farm.Name.from(oldName) yield
        val view = Farm.View(id, original, position, area)
        val expected =
          if name.trim.isEmpty then Farm.Name.Error.Empty else Farm.Name.Error.TooLong(100)
        assertEquals(view.patch(name, position, area), Left(Farm.Constraints.InvalidName(expected)))
      assertEquals(result, Right(()))
    }

  test("farm names and patch construction cannot bypass their boundaries"):
    assert(compileErrors("val name: Farm.Name = \"raw\"").nonEmpty)
    assert(compileErrors("""
      for
        name <- Farm.Name.from("Cedar")
        area <- Area.from(1)
        position <- Position.from(longitude = 0, latitude = 0).toRight("Invalid position")
      yield Farm.Register(name, position, area)
    """).nonEmpty)
    assert(compileErrors("""
      for
        name <- Farm.Name.from("Cedar")
        area <- Area.from(1)
        position <- Position.from(longitude = 0, latitude = 0).toRight("Invalid position")
      yield new Farm.Register(name, position, area)
    """).nonEmpty)
    assert(compileErrors("""
      for
        name <- Farm.Name.from("Cedar")
        area <- Area.from(1)
        position <- Position.from(longitude = 0, latitude = 0).toRight("Invalid position")
      yield Farm.Patch(name, position, area, Id.generate())
    """).nonEmpty)
    assert(compileErrors("""
      for
        name <- Farm.Name.from("Cedar")
        area <- Area.from(1)
        position <- Position.from(longitude = 0, latitude = 0).toRight("Invalid position")
      yield new Farm.Patch(name, position, area, Id.generate())
    """).nonEmpty)
