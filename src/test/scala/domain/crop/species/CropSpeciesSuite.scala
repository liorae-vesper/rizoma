package rizoma
package domain

import org.scalacheck.Prop.forAll

class CropSpeciesSuite extends munit.ScalaCheckSuite:
  import DomainGenerators.*

  property("species names trim surrounding whitespace and retain internal characters"):
    forAll(names, padding, padding) { (name, before, after) =>
      val result = for
        common <- crop.species.CommonName.from(before + name + after)
        scientific <- crop.species.ScientificName.from(before + name + after)
      yield
        assertEquals(common.toString, name)
        assertEquals(scientific.toString, name)
      assertEquals(result, Right(()))
    }

  property("species names reject blank input"):
    forAll(padding) { name =>
      assertEquals(crop.species.CommonName.from(name), Left(crop.species.CommonName.Error.Empty))
      assertEquals(
        crop.species.ScientificName.from(name),
        Left(crop.species.ScientificName.Error.Empty)
      )
    }

  property("species names reject more than 100 characters after trimming"):
    forAll(longNames, padding) { (name, whitespace) =>
      assertEquals(
        crop.species.CommonName.from(whitespace + name + whitespace),
        Left(crop.species.CommonName.Error.TooLong(100))
      )
      assertEquals(
        crop.species.ScientificName.from(whitespace + name + whitespace),
        Left(crop.species.ScientificName.Error.TooLong(100))
      )
    }

  test("species names accept boundary lengths and preserve Unicode and internal whitespace"):
    List("a", "a" * 100, "🌱" * 50, "Feijão  São João", "Phaseolus vulgaris").foreach { value =>
      assertEquals(crop.species.CommonName.from(s" $value ").map(_.toString), Right(value))
      assertEquals(crop.species.ScientificName.from(s" $value ").map(_.toString), Right(value))
    }
    assert(crop.species.CommonName.from("a" * 101).isLeft)
    assert(crop.species.ScientificName.from("🌱" * 51).isLeft)

  property("species registration validates and normalizes both names"):
    forAll(names, names, padding) { (commonName, scientificName, whitespace) =>
      val result =
        for command <- crop.species.Registration
            .from(whitespace + commonName + whitespace, scientificName)
        yield
          assertEquals(command.commonName.toString, commonName)
          assertEquals(command.scientificName.toString, scientificName)
      assertEquals(result, Right(()))
    }

  test("species registration reports the name that failed validation"):
    assertEquals(
      crop.species.Registration.from(" ", "Phaseolus vulgaris"),
      Left(crop.species.Constraints.InvalidCommonName(crop.species.CommonName.Error.Empty))
    )
    assertEquals(
      crop.species.Registration.from("Beans", " "),
      Left(crop.species.Constraints.InvalidScientificName(crop.species.ScientificName.Error.Empty))
    )

  property("species patches retain identity and validate replacement names"):
    forAll(ids, names, names) { (id, commonName, scientificName) =>
      val result = for
        original <- crop.species.Registration.from("Beans", "Phaseolus vulgaris")
        view = crop.species.View(
          crop.species.SpeciesId.from(id),
          original.commonName,
          original.scientificName
        )
        patch <- view.patch(s" $commonName ", s" $scientificName ")
      yield
        assertEquals(patch.speciesId, view.speciesId)
        assertEquals(patch.commonName.toString, commonName)
        assertEquals(patch.scientificName.toString, scientificName)
        assertEquals(
          view.patch(" ", scientificName),
          Left(crop.species.Constraints.InvalidCommonName(crop.species.CommonName.Error.Empty))
        )
        assertEquals(
          view.patch(commonName, " "),
          Left(
            crop.species.Constraints.InvalidScientificName(crop.species.ScientificName.Error.Empty)
          )
        )
      assertEquals(result, Right(()))
    }

  test("species names, identities, and commands cannot bypass validation"):
    assert(compileErrors("val name: crop.species.CommonName = \"Beans\"").nonEmpty)
    assert(compileErrors("val name: crop.species.ScientificName = \"Phaseolus vulgaris\"").nonEmpty)
    assert(compileErrors("""
      for id <- Id.generate() yield
        val speciesId: crop.species.SpeciesId = id
        speciesId
    """).nonEmpty)
    assert(compileErrors("crop.species.Registration(null, null)").nonEmpty)
    assert(compileErrors("crop.species.Patch(null, null, null)").nonEmpty)
