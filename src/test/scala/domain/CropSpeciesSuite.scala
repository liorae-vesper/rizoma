package rizoma
package domain

import org.scalacheck.Prop.forAll

class CropSpeciesSuite extends munit.ScalaCheckSuite:
  import DomainGenerators.*

  property("species names trim surrounding whitespace and retain internal characters"):
    forAll(names, padding, padding) { (name, before, after) =>
      val result = for
        common <- CropSpecies.CommonName.from(before + name + after)
        scientific <- CropSpecies.ScientificName.from(before + name + after)
      yield
        assertEquals(common.toString, name)
        assertEquals(scientific.toString, name)
      assertEquals(result, Right(()))
    }

  property("species names reject blank input"):
    forAll(padding) { name =>
      assertEquals(CropSpecies.CommonName.from(name), Left(CropSpecies.CommonName.Error.Empty))
      assertEquals(
        CropSpecies.ScientificName.from(name),
        Left(CropSpecies.ScientificName.Error.Empty)
      )
    }

  property("species names reject more than 100 characters after trimming"):
    forAll(longNames, padding) { (name, whitespace) =>
      assertEquals(
        CropSpecies.CommonName.from(whitespace + name + whitespace),
        Left(CropSpecies.CommonName.Error.TooLong(100))
      )
      assertEquals(
        CropSpecies.ScientificName.from(whitespace + name + whitespace),
        Left(CropSpecies.ScientificName.Error.TooLong(100))
      )
    }

  test("species names accept boundary lengths and preserve Unicode and internal whitespace"):
    List("a", "a" * 100, "🌱" * 50, "Feijão  São João", "Phaseolus vulgaris").foreach { value =>
      assertEquals(CropSpecies.CommonName.from(s" $value ").map(_.toString), Right(value))
      assertEquals(CropSpecies.ScientificName.from(s" $value ").map(_.toString), Right(value))
    }
    assert(CropSpecies.CommonName.from("a" * 101).isLeft)
    assert(CropSpecies.ScientificName.from("🌱" * 51).isLeft)

  property("species registration validates and normalizes both names"):
    forAll(names, names, padding) { (commonName, scientificName, whitespace) =>
      val result =
        for command <- CropSpecies.Register
            .from(whitespace + commonName + whitespace, scientificName)
        yield
          assertEquals(command.commonName.toString, commonName)
          assertEquals(command.scientificName.toString, scientificName)
      assertEquals(result, Right(()))
    }

  test("species registration reports the name that failed validation"):
    assertEquals(
      CropSpecies.Register.from(" ", "Phaseolus vulgaris"),
      Left(CropSpecies.Constraints.InvalidCommonName(CropSpecies.CommonName.Error.Empty))
    )
    assertEquals(
      CropSpecies.Register.from("Beans", " "),
      Left(CropSpecies.Constraints.InvalidScientificName(CropSpecies.ScientificName.Error.Empty))
    )

  property("species patches retain identity and validate replacement names"):
    forAll(ids, names, names) { (id, commonName, scientificName) =>
      val result = for
        original <- CropSpecies.Register.from("Beans", "Phaseolus vulgaris")
        view = CropSpecies.View(
          CropSpecies.SpeciesId.from(id),
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
          Left(CropSpecies.Constraints.InvalidCommonName(CropSpecies.CommonName.Error.Empty))
        )
        assertEquals(
          view.patch(commonName, " "),
          Left(
            CropSpecies.Constraints.InvalidScientificName(CropSpecies.ScientificName.Error.Empty)
          )
        )
      assertEquals(result, Right(()))
    }

  test("species names, identities, and commands cannot bypass validation"):
    assert(compileErrors("val name: CropSpecies.CommonName = \"Beans\"").nonEmpty)
    assert(compileErrors("val name: CropSpecies.ScientificName = \"Phaseolus vulgaris\"").nonEmpty)
    assert(compileErrors("""
      for id <- Id.generate() yield
        val speciesId: CropSpecies.SpeciesId = id
        speciesId
    """).nonEmpty)
    assert(compileErrors("CropSpecies.Register(null, null)").nonEmpty)
    assert(compileErrors("CropSpecies.Patch(null, null, null)").nonEmpty)
