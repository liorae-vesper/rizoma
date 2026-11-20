package rizoma
package domain

import org.scalacheck.Prop.forAll

class PlotSuite extends munit.ScalaCheckSuite:
  import DomainGenerators.*

  property("plot names accept valid input and normalize surrounding whitespace"):
    forAll(names, padding, padding) { (name, before, after) =>
      val result = for
        canonical <- plot.Name.from(name)
        padded <- plot.Name.from(before + name + after)
      yield
        assertEquals(canonical.toString, name)
        assertEquals(padded, canonical)
      assertEquals(result, Right(()))
    }

  property("plot names reject empty and whitespace-only input"):
    forAll(padding) { name =>
      assertEquals(plot.Name.from(name), Left(plot.Name.Error.Empty))
    }

  property("plot names reject excessive length after trimming"):
    forAll(longNames, padding, padding) { (name, before, after) =>
      assertEquals(plot.Name.from(before + name + after), Left(plot.Name.Error.TooLong(100)))
    }

  test("plot name length boundaries apply after trimming"):
    assert(plot.Name.from("a").isRight)
    assert(plot.Name.from(" \t" + "a" * 100 + "\n ").isRight)
    assertEquals(plot.Name.from("a" * 101), Left(plot.Name.Error.TooLong(100)))
    assert(plot.Name.from("🌱" * 50).isRight)
    assertEquals(plot.Name.from("🌱" * 51), Left(plot.Name.Error.TooLong(100)))

  test("plot names preserve internal whitespace and Unicode"):
    val raw = "Talhão  São João 田"
    val result = for
      name <- plot.Name.from(raw)
      padded <- plot.Name.from(" \t" + raw + "\n")
      collapsed <- plot.Name.from("Talhão São João 田")
    yield
      assertEquals(name.toString, raw)
      assertEquals(padded, name)
      assertNotEquals(name, collapsed)
    assertEquals(result, Right(()))

  property("plot patches replace the values and retain plot and farm identities"):
    val originals = for
      geometry <- geometries
      area <- areas
      name <- names
      farmId <- ids
      plotId <- ids
    yield (geometry, area, name, farmId, plotId)

    forAll(originals, geometries, areas, names, padding) {
      (original, geometry, area, name, whitespace) =>
        val (oldGeometry, oldArea, oldName, farmId, plotId) = original
        val result = for
          originalName <- plot.Name.from(oldName)
          canonical <- plot.Name.from(name)
          view = plot.View(oldGeometry, oldArea, originalName, farmId, plot.PlotId.from(plotId))
          patch <- view.patch(geometry, area, whitespace + name + whitespace)
        yield
          assertEquals(patch.plotId, view.plotId)
          assertEquals(patch.currentFarm, farmId)
          assertEquals(patch.newName, canonical)
          assertEquals(patch.newGeometry, geometry)
          assertEquals(patch.newArea, area)
        assertEquals(result, Right(()))
    }

  property("plot patches map either name error into their constraints"):
    forAll(geometries, areas, names, ids, ids, invalidNames) {
      (geometry, area, oldName, farmId, plotId, name) =>
        val result = for originalName <- plot.Name.from(oldName) yield
          val view = plot.View(geometry, area, originalName, farmId, plot.PlotId.from(plotId))
          val expected =
            if name.trim.isEmpty then plot.Name.Error.Empty else plot.Name.Error.TooLong(100)
          assertEquals(
            view.patch(geometry, area, name),
            Left(plot.Constraints.InvalidName(expected))
          )
        assertEquals(result, Right(()))
    }

  test("plot names and patch construction cannot bypass their boundaries"):
    assert(compileErrors("val name: plot.Name = \"raw\"").nonEmpty)
    assert(compileErrors("""
      for uuid <- UUIDv7.parse("00000000-0000-7000-8000-000000000000") yield
        val id: plot.PlotId = Id.fromUUIDv7(uuid)
        id
    """).nonEmpty)
    assert(compileErrors("""
      for
        name <- plot.Name.from("North")
        area <- Area.from(1)
        geometry <- MultiPolygon.from(Nil).toRight("Invalid geometry")
        id <- Id.parse("00000000-0000-7000-8000-000000000000")
      yield plot.Patch(geometry, area, name, id, plot.PlotId.from(id), plot.Status.Active)
    """).nonEmpty)
    assert(compileErrors("""
      for
        name <- plot.Name.from("North")
        area <- Area.from(1)
        geometry <- MultiPolygon.from(Nil).toRight("Invalid geometry")
        id <- Id.parse("00000000-0000-7000-8000-000000000000")
      yield new plot.Patch(geometry, area, name, id, plot.PlotId.from(id), plot.Status.Active)
    """).nonEmpty)

  property("plots default to active and retain their status when patching other fields"):
    forAll(geometries, areas, names, ids, ids) { (geometry, area, name, farmId, plotId) =>
      val result = for validated <- plot.Name.from(name) yield
        val registration = plot.Registration(geometry, area, validated, farmId)
        assertEquals(registration.status, plot.Status.Active)
        plot.Status.values.foreach { status =>
          val view = plot.View(geometry, area, validated, farmId, plot.PlotId.from(plotId), status)
          assertEquals(view.patch(geometry, area, name).map(_.status), Right(status))
        }
      assertEquals(result, Right(()))
    }

  property("plot patches can explicitly change the status while retaining identities"):
    forAll(geometries, areas, names, ids, ids) { (geometry, area, name, farmId, plotId) =>
      val result = for
        validated <- plot.Name.from(name)
        view = plot.View(geometry, area, validated, farmId, plot.PlotId.from(plotId))
        patch <- view.patch(geometry, area, name, plot.Status.Resting)
      yield
        assertEquals(patch.status, plot.Status.Resting)
        assertEquals(patch.plotId, view.plotId)
        assertEquals(patch.currentFarm, farmId)
      assertEquals(result, Right(()))
    }
