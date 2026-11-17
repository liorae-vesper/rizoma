package rizoma
package domain

import org.scalacheck.Prop.forAll

import java.time.Instant

class CropCycleSuite extends munit.ScalaCheckSuite:
  import DomainGenerators.*

  private val start = Instant.parse("2026-01-01T10:00:00Z")
  private val end = start.plusSeconds(3600)

  test("a cycle without dates is planned"):
    val result = CropCycle.Period.from(None, None).map { period =>
      assertEquals(period.startedAt, None)
      assertEquals(period.endedAt, None)
      assertEquals(period.status, CropCycle.Status.Planned)
    }
    assertEquals(result, Right(()))

  test("a started cycle without an end is active"):
    assertEquals(
      CropCycle.Period.from(Some(start), None).map(_.status),
      Right(CropCycle.Status.Active)
    )

  test("a cycle with ordered start and end timestamps is completed"):
    List(start, end, Instant.MAX).foreach { endedAt =>
      val result = CropCycle.Period.from(Some(start), Some(endedAt)).map { period =>
        assertEquals(period.startedAt, Some(start))
        assertEquals(period.endedAt, Some(endedAt))
        assertEquals(period.status, CropCycle.Status.Completed)
      }
      assertEquals(result, Right(()))
    }

  test("a cycle cannot end before it starts or without a start"):
    assertEquals(
      CropCycle.Period.from(None, Some(end)),
      Left(CropCycle.Constraints.EndWithoutStart)
    )
    assertEquals(
      CropCycle.Period.from(Some(start), Some(start.minusNanos(1))),
      Left(CropCycle.Constraints.EndBeforeStart)
    )

  property("cycle registration retains its species, plot, and occupied polygon"):
    forAll(ids, ids, polygons) { (speciesId, plotId, polygon) =>
      val species = CropSpecies.SpeciesId.from(speciesId)
      val plot = Plot.PlotId.from(plotId)
      val result = CropCycle.Register.from(species, plot, polygon).map { command =>
        assertEquals(command.speciesId, species)
        assertEquals(command.plotId, plot)
        assertEquals(command.occupiedLand, polygon)
        assertEquals(command.period.status, CropCycle.Status.Planned)
      }
      assertEquals(result, Right(()))
    }

  property("cycle registration validates its timestamps"):
    forAll(ids, ids, polygons) { (speciesId, plotId, polygon) =>
      assertEquals(
        CropCycle.Register.from(
          CropSpecies.SpeciesId.from(speciesId),
          Plot.PlotId.from(plotId),
          polygon,
          endedAt = Some(end)
        ),
        Left(CropCycle.Constraints.EndWithoutStart)
      )
      assertEquals(
        CropCycle.Register.from(
          CropSpecies.SpeciesId.from(speciesId),
          Plot.PlotId.from(plotId),
          polygon,
          Some(end),
          Some(start)
        ),
        Left(CropCycle.Constraints.EndBeforeStart)
      )
    }

  property("cycle patches replace editable values while retaining cycle and plot identities"):
    forAll(ids, ids, ids, ids, polygons, polygons) {
      (cycleId, plotId, oldSpeciesId, speciesId, originalPolygon, polygon) =>
        val result = for
          period <- CropCycle.Period.from(None, None)
          view = CropCycle.View(
            CropCycle.CycleId.from(cycleId),
            CropSpecies.SpeciesId.from(oldSpeciesId),
            Plot.PlotId.from(plotId),
            originalPolygon,
            period
          )
          patch <- view.patch(
            CropSpecies.SpeciesId.from(speciesId),
            polygon,
            Some(start),
            Some(end)
          )
        yield
          assertEquals(patch.cycleId, view.cycleId)
          assertEquals(patch.plotId, view.plotId)
          assertEquals(patch.speciesId, CropSpecies.SpeciesId.from(speciesId))
          assertEquals(patch.occupiedLand, polygon)
          assertEquals(patch.period.status, CropCycle.Status.Completed)
        assertEquals(result, Right(()))
    }

  property("cycle patches preserve existing dates by default and allow active cycles to end"):
    forAll(ids, ids, ids, polygons) { (cycleId, speciesId, plotId, polygon) =>
      val result = for
        period <- CropCycle.Period.from(Some(start), None)
        view = CropCycle.View(
          CropCycle.CycleId.from(cycleId),
          CropSpecies.SpeciesId.from(speciesId),
          Plot.PlotId.from(plotId),
          polygon,
          period
        )
        unchanged <- view.patch(view.speciesId, polygon)
        completed <- view.patch(view.speciesId, polygon, endedAt = Some(end))
      yield
        assertEquals(unchanged.period, period)
        assertEquals(completed.period.startedAt, Some(start))
        assertEquals(completed.period.endedAt, Some(end))
      assertEquals(result, Right(()))
    }

  property("an active or completed cycle cannot have its start cleared"):
    forAll(ids, ids, ids, polygons) { (cycleId, speciesId, plotId, polygon) =>
      List(None, Some(end)).foreach { endedAt =>
        val result = for period <- CropCycle.Period.from(Some(start), endedAt) yield
          val view = CropCycle.View(
            CropCycle.CycleId.from(cycleId),
            CropSpecies.SpeciesId.from(speciesId),
            Plot.PlotId.from(plotId),
            polygon,
            period
          )
          assertEquals(
            view.patch(view.speciesId, polygon, None, None),
            Left(CropCycle.Constraints.CannotClearStart)
          )
        assertEquals(result, Right(()))
      }
    }

  property("completed cycles cannot be reopened and patches must retain chronological order"):
    forAll(ids, ids, ids, polygons) { (cycleId, speciesId, plotId, polygon) =>
      val result = for period <- CropCycle.Period.from(Some(start), Some(end)) yield
        val view = CropCycle.View(
          CropCycle.CycleId.from(cycleId),
          CropSpecies.SpeciesId.from(speciesId),
          Plot.PlotId.from(plotId),
          polygon,
          period
        )
        assertEquals(
          view.patch(view.speciesId, polygon, endedAt = None),
          Left(CropCycle.Constraints.CannotClearEnd)
        )
        assertEquals(
          view.patch(view.speciesId, polygon, endedAt = Some(start.minusNanos(1))),
          Left(CropCycle.Constraints.EndBeforeStart)
        )
      assertEquals(result, Right(()))
    }

  test("cycle periods, identities, and commands cannot bypass validation"):
    assert(compileErrors("CropCycle.Period(None, None)").nonEmpty)
    assert(compileErrors("new CropCycle.Period(None, None)").nonEmpty)
    assert(compileErrors("CropCycle.Register(null, null, null, null)").nonEmpty)
    assert(compileErrors("CropCycle.Patch(null, null, null, null, null)").nonEmpty)
    assert(compileErrors("""
      for id <- Id.generate() yield
        val cycleId: CropCycle.CycleId = Plot.PlotId.from(id)
        cycleId
    """).nonEmpty)
    assert(compileErrors("""
      CropCycle.Period.from(None, None).map(
        _.copy(endedAt = Some(java.time.Instant.EPOCH))
      )
    """).nonEmpty)
