package rizoma
package domain

import org.scalacheck.Prop.forAll

import java.time.Instant

class CropCycleSuite extends munit.ScalaCheckSuite:
  import DomainGenerators.*

  private val start = Instant.parse("2026-01-01T10:00:00Z")
  private val end = start.plusSeconds(3600)

  test("a cycle without dates is planned"):
    val result = crop.cycle.Period.from(None, None).map { period =>
      assertEquals(period.startedAt, None)
      assertEquals(period.endedAt, None)
      assertEquals(period.status, crop.cycle.Status.Planned)
    }
    assertEquals(result, Right(()))

  test("a started cycle without an end is active"):
    assertEquals(
      crop.cycle.Period.from(Some(start), None).map(_.status),
      Right(crop.cycle.Status.Active)
    )

  test("a cycle with ordered start and end timestamps is completed"):
    List(start, end, Instant.MAX).foreach { endedAt =>
      val result = crop.cycle.Period.from(Some(start), Some(endedAt)).map { period =>
        assertEquals(period.startedAt, Some(start))
        assertEquals(period.endedAt, Some(endedAt))
        assertEquals(period.status, crop.cycle.Status.Completed)
      }
      assertEquals(result, Right(()))
    }

  test("a cycle cannot end before it starts or without a start"):
    assertEquals(
      crop.cycle.Period.from(None, Some(end)),
      Left(crop.cycle.Constraints.EndWithoutStart)
    )
    assertEquals(
      crop.cycle.Period.from(Some(start), Some(start.minusNanos(1))),
      Left(crop.cycle.Constraints.EndBeforeStart)
    )

  property("cycle registration retains its species, plot, and occupied polygon"):
    forAll(ids, ids, polygons) { (speciesId, plotId, polygon) =>
      val species = crop.species.SpeciesId.from(speciesId)
      val plotIdentity = plot.PlotId.from(plotId)
      val result = crop.cycle.Registration.from(species, plotIdentity, polygon).map { command =>
        assertEquals(command.speciesId, species)
        assertEquals(command.plotId, plotIdentity)
        assertEquals(command.occupiedLand, polygon)
        assertEquals(command.period.status, crop.cycle.Status.Planned)
      }
      assertEquals(result, Right(()))
    }

  property("cycle registration validates its timestamps"):
    forAll(ids, ids, polygons) { (speciesId, plotId, polygon) =>
      assertEquals(
        crop.cycle.Registration.from(
          crop.species.SpeciesId.from(speciesId),
          plot.PlotId.from(plotId),
          polygon,
          endedAt = Some(end)
        ),
        Left(crop.cycle.Constraints.EndWithoutStart)
      )
      assertEquals(
        crop.cycle.Registration.from(
          crop.species.SpeciesId.from(speciesId),
          plot.PlotId.from(plotId),
          polygon,
          Some(end),
          Some(start)
        ),
        Left(crop.cycle.Constraints.EndBeforeStart)
      )
    }

  property("cycle patches replace editable values while retaining cycle and plot identities"):
    forAll(ids, ids, ids, ids, polygons, polygons) {
      (cycleId, plotId, oldSpeciesId, speciesId, originalPolygon, polygon) =>
        val result = for
          period <- crop.cycle.Period.from(None, None)
          view = crop.cycle.View(
            crop.cycle.CycleId.from(cycleId),
            crop.species.SpeciesId.from(oldSpeciesId),
            plot.PlotId.from(plotId),
            originalPolygon,
            period
          )
          patch <- view.patch(
            crop.species.SpeciesId.from(speciesId),
            polygon,
            Some(start),
            Some(end)
          )
        yield
          assertEquals(patch.cycleId, view.cycleId)
          assertEquals(patch.plotId, view.plotId)
          assertEquals(patch.speciesId, crop.species.SpeciesId.from(speciesId))
          assertEquals(patch.occupiedLand, polygon)
          assertEquals(patch.period.status, crop.cycle.Status.Completed)
        assertEquals(result, Right(()))
    }

  property("cycle patches preserve existing dates by default and allow active cycles to end"):
    forAll(ids, ids, ids, polygons) { (cycleId, speciesId, plotId, polygon) =>
      val result = for
        period <- crop.cycle.Period.from(Some(start), None)
        view = crop.cycle.View(
          crop.cycle.CycleId.from(cycleId),
          crop.species.SpeciesId.from(speciesId),
          plot.PlotId.from(plotId),
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
        val result = for period <- crop.cycle.Period.from(Some(start), endedAt) yield
          val view = crop.cycle.View(
            crop.cycle.CycleId.from(cycleId),
            crop.species.SpeciesId.from(speciesId),
            plot.PlotId.from(plotId),
            polygon,
            period
          )
          assertEquals(
            view.patch(view.speciesId, polygon, None, None),
            Left(crop.cycle.Constraints.CannotClearStart)
          )
        assertEquals(result, Right(()))
      }
    }

  property("completed cycles cannot be reopened and patches must retain chronological order"):
    forAll(ids, ids, ids, polygons) { (cycleId, speciesId, plotId, polygon) =>
      val result = for period <- crop.cycle.Period.from(Some(start), Some(end)) yield
        val view = crop.cycle.View(
          crop.cycle.CycleId.from(cycleId),
          crop.species.SpeciesId.from(speciesId),
          plot.PlotId.from(plotId),
          polygon,
          period
        )
        assertEquals(
          view.patch(view.speciesId, polygon, endedAt = None),
          Left(crop.cycle.Constraints.CannotClearEnd)
        )
        assertEquals(
          view.patch(view.speciesId, polygon, endedAt = Some(start.minusNanos(1))),
          Left(crop.cycle.Constraints.EndBeforeStart)
        )
      assertEquals(result, Right(()))
    }

  test("cycle periods, identities, and commands cannot bypass validation"):
    assert(compileErrors("crop.cycle.Period(None, None)").nonEmpty)
    assert(compileErrors("new crop.cycle.Period(None, None)").nonEmpty)
    assert(compileErrors("crop.cycle.Registration(null, null, null, null)").nonEmpty)
    assert(compileErrors("crop.cycle.Patch(null, null, null, null, null)").nonEmpty)
    assert(compileErrors("""
      for id <- Id.generate() yield
        val cycleId: crop.cycle.CycleId = plot.PlotId.from(id)
        cycleId
    """).nonEmpty)
    assert(compileErrors("""
      crop.cycle.Period.from(None, None).map(
        _.copy(endedAt = Some(java.time.Instant.EPOCH))
      )
    """).nonEmpty)
