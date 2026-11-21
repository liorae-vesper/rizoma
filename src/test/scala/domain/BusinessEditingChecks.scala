package rizoma.domain

import io.circe.syntax.*
import rizoma.domain.crop.{activity, cycle, species}

private[domain] trait BusinessEditingChecks extends BusinessFixture:
  final case class Edits(
      plotPatch: plot.Patch,
      speciesPatch: species.Patch,
      cyclePatch: cycle.Patch,
      harvestPatch: harvest.Patch,
      activityPatch: activity.Patch
  )

  def editAll(fixture: BusinessData): Edits =
    import fixture.*
    val plotPatch = plot
      .View(geometry, area, plotName, farmId, plot.PlotId.from(plotId))
      .patch(geometry, area, "Revised plot", plot.Status.Resting)
      .fold(e => fail(e.toString), identity)
    assertEquals(plot.edit(plotPatch)(using plots), Right(()))
    assertEquals(
      db.queryOne("SELECT name, geometry, area, unit_of_area, status FROM plots")(r =>
        (r.getString(1), r.getString(2), r.getDouble(3), r.getString(4), r.getString(5))
      ),
      Some(("Revised plot", geometry.asJson.noSpaces, 10.0, "square_meters", "Resting"))
    )
    val speciesPatch = species
      .View(
        species.SpeciesId.from(speciesId),
        speciesInput.commonName,
        speciesInput.scientificName
      )
      .patch("Maize", "Zea mays L.")
      .fold(e => fail(e.toString), identity)
    assertEquals(species.edit(speciesPatch)(using speciesStore), Right(()))
    assertEquals(
      db.queryOne("SELECT common_name, scientific_name FROM species")(r =>
        (r.getString(1), r.getString(2))
      ),
      Some(("Maize", "Zea mays L."))
    )
    val cyclePatch = cycle
      .View(
        cycle.CycleId.from(cycleId),
        species.SpeciesId.from(speciesId),
        plot.PlotId.from(plotId),
        polygon,
        cycleInput.period
      )
      .patch(species.SpeciesId.from(speciesId), polygon, Some(at), Some(at))
      .fold(e => fail(e.toString), identity)
    assertEquals(cycle.edit(cyclePatch)(using cycles), Right(()))
    assertEquals(
      db.queryOne("SELECT occupied_land, started_at, ended_at FROM cycles")(r =>
        (r.getString(1), r.getString(2), r.getString(3))
      ),
      Some((polygon.asJson.noSpaces, at.toString, at.toString))
    )
    val harvestPatch = harvest
      .View(
        harvest.HarvestId.from(harvestId),
        cycle.CycleId.from(cycleId),
        harvestInput.yieldAmount,
        at
      )
      .patch(7, at, harvest.UnitOfYield.Kilograms)
      .fold(e => fail(e.toString), identity)
    assertEquals(harvest.edit(harvestPatch)(using harvests), Right(()))
    assertEquals(
      db.queryOne("SELECT amount, unit_of_yield, harvested_at FROM harvests")(r =>
        (r.getDouble(1), r.getString(2), r.getString(3))
      ),
      Some((7.0, "Kilograms", at.toString))
    )
    val activityPatch = activity
      .View(
        activity.ActivityId.from(activityId),
        cycle.CycleId.from(cycleId),
        activityInput.event,
        at
      )
      .patch("Irrigation", at)
      .fold(e => fail(e.toString), identity)
    assertEquals(activity.edit(activityPatch)(using activities), Right(()))
    assertEquals(
      db.queryOne("SELECT event, occurred_at FROM activities")(r =>
        (r.getString(1), r.getString(2))
      ),
      Some(("Irrigation", at.toString))
    )
    Edits(plotPatch, speciesPatch, cyclePatch, harvestPatch, activityPatch)
