package rizoma.domain

import rizoma.domain.crop.{activity, cycle, species}
import rizoma.prelude.Id

class BusinessDeletionSuite extends BusinessEditingChecks:
  test("soft deletion retains records and enforces active references"):
    withFixture { fixture =>
      import fixture.*
      fixture.seed()
      val edits = editAll(fixture)
      val plotPatch = edits.plotPatch
      val speciesPatch = edits.speciesPatch
      val cyclePatch = edits.cyclePatch
      val harvestPatch = edits.harvestPatch
      val activityPatch = edits.activityPatch
      assertEquals(
        cycle.delete(cycle.Delete(cycle.CycleId.from(cycleId)))(using cycles),
        Left(ActionError.HasDependents("cycle", cycleId.value))
      )
      assertEquals(
        harvest.delete(harvest.Delete(harvest.HarvestId.from(harvestId)))(using harvests),
        Right(())
      )
      assertEquals(
        activity.delete(activity.Delete(activity.ActivityId.from(activityId)))(using activities),
        Right(())
      )
      assert(activity.edit(activityPatch)(using activities).isLeft)
      assert(harvest.edit(harvestPatch)(using harvests).isLeft)
      assertEquals(
        db.queryOne("SELECT deleted FROM activities WHERE id = ?", Seq(activityId.value))(
          _.getInt(1)
        ),
        Some(1)
      )
      assertEquals(
        db.queryOne("SELECT deleted FROM harvests WHERE id = ?", Seq(harvestId.value))(_.getInt(1)),
        Some(1)
      )
      assertEquals(
        cycle.delete(cycle.Delete(cycle.CycleId.from(cycleId)))(using cycles),
        Right(())
      )
      assert(cycle.edit(cyclePatch)(using cycles).isLeft)
      assert(cycle.finish(cycle.Finish(cycle.CycleId.from(cycleId), at))(using cycles).isLeft)
      assertEquals(
        db.queryOne("SELECT deleted FROM cycles WHERE id = ?", Seq(cycleId.value))(_.getInt(1)),
        Some(1)
      )
      assert(
        activity
          .record(activityInput)(using
            activities,
            generator(Id.generate().fold(error => fail(error.toString), identity))
          )
          .isLeft
      )
      assert(
        harvest
          .register(harvestInput)(using
            harvests,
            generator(Id.generate().fold(error => fail(error.toString), identity))
          )
          .isLeft
      )
      assertEquals(plot.delete(plot.Delete(plot.PlotId.from(plotId)))(using plots), Right(()))
      assertEquals(
        plot.delete(plot.Delete(plot.PlotId.from(plotId)))(using plots),
        Left(ActionError.NotFound("plot", plotId.value))
      )
      assert(plot.edit(plotPatch)(using plots).isLeft)
      assertEquals(
        plot.mark(plot.Mark(plot.PlotId.from(plotId), geometry))(using plots),
        Left(ActionError.NotFound("plot", plotId.value))
      )
      assert(plot.demark(plot.Demark(plot.PlotId.from(plotId)))(using plots).isLeft)
      assert(plot.remeasure(plot.Remeasure(plot.PlotId.from(plotId), area))(using plots).isLeft)
      assert(plot.activate(plot.Activate(plot.PlotId.from(plotId)))(using plots).isLeft)
      assert(plot.resting(plot.Resting(plot.PlotId.from(plotId)))(using plots).isLeft)
      assertEquals(
        db.queryOne("SELECT deleted FROM plots WHERE id = ?", Seq(plotId.value))(_.getInt(1)),
        Some(1)
      )
      assert(
        cycle
          .register(cycleInput)(using
            cycles,
            generator(Id.generate().fold(error => fail(error.toString), identity))
          )
          .isLeft
      )
      assertEquals(
        species.delete(species.Delete(species.SpeciesId.from(speciesId)))(using speciesStore),
        Right(())
      )
      assertEquals(species.get(species.SpeciesId.from(speciesId))(using speciesStore), Right(None))
      assertEquals(species.list()(using speciesStore), Right(Nil))
      assert(species.edit(speciesPatch)(using speciesStore).isLeft)
      assertEquals(
        db.queryOne("SELECT deleted FROM species WHERE id = ?", Seq(speciesId.value))(_.getInt(1)),
        Some(1)
      )
      val replacementPlotId = Id.generate().fold(error => fail(error.toString), identity)
      assertEquals(
        plot.register(plotInput)(using plots, generator(replacementPlotId)),
        Right(plot.PlotId.from(replacementPlotId))
      )
      val deletedSpeciesInput = cycle.Registration
        .from(species.SpeciesId.from(speciesId), plot.PlotId.from(replacementPlotId), polygon)
        .fold(e => fail(e.toString), identity)
      assert(
        cycle
          .register(deletedSpeciesInput)(using
            cycles,
            generator(Id.generate().fold(error => fail(error.toString), identity))
          )
          == Left(ActionError.InvalidReference("cycle"))
      )
      assert(
        species
          .delete(species.Delete(species.SpeciesId.from(speciesId)))(using speciesStore)
          .isLeft
      )
    }
