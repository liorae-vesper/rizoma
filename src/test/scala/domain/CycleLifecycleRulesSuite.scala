package rizoma.domain

import rizoma.domain.crop.cycle
import rizoma.prelude.Id

class CycleLifecycleRulesSuite extends BusinessFixture:
  test("harvest requires an active cycle and valid time, including corrections and finishing"):
    withFixture { f =>
      import f.*
      seed()
      def fresh: Id = Id.generate().fold(e => fail(e.toString), identity)
      val plannedId = fresh
      val planned = cycle.Registration
        .from(cycleInput.speciesId, cycleInput.plotId, polygon)
        .fold(e => fail(e.toString), identity)
      assert(cycle.register(planned)(using cycles, generator(plannedId)).isRight)
      val plannedHarvest = harvest.Registration
        .from(cycle.CycleId.from(plannedId), 1, at)
        .fold(e => fail(e.toString), identity)
      assert(harvest.register(plannedHarvest)(using harvests, generator(fresh)).isLeft)
      val before = harvest.Registration
        .from(harvestInput.cycleId, 1, at.minusSeconds(1))
        .fold(e => fail(e.toString), identity)
      assert(harvest.register(before)(using harvests, generator(fresh)).isLeft)
      val later = harvest.Registration
        .from(harvestInput.cycleId, 1, at.plusSeconds(2))
        .fold(e => fail(e.toString), identity)
      assert(harvest.register(later)(using harvests, generator(fresh)).isRight)
      assert(
        cycle.finish(cycle.Finish(harvestInput.cycleId, at.plusSeconds(1)))(using cycles).isLeft
      )
      val cycleView = cycle.View(
        harvestInput.cycleId,
        cycleInput.speciesId,
        cycleInput.plotId,
        polygon,
        cycleInput.period
      )
      val badPeriod = cycleView
        .patch(cycleInput.speciesId, polygon, Some(at.plusSeconds(1)))
        .fold(e => fail(e.toString), identity)
      assert(cycle.edit(badPeriod)(using cycles).isLeft)
      assert(
        cycle.finish(cycle.Finish(harvestInput.cycleId, at.minusNanos(1)))(using cycles).isLeft
      )
      assertEquals(
        cycle.finish(cycle.Finish(harvestInput.cycleId, at.plusSeconds(2)))(using cycles),
        Right(())
      )
      assert(harvest.register(harvestInput)(using harvests, generator(fresh)).isLeft)
      val view = harvest.View(
        harvest.HarvestId.from(harvestId),
        harvestInput.cycleId,
        harvestInput.yieldAmount,
        at
      )
      assert(
        harvest
          .edit(view.patch(1, at.plusSeconds(3)).fold(e => fail(e.toString), identity))(using
            harvests
          )
          .isLeft
      )
      assertEquals(
        harvest.edit(view.patch(2, at).fold(e => fail(e.toString), identity))(using harvests),
        Right(())
      )
    }
