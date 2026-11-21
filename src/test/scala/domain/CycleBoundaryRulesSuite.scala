package rizoma.domain

import rizoma.domain.crop.cycle
import rizoma.prelude.Id

class CycleBoundaryRulesSuite extends BusinessFixture:
  test("cycle land and plot boundary changes remain consistent"):
    withFixture { f =>
      import f.*
      seed()
      def pos(x: Double, y: Double): Position = Position.from(x, y).getOrElse(fail("position"))
      def square(a: Double, b: Double): Polygon = Polygon
        .from(List(pos(a, a), pos(b, a), pos(b, b), pos(a, b), pos(a, a)))
        .getOrElse(fail("polygon"))
      def multi(p: Polygon): MultiPolygon =
        MultiPolygon.from(List(p)).getOrElse(fail("multipolygon"))
      val outer = square(0, 10)
      val inside = square(1, 2)
      val outside = square(11, 12)
      val invalid = Polygon
        .from(List(pos(0, 0), pos(2, 2), pos(0, 2), pos(2, 0), pos(0, 0)))
        .getOrElse(fail("polygon"))
      assert(plot.mark(plot.Mark(plot.PlotId.from(plotId), multi(invalid)))(using plots).isLeft)
      assertEquals(
        plot.mark(plot.Mark(plot.PlotId.from(plotId), multi(outer)))(using plots),
        Right(())
      )
      def command(p: Polygon) = cycle.Registration
        .from(cycleInput.speciesId, cycleInput.plotId, p)
        .fold(e => fail(e.toString), identity)
      def fresh = Id.generate().fold(e => fail(e.toString), identity)
      val activeId = fresh
      assertEquals(
        cycle.register(command(inside))(using cycles, generator(activeId)),
        Right(cycle.CycleId.from(activeId))
      )
      assert(cycle.register(command(outside))(using cycles, generator(fresh)).isLeft)
      assert(plot.mark(plot.Mark(plot.PlotId.from(plotId), multi(outside)))(using plots).isLeft)
      val plotView = plot.View(multi(outer), area, plotName, farmId, plot.PlotId.from(plotId))
      val plotPatch = plotView
        .patch(multi(outside), area, plotName.toString, plot.Status.Active)
        .fold(e => fail(e.toString), identity)
      assert(plot.edit(plotPatch)(using plots).isLeft)
      val current = cycle.View(
        cycle.CycleId.from(activeId),
        cycleInput.speciesId,
        cycleInput.plotId,
        inside,
        command(inside).period
      )
      val patch = current.patch(cycleInput.speciesId, outside).fold(e => fail(e.toString), identity)
      assert(cycle.edit(patch)(using cycles).isLeft)
      assertEquals(plot.demark(plot.Demark(plot.PlotId.from(plotId)))(using plots), Right(()))
      val outsideId = fresh
      assert(cycle.register(command(outside))(using cycles, generator(outsideId)).isRight)
      assert(plot.mark(plot.Mark(plot.PlotId.from(plotId), multi(outer)))(using plots).isLeft)
      assertEquals(
        cycle.delete(cycle.Delete(cycle.CycleId.from(outsideId)))(using cycles),
        Right(())
      )
      assertEquals(
        cycle.delete(cycle.Delete(cycle.CycleId.from(activeId)))(using cycles),
        Right(())
      )
      val historicalId = fresh
      assertEquals(
        cycle.plant(cycle.Plant(cycleInput.speciesId, cycleInput.plotId, inside, at))(using
          cycles,
          generator(historicalId)
        ),
        Right(cycle.CycleId.from(historicalId))
      )
      assertEquals(
        cycle.finish(cycle.Finish(cycle.CycleId.from(historicalId), at))(using cycles),
        Right(())
      )
      assertEquals(
        cycle.finish(cycle.Finish(cycle.CycleId.from(cycleId), at))(using cycles),
        Right(())
      )
      assertEquals(
        plot.mark(plot.Mark(plot.PlotId.from(plotId), multi(outside)))(using plots),
        Right(())
      )
    }
