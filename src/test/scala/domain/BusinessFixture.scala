package rizoma.domain

import org.sqlite.SQLiteDataSource
import rizoma.domain.crop.{activity, cycle, species}
import rizoma.persistence.{
  SqliteActivities,
  SqliteCycles,
  SqliteDatabase,
  SqliteFarms,
  SqliteHarvests,
  SqlitePlots,
  SqliteSpecies
}
import rizoma.prelude.Prelude.IdGenerator
import rizoma.prelude.jdbc.Database
import rizoma.prelude.{Id, InvalidGeneration}

import java.time.Instant

private[domain] trait BusinessFixture extends munit.FunSuite:
  final class BusinessData(val db: Database):
    val farms = new SqliteFarms(db)
    val plots = new SqlitePlots(db)
    val speciesStore = new SqliteSpecies(db)
    val cycles = new SqliteCycles(db)
    val activities = new SqliteActivities(db)
    val harvests = new SqliteHarvests(db)
    SqliteDatabase.initialize(db)
    SqliteDatabase.initialize(db)
    val farmId: Id = Id.generate().fold(error => fail(error.toString), identity)
    val plotId: Id = Id.generate().fold(error => fail(error.toString), identity)
    val speciesId: Id = Id.generate().fold(error => fail(error.toString), identity)
    val cycleId: Id = Id.generate().fold(error => fail(error.toString), identity)
    val activityId: Id = Id.generate().fold(error => fail(error.toString), identity)
    val harvestId: Id = Id.generate().fold(error => fail(error.toString), identity)
    def generator(id: Id): IdGenerator = new IdGenerator:
      def next(): Either[rizoma.prelude.InvalidGeneration, Id] = Right(id)
    val generationFailure: InvalidGeneration = InvalidGeneration(None)
    val failingGenerator: IdGenerator = new IdGenerator:
      def next(): Either[InvalidGeneration, Id] = Left(generationFailure)
    val position: Position = Position.from(1, 2).getOrElse(fail("position"))
    val area: Area = Area.from(10).fold(e => fail(e.toString), identity)
    val farmInput: farm.Registration =
      farm.Registration.from("Farm", position, area).fold(e => fail(e.toString), identity)
    val polygon: Polygon = Polygon.from(Nil).getOrElse(fail("polygon"))
    val geometry: MultiPolygon = MultiPolygon.from(List(polygon)).getOrElse(fail("geometry"))
    val plotName: plot.Name = plot.Name.from("Plot").fold(e => fail(e.toString), identity)
    val plotInput: plot.Registration = plot.Registration(geometry, area, plotName, farmId)
    val speciesInput: species.Registration =
      species.Registration.from("Corn", "Zea mays").fold(e => fail(e.toString), identity)
    val at: Instant = Instant.parse("2024-01-01T00:00:00.123456789Z")
    val cycleInput: cycle.Registration = cycle.Registration
      .from(species.SpeciesId.from(speciesId), plot.PlotId.from(plotId), polygon, Some(at))
      .fold(e => fail(e.toString), identity)
    val activityInput: activity.Registration = activity.Registration
      .from(cycle.CycleId.from(cycleId), " Sowing ", at)
      .fold(e => fail(e.toString), identity)
    val harvestInput: harvest.Registration = harvest.Registration
      .from(cycle.CycleId.from(cycleId), 5, at, harvest.UnitOfYield.Pounds)
      .fold(e => fail(e.toString), identity)

    def seed(): Unit =
      assertEquals(farm.register(farmInput)(using farms, generator(farmId)), Right(farmId))
      assertEquals(
        plot.register(plotInput)(using plots, failingGenerator),
        Left(ActionError.Generation(generationFailure))
      )
      assertEquals(
        plot.register(plotInput)(using plots, generator(plotId)),
        Right(plot.PlotId.from(plotId))
      )
      assertEquals(
        plot.register(
          plotInput.copy(farmId = Id.generate().fold(error => fail(error.toString), identity))
        )(using plots, generator(Id.generate().fold(error => fail(error.toString), identity))),
        Left(ActionError.InvalidReference("plot"))
      )
      assertEquals(
        species.register(speciesInput)(using speciesStore, failingGenerator),
        Left(ActionError.Generation(generationFailure))
      )
      assertEquals(
        species.register(speciesInput)(using speciesStore, generator(speciesId)),
        Right(species.SpeciesId.from(speciesId))
      )
      assertEquals(
        cycle.register(cycleInput)(using cycles, failingGenerator),
        Left(ActionError.Generation(generationFailure))
      )
      assertEquals(
        cycle.register(cycleInput)(using cycles, generator(cycleId)),
        Right(cycle.CycleId.from(cycleId))
      )
      assertEquals(
        plot.delete(plot.Delete(plot.PlotId.from(plotId)))(using plots),
        Left(ActionError.HasDependents("plot", plotId.value))
      )
      assertEquals(db.queryOne("SELECT COUNT(*) FROM plots")(_.getInt(1)), Some(1))
      assertEquals(
        db.queryOne("SELECT started_at IS NOT NULL AND ended_at IS NULL FROM cycles")(_.getInt(1)),
        Some(1)
      )
      assertEquals(
        activity.record(activityInput)(using activities, failingGenerator),
        Left(ActionError.Generation(generationFailure))
      )
      assertEquals(
        activity.register(activityInput)(using activities, generator(activityId)),
        Right(activity.ActivityId.from(activityId))
      )
      assertEquals(
        harvest.register(harvestInput)(using harvests, failingGenerator),
        Left(ActionError.Generation(generationFailure))
      )
      assertEquals(
        harvest.register(harvestInput)(using harvests, generator(harvestId)),
        Right(harvest.HarvestId.from(harvestId))
      )

  def withFixture(run: BusinessData => Unit): Unit =
    val url = s"jdbc:sqlite:file:${java.util.UUID.randomUUID()}?mode=memory&cache=shared"
    val ds = new SQLiteDataSource()
    ds.setUrl(url)
    val anchor = ds.getConnection()
    try run(new BusinessData(SqliteDatabase.open(url)))
    finally anchor.close()
