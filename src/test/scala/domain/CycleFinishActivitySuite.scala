package rizoma.domain

import org.sqlite.SQLiteDataSource
import rizoma.domain.crop.{activity, cycle, species}
import rizoma.persistence.{
  SqliteActivities,
  SqliteCycles,
  SqliteDatabase,
  SqlitePlots,
  SqliteSpecies
}
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

import java.time.Instant

class CycleFinishActivitySuite extends munit.FunSuite:
  test("cycle finishing and activity recording use SQLite"):
    val url = s"jdbc:sqlite:file:${java.util.UUID.randomUUID()}?mode=memory&cache=shared"
    val ds = new SQLiteDataSource()
    ds.setUrl(url)
    val anchor = ds.getConnection()
    try
      val db = SqliteDatabase.open(url)
      SqliteDatabase.initialize(db)
      val plots = new SqlitePlots(db)
      val speciesStore = new SqliteSpecies(db)
      val activities = new SqliteActivities(db)
      val cycles = new SqliteCycles(db)
      def ids(id: Id): IdGenerator = new IdGenerator:
        def next(): Either[rizoma.prelude.InvalidGeneration, Id] = Right(id)
      val farmId = Id.generate().fold(error => fail(error.toString), identity)
      val plotId = Id.generate().fold(error => fail(error.toString), identity)
      val speciesId = Id.generate().fold(error => fail(error.toString), identity)
      val activityId = Id.generate().fold(error => fail(error.toString), identity)
      val cycleId = Id.generate().fold(error => fail(error.toString), identity)
      val _ = db.execute(
        "INSERT INTO farms VALUES (?, ?, ?, ?, ?)",
        Seq(farmId.value, "Farm", "[0,0]", 1.0, "acres")
      )
      val geometry = MultiPolygon.from(Nil).getOrElse(fail("geometry"))
      val area = Area.from(1).fold(e => fail(e.toString), identity)
      val name = plot.Name.from("North").fold(e => fail(e.toString), identity)
      assertEquals(
        plot.register(plot.Registration(geometry, area, name, farmId))(using plots, ids(plotId)),
        Right(plot.PlotId.from(plotId))
      )
      val input =
        species.Registration.from("Corn", "Zea mays").fold(e => fail(e.toString), identity)
      assertEquals(
        species.register(input)(using speciesStore, ids(speciesId)),
        Right(species.SpeciesId.from(speciesId))
      )
      val _ = db.execute(
        "INSERT INTO cycles (id, plot_id, species_id, occupied_land) VALUES (?, ?, ?, ?)",
        Seq(
          cycleId.value,
          plotId.value,
          speciesId.value,
          "{\"type\":\"Polygon\",\"coordinates\":[]}"
        )
      )
      val time = Instant.parse("2025-01-02T12:00:00Z")
      val cycleRef = cycle.CycleId.from(cycleId)
      assert(cycle.finish(cycle.Finish(cycleRef, time))(using cycles).isLeft)
      val _ = db.execute(
        "UPDATE cycles SET started_at = ? WHERE id = ?",
        Seq(time.toString, cycleId.value)
      )
      assert(cycle.finish(cycle.Finish(cycleRef, time.minusSeconds(1)))(using cycles).isLeft)
      assertEquals(
        cycle.finish(cycle.Finish(cycleRef, time.plusSeconds(1)))(using cycles),
        Right(())
      )
      assert(cycle.finish(cycle.Finish(cycleRef, time.plusSeconds(2)))(using cycles).isLeft)
      assertEquals(
        db.queryOne("SELECT ended_at FROM cycles")(_.getString(1)),
        Some(time.plusSeconds(1).toString)
      )
      assert(
        cycle
          .finish(
            cycle.Finish(
              cycle.CycleId.from(Id.generate().fold(error => fail(error.toString), identity)),
              time
            )
          )(using cycles)
          .isLeft
      )
      val event = activity.Registration
        .from(cycle.CycleId.from(cycleId), " Sowing ", time)
        .fold(e => fail(e.toString), identity)
      assertEquals(
        activity.record(event)(using activities, ids(activityId)),
        Right(activity.ActivityId.from(activityId))
      )
      assertEquals(db.queryOne("SELECT event FROM activities")(_.getString(1)), Some("Sowing"))
    finally anchor.close()
