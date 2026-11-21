package rizoma.domain

import org.sqlite.SQLiteDataSource
import rizoma.domain.crop.activity
import rizoma.persistence.{
  SqliteActivities,
  SqliteCycles,
  SqliteDatabase,
  SqliteFarms,
  SqlitePlots,
  SqliteSpecies
}
import rizoma.prelude.Id

class FieldJournalMigrationSuite extends munit.FunSuite:
  test("initialization retains legacy activities and makes journal details optional"):
    val url = s"jdbc:sqlite:file:${java.util.UUID.randomUUID()}?mode=memory&cache=shared"
    val ds = new SQLiteDataSource()
    ds.setUrl(url)
    val anchor = ds.getConnection()
    try
      val db = SqliteDatabase.open(url)
      new SqliteFarms(db).initialize()
      new SqlitePlots(db).initialize()
      new SqliteSpecies(db).initialize()
      new SqliteCycles(db).initialize()
      val _ = db.execute(
        "CREATE TABLE activities (id TEXT PRIMARY KEY, cycle_id TEXT NOT NULL REFERENCES cycles(id), event TEXT NOT NULL, occurred_at TEXT NOT NULL, deleted INTEGER NOT NULL DEFAULT 0)"
      )
      def id(): Id = Id.generate().fold(e => fail(e.toString), identity)
      val farmId = id()
      val plotId = id()
      val speciesId = id()
      val cycleId = id()
      val activityId = id()
      val _ = db.execute(
        "INSERT INTO farms VALUES (?, ?, ?, ?, ?)",
        Seq(farmId.value, "Farm", "[0,0]", 1.0, "acres")
      )
      val _ = db.execute(
        "INSERT INTO plots (id, farm_id, name, geometry, area, unit_of_area, status) VALUES (?, ?, ?, ?, ?, ?, ?)",
        Seq(plotId.value, farmId.value, "Plot", "{}", 1.0, "acres", "Active")
      )
      val _ = db.execute(
        "INSERT INTO species (id, common_name, scientific_name) VALUES (?, ?, ?)",
        Seq(speciesId.value, "Corn", "Zea mays")
      )
      val _ = db.execute(
        "INSERT INTO cycles (id, plot_id, species_id, occupied_land) VALUES (?, ?, ?, ?)",
        Seq(cycleId.value, plotId.value, speciesId.value, "{}")
      )
      val _ = db.execute(
        "INSERT INTO activities (id, cycle_id, event, occurred_at) VALUES (?, ?, ?, ?)",
        Seq(activityId.value, cycleId.value, "Old note", "2024-01-01T00:00:00Z")
      )
      SqliteDatabase.initialize(db)
      SqliteDatabase.initialize(db)
      val store = new SqliteActivities(db)
      val entry = activity
        .get(activity.ActivityId.from(activityId))(using store)
        .fold(e => fail(e.toString), identity)
        .getOrElse(fail("missing legacy entry"))
      assertEquals(entry.event.toString, "Old note")
      assertEquals(entry.journal, activity.Journal.empty)
      val notes = activity.Journal
        .from(None, Some("Updated"), Nil, Nil, Nil)
        .fold(e => fail(e.toString), identity)
      val patch =
        entry.patch("Old note", entry.occurredAt, notes).fold(e => fail(e.toString), identity)
      assertEquals(activity.edit(patch)(using store), Right(()))
      assertEquals(
        activity.get(activity.ActivityId.from(activityId))(using store).map(_.map(_.journal)),
        Right(Some(notes))
      )
    finally anchor.close()
