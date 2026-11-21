package rizoma.domain

import org.sqlite.SQLiteDataSource
import rizoma.domain.crop.{cycle, species}
import rizoma.persistence.{SqliteCycles, SqliteDatabase, SqliteHarvests}
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

import java.time.Instant

class CropOperationsSuite extends munit.FunSuite:
  test("plant creates a started cycle and harvest records yield without finishing it"):
    val url = s"jdbc:sqlite:file:${java.util.UUID.randomUUID()}?mode=memory&cache=shared"
    val ds = new SQLiteDataSource()
    ds.setUrl(url)
    val anchor = ds.getConnection()
    try
      val db = SqliteDatabase.open(url)
      SqliteDatabase.initialize(db)
      val farmId = Id.generate().fold(error => fail(error.toString), identity)
      val plotId = Id.generate().fold(error => fail(error.toString), identity)
      val speciesId = Id.generate().fold(error => fail(error.toString), identity)
      val cycleId = Id.generate().fold(error => fail(error.toString), identity)
      val harvestId = Id.generate().fold(error => fail(error.toString), identity)
      def ids(id: Id): IdGenerator = new IdGenerator:
        def next(): Either[rizoma.prelude.InvalidGeneration, Id] = Right(id)
      val _ = db.execute(
        "INSERT INTO farms VALUES (?, ?, ?, ?, ?)",
        Seq(farmId.value, "Farm", "[0,0]", 1.0, "acres")
      )
      val _ = db.execute(
        "INSERT INTO plots (id, farm_id, name, geometry, area, unit_of_area, status) VALUES (?, ?, ?, ?, ?, ?, ?)",
        Seq(
          plotId.value,
          farmId.value,
          "North",
          "{\"type\":\"MultiPolygon\",\"coordinates\":[]}",
          1.0,
          "acres",
          "Active"
        )
      )
      val _ = db.execute(
        "INSERT INTO species (id, common_name, scientific_name) VALUES (?, ?, ?)",
        Seq(speciesId.value, "Maize", "Zea mays")
      )
      val polygon = Polygon.from(Nil).getOrElse(fail("polygon"))
      val plantedAt = Instant.parse("2024-05-01T00:00:00.123456789Z")
      val command =
        cycle.Plant(species.SpeciesId.from(speciesId), plot.PlotId.from(plotId), polygon, plantedAt)
      val cycles = new SqliteCycles(db)
      val harvests = new SqliteHarvests(db)
      assertEquals(
        cycle.plant(command)(using cycles, ids(cycleId)),
        Right(cycle.CycleId.from(cycleId))
      )
      assertEquals(
        db.queryOne("SELECT species_id, plot_id, started_at, ended_at IS NULL FROM cycles")(row =>
          (row.getString(1), row.getString(2), row.getString(3), row.getInt(4))
        ),
        Some((speciesId.value, plotId.value, plantedAt.toString, 1))
      )
      val harvestedAt = plantedAt.plusSeconds(3600)
      val harvestInput = harvest.Registration
        .from(cycle.CycleId.from(cycleId), 42, harvestedAt, harvest.UnitOfYield.Kilograms)
        .fold(e => fail(e.toString), identity)
      assertEquals(
        cycle.harvest(harvestInput)(using harvests, ids(harvestId)),
        Right(harvest.HarvestId.from(harvestId))
      )
      assertEquals(
        db.queryOne("SELECT cycle_id, amount, unit_of_yield, harvested_at FROM harvests")(row =>
          (row.getString(1), row.getDouble(2), row.getString(3), row.getString(4))
        ),
        Some((cycleId.value, 42.0, "Kilograms", harvestedAt.toString))
      )
      assertEquals(db.queryOne("SELECT ended_at IS NULL FROM cycles")(_.getInt(1)), Some(1))
      assertEquals(
        cycle.finish(cycle.Finish(cycle.CycleId.from(cycleId), harvestedAt))(using cycles),
        Right(())
      )
      assert(
        cycle
          .plant(
            command.copy(plotId =
              plot.PlotId.from(Id.generate().fold(error => fail(error.toString), identity))
            )
          )(using
            cycles,
            ids(Id.generate().fold(error => fail(error.toString), identity))
          )
          .isLeft
      )
      val orphan = harvest.Registration
        .from(
          cycle.CycleId.from(Id.generate().fold(error => fail(error.toString), identity)),
          1,
          harvestedAt
        )
        .fold(e => fail(e.toString), identity)
      assert(
        cycle
          .harvest(orphan)(using
            harvests,
            ids(Id.generate().fold(error => fail(error.toString), identity))
          )
          .isLeft
      )
    finally anchor.close()
