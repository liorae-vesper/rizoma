package rizoma.domain

import io.circe.syntax.*
import org.sqlite.SQLiteDataSource
import rizoma.domain.crop.{cycle, species}
import rizoma.persistence.{SqliteCycles, SqliteDatabase, SqlitePlots}
import rizoma.prelude.Id

import java.time.Instant

class StaleActionsSuite extends munit.FunSuite:
  test("stale patches cannot reopen a finished cycle or overwrite a changed plot"):
    val url = s"jdbc:sqlite:file:${java.util.UUID.randomUUID()}?mode=memory&cache=shared"
    val ds = new SQLiteDataSource()
    ds.setUrl(url)
    val anchor = ds.getConnection()
    try
      val db = SqliteDatabase.open(url)
      SqliteDatabase.initialize(db)
      def id(): Id = Id.generate().fold(e => fail(e.toString), identity)
      val farmId = id()
      val plotId = id()
      val speciesId = id()
      val cycleId = id()
      val _ = db.execute(
        "INSERT INTO farms (id, name, location, total_area, unit_of_area) VALUES (?, ?, ?, ?, ?)",
        Seq(farmId.value, "Farm", "[0,0]", 1.0, "acres")
      )
      val geometry = MultiPolygon.from(Nil).getOrElse(fail("geometry"))
      val polygon = Polygon.from(Nil).getOrElse(fail("polygon"))
      val area = Area.from(1).fold(e => fail(e.toString), identity)
      val name = plot.Name.from("Plot").fold(e => fail(e.toString), identity)
      val _ = db.execute(
        "INSERT INTO plots (id, farm_id, name, geometry, area, unit_of_area, status) VALUES (?, ?, ?, ?, ?, ?, ?)",
        Seq(
          plotId.value,
          farmId.value,
          "Plot",
          geometry.asJson.noSpaces,
          1.0,
          "square_meters",
          "Active"
        )
      )
      val _ = db.execute(
        "INSERT INTO species (id, common_name, scientific_name) VALUES (?, ?, ?)",
        Seq(speciesId.value, "Corn", "Zea mays")
      )
      val started = Instant.parse("2025-01-01T00:00:00Z")
      val ended = started.plusSeconds(1)
      val _ = db.execute(
        "INSERT INTO cycles (id, plot_id, species_id, occupied_land, started_at) VALUES (?, ?, ?, ?, ?)",
        Seq(cycleId.value, plotId.value, speciesId.value, polygon.asJson.noSpaces, started.toString)
      )
      val plots = new SqlitePlots(db)
      val cycles = new SqliteCycles(db)
      val plotRef = plot.PlotId.from(plotId)
      val cycleRef = cycle.CycleId.from(cycleId)
      val period = cycle.Period.from(Some(started), None).fold(e => fail(e.toString), identity)
      val cyclePatch = cycle
        .View(cycleRef, species.SpeciesId.from(speciesId), plotRef, polygon, period)
        .patch(species.SpeciesId.from(speciesId), polygon)
        .fold(e => fail(e.toString), identity)
      val plotPatch = plot
        .View(geometry, area, name, farmId, plotRef)
        .patch(geometry, area, "Renamed")
        .fold(e => fail(e.toString), identity)
      assertEquals(cycle.finish(cycle.Finish(cycleRef, ended))(using cycles), Right(()))
      assertEquals(
        cycle.edit(cyclePatch)(using cycles),
        Left(ActionError.Conflict("cycle", cycleId.value))
      )
      assertEquals(db.queryOne("SELECT ended_at FROM cycles")(_.getString(1)), Some(ended.toString))
      assertEquals(plot.resting(plot.Resting(plotRef))(using plots), Right(()))
      assertEquals(
        plot.edit(plotPatch)(using plots),
        Left(ActionError.Conflict("plot", plotId.value))
      )
      assertEquals(
        db.queryOne("SELECT name, status FROM plots")(r => (r.getString(1), r.getString(2))),
        Some(("Plot", "Resting"))
      )
    finally anchor.close()
