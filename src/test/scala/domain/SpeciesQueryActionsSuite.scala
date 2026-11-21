package rizoma.domain

import org.sqlite.SQLiteDataSource
import rizoma.domain.crop.species
import rizoma.persistence.{SqliteDatabase, SqlitePlots, SqliteSpecies}
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

class SpeciesQueryActionsSuite extends munit.FunSuite:
  test("species queries and removal preserve constraints"):
    val url = s"jdbc:sqlite:file:${java.util.UUID.randomUUID()}?mode=memory&cache=shared"
    val ds = new SQLiteDataSource()
    ds.setUrl(url)
    val anchor = ds.getConnection()
    try
      val db = SqliteDatabase.open(url)
      SqliteDatabase.initialize(db)
      val plots = new SqlitePlots(db)
      val speciesStore = new SqliteSpecies(db)
      def ids(id: Id): IdGenerator = new IdGenerator:
        def next(): Either[rizoma.prelude.InvalidGeneration, Id] = Right(id)
      val farmId = Id.generate().fold(error => fail(error.toString), identity)
      val plotId = Id.generate().fold(error => fail(error.toString), identity)
      val speciesId = Id.generate().fold(error => fail(error.toString), identity)
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
      assertEquals(species.list()(using speciesStore), Right(Nil))
      val input =
        species.Registration.from("Corn", "Zea mays").fold(e => fail(e.toString), identity)
      assertEquals(
        species.register(input)(using speciesStore, ids(speciesId)),
        Right(species.SpeciesId.from(speciesId))
      )
      val expected =
        species.View(species.SpeciesId.from(speciesId), input.commonName, input.scientificName)
      assertEquals(
        species.get(species.SpeciesId.from(speciesId))(using speciesStore),
        Right(Some(expected))
      )
      assertEquals(
        species.get(
          species.SpeciesId.from(Id.generate().fold(error => fail(error.toString), identity))
        )(using speciesStore),
        Right(None)
      )
      assertEquals(species.list()(using speciesStore), Right(List(expected)))
      val _ = db.execute(
        "INSERT INTO cycles (id, plot_id, species_id, occupied_land) VALUES (?, ?, ?, ?)",
        Seq(
          cycleId.value,
          plotId.value,
          speciesId.value,
          "{\"type\":\"Polygon\",\"coordinates\":[]}"
        )
      )
      assert(
        species
          .remove(species.Delete(species.SpeciesId.from(speciesId)))(using speciesStore)
          .isLeft
      )
      val _ = db.execute("DELETE FROM activities")
      val _ = db.execute("DELETE FROM cycles")
      assertEquals(
        species.remove(species.Delete(species.SpeciesId.from(speciesId)))(using speciesStore),
        Right(())
      )
      assertEquals(species.list()(using speciesStore), Right(Nil))
      val _ = db.execute(
        "INSERT INTO species (id, common_name, scientific_name) VALUES (?, ?, ?)",
        Seq("invalid", "Corn", "Zea")
      )
      assert(species.list()(using speciesStore).isLeft)
      val _ = db.execute("DELETE FROM species")
      val _ = db.execute(
        "INSERT INTO species (id, common_name, scientific_name) VALUES (?, ?, ?)",
        Seq(speciesId.value, " ", "Zea")
      )
      assert(species.get(species.SpeciesId.from(speciesId))(using speciesStore).isLeft)
    finally anchor.close()
