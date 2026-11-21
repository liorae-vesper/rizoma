package rizoma.domain

import org.sqlite.SQLiteDataSource
import rizoma.domain.crop.species
import rizoma.persistence.{SqliteDatabase, SqliteSpecies}
import rizoma.prelude.Id

class SoftDeleteMigrationSuite extends munit.FunSuite:
  test("initialization upgrades legacy species data without losing records"):
    val url = s"jdbc:sqlite:file:${java.util.UUID.randomUUID()}?mode=memory&cache=shared"
    val ds = new SQLiteDataSource()
    ds.setUrl(url)
    val anchor = ds.getConnection()
    try
      val db = SqliteDatabase.open(url)
      val id = Id.generate().fold(error => fail(error.toString), identity)
      val _ = db.execute(
        "CREATE TABLE species (id TEXT PRIMARY KEY, common_name TEXT NOT NULL, scientific_name TEXT NOT NULL)"
      )
      val _ = db.execute("INSERT INTO species VALUES (?, ?, ?)", Seq(id.value, "Corn", "Zea mays"))
      SqliteDatabase.initialize(db)
      SqliteDatabase.initialize(db)
      val store = new SqliteSpecies(db)
      assert(species.get(species.SpeciesId.from(id))(using store).exists(_.nonEmpty))
      assertEquals(
        species.delete(species.Delete(species.SpeciesId.from(id)))(using store),
        Right(())
      )
      assertEquals(species.get(species.SpeciesId.from(id))(using store), Right(None))
      assertEquals(
        db.queryOne("SELECT common_name, deleted FROM species WHERE id = ?", Seq(id.value))(row =>
          (row.getString(1), row.getInt(2))
        ),
        Some(("Corn", 1))
      )
    finally anchor.close()
