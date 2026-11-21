package rizoma.domain

import org.sqlite.SQLiteDataSource
import rizoma.persistence.SqliteDatabase

class SchemaMigrationSuite extends munit.FunSuite:
  test("migrations are versioned, repeatable and reject newer schemas"):
    val url = s"jdbc:sqlite:file:${java.util.UUID.randomUUID()}?mode=memory&cache=shared"
    val source = new SQLiteDataSource()
    source.setUrl(url)
    val anchor = source.getConnection()
    try
      val db = SqliteDatabase.open(url)
      SqliteDatabase.initialize(db)
      val version = db
        .queryOne("SELECT version FROM schema_migrations ORDER BY version DESC LIMIT 1")(
          _.getInt(1)
        )
        .getOrElse(fail("missing version"))
      assert(version >= 1)
      SqliteDatabase.initialize(db)
      assertEquals(
        db.queryOne("SELECT COUNT(*) FROM schema_migrations")(_.getInt(1)),
        Some(version)
      )
      val _ = db.execute(
        "INSERT INTO schema_migrations (version, name) VALUES (?, ?)",
        Seq(version + 1, "future")
      )
      intercept[IllegalStateException](SqliteDatabase.initialize(db))
    finally anchor.close()

  test("an unsuccessful legacy upgrade rolls back the migration marker and new tables"):
    val url = s"jdbc:sqlite:file:${java.util.UUID.randomUUID()}?mode=memory&cache=shared"
    val source = new SQLiteDataSource()
    source.setUrl(url)
    val anchor = source.getConnection()
    try
      val db = SqliteDatabase.open(url)
      val _ = db.execute("CREATE TABLE stock_items (id TEXT PRIMARY KEY)")
      assert(scala.util.Try(SqliteDatabase.initialize(db)).isFailure)
      assertEquals(
        db.queryOne("SELECT 1 FROM sqlite_master WHERE name = 'schema_migrations'")(_ => true),
        None
      )
      assertEquals(db.queryOne("SELECT 1 FROM sqlite_master WHERE name = 'farms'")(_ => true), None)
    finally anchor.close()
