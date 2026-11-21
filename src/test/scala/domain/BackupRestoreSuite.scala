package rizoma.domain

import rizoma.persistence.{SqliteBackup, SqliteDatabase}

import java.nio.file.{Files, Path}

class BackupRestoreSuite extends munit.FunSuite:
  test("backup and restore preserve committed data and never overwrite files"):
    val dir = Files.createTempDirectory(Path.of("target"), "backup-test-")
    val original = dir.resolve("original.sqlite")
    val snapshot = dir.resolve("snapshot.sqlite")
    val restored = dir.resolve("restored.sqlite")
    val db = SqliteDatabase.open(s"jdbc:sqlite:$original")
    SqliteDatabase.initialize(db)
    val _ = db.execute(
      "INSERT INTO species (id, common_name, scientific_name, deleted) VALUES (?, ?, ?, 0)",
      Seq("sample", "Corn", "Zea mays")
    )
    SqliteBackup.backup(db, snapshot).get
    val _ = db.execute("UPDATE species SET common_name = 'Maize' WHERE id = 'sample'")
    SqliteBackup.restore(snapshot, restored).get
    val copy = SqliteDatabase.open(s"jdbc:sqlite:$restored")
    assertEquals(
      copy.queryOne("SELECT common_name FROM species WHERE id = 'sample'")(_.getString(1)),
      Some("Corn")
    )
    assert(SqliteBackup.restore(snapshot, restored).isFailure)
    assert(SqliteBackup.backup(db, snapshot).isFailure)
    val invalid = dir.resolve("invalid.sqlite")
    val _ = Files.writeString(invalid, "not a database")
    val invalidDestination = dir.resolve("invalid-restored.sqlite")
    assert(SqliteBackup.restore(invalid, invalidDestination).isFailure)
    assert(!Files.exists(invalidDestination))
    val incomplete = dir.resolve("incomplete.sqlite")
    val incompleteDb = SqliteDatabase.open(s"jdbc:sqlite:$incomplete")
    val _ = incompleteDb.execute(
      "CREATE TABLE schema_migrations (version INTEGER PRIMARY KEY, name TEXT NOT NULL)"
    )
    val _ = incompleteDb.execute(
      "INSERT INTO schema_migrations VALUES (1, 'baseline'), (2, 'access'), (3, 'tasks')"
    )
    assert(SqliteBackup.restore(incomplete, dir.resolve("incomplete-restored.sqlite")).isFailure)
