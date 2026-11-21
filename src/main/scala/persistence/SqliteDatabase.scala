package rizoma.persistence

import org.sqlite.{Function, SQLiteConfig, SQLiteDataSource}
import rizoma.prelude.Id
import rizoma.prelude.jdbc.Database

/** Creates SQLite connections with referential integrity enabled on every connection. */
object SqliteDatabase:
  def open(url: String): Database =
    val config = new SQLiteConfig()
    config.enforceForeignKeys(true)
    val ds = new SQLiteDataSource(config)
    ds.setUrl(url)
    actor(new Database(ds), "system")

  private def actor(db: Database, identity: String): Database =
    db.withConnectionInitializer(conn =>
      Function.create(
        conn,
        "rizoma_actor",
        new Function {
          override def xFunc(): Unit = result(identity)
        }
      )
    )

  /** The caller is responsible for authenticating this identity. */
  def asUser(db: Database, id: Id): Database = actor(db, id.value)

  /** Applies each schema version atomically, including upgrades from unversioned databases. */
  @SuppressWarnings(Array("scalafix:DisableSyntax"))
  def initialize(db: Database): Unit = db.transaction { session =>
    val _ = session.execute(
      "CREATE TABLE IF NOT EXISTS schema_migrations (version INTEGER PRIMARY KEY, name TEXT NOT NULL)"
    )
    val _ = session.execute("UPDATE schema_migrations SET version = version")
    val versions =
      session.query("SELECT version FROM schema_migrations ORDER BY version")(_.getInt(1))
    if versions != versions.indices.map(_ + 1).toList || versions.exists(_ > 3) then
      throw new IllegalStateException("Unsupported or inconsistent schema migration history")
    if versions.isEmpty then
      new SqliteFarms(db).initialize(session)
      new SqlitePlots(db).initialize(session)
      new SqliteSpecies(db).initialize(session)
      new SqliteCycles(db).initialize(session)
      new SqliteActivities(db).initialize(session)
      new SqliteHarvests(db).initialize(session)
      new SqliteInventory(db).initialize(session)
      val _ =
        session.execute("INSERT INTO schema_migrations (version, name) VALUES (1, 'baseline')")
    if !versions.contains(2) then
      SqliteAccess.initialize(session)
      SqliteAudit.initialize(session)
      val _ = session.execute(
        "INSERT INTO schema_migrations (version, name) VALUES (2, 'access and audit')"
      )
    if !versions.contains(3) then
      SqliteTasks.initialize(session)
      SqliteAudit.attach(session, "tasks", "id")
      val _ =
        session.execute("INSERT INTO schema_migrations (version, name) VALUES (3, 'task planning')")
  }
