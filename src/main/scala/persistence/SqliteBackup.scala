package rizoma.persistence

import rizoma.prelude.jdbc.Database

import java.nio.file.{Files, Path}
import scala.util.Try

object SqliteBackup:
  /** Writes a consistent SQLite snapshot to a new file; existing files are never overwritten. */
  def backup(db: Database, destination: Path): Try[Unit] = Try {
    val target = destination.toAbsolutePath.normalize()
    val _ = Files.createFile(target)
    val _ = db.execute("VACUUM main INTO ?", Seq(target.toString))
  }

  /** Validates a backup and restores it to a new file, without replacing a live database. */
  @SuppressWarnings(Array("scalafix:DisableSyntax"))
  def restore(source: Path, destination: Path): Try[Unit] = Try {
    val input = source.toAbsolutePath.normalize()
    if !Files.isRegularFile(input) then throw new IllegalArgumentException("Backup does not exist")
    val db = SqliteDatabase.open(s"jdbc:sqlite:${input.toUri}?mode=ro")
    val check = db.queryOne("PRAGMA quick_check")(_.getString(1))
    if check != Some("ok") then throw new IllegalArgumentException("Backup integrity check failed")
    val versions = db.query("SELECT version FROM schema_migrations ORDER BY version")(_.getInt(1))
    if versions != List(1, 2, 3) then
      throw new IllegalArgumentException("Unsupported backup schema")
    val required = Set(
      "farms",
      "plots",
      "species",
      "cycles",
      "activities",
      "harvests",
      "stock_items",
      "stock_balances",
      "stock_receipt_origins",
      "stock_movements",
      "harvest_lot_origins",
      "users",
      "farm_memberships",
      "audit_history",
      "tasks"
    )
    val tables =
      db.query("SELECT name FROM sqlite_master WHERE type = 'table'")(_.getString(1)).toSet
    if !required.subsetOf(tables) then
      throw new IllegalArgumentException("Backup schema is incomplete")
    if db.queryOne("PRAGMA foreign_key_check")(_ => true).getOrElse(false) then
      throw new IllegalArgumentException("Backup has invalid references")
    db
  }.flatMap(db => backup(db, destination))
