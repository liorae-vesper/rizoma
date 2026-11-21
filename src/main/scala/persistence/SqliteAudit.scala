package rizoma.persistence

import rizoma.prelude.jdbc.{Database, SqlSession}

import scala.util.Try

final case class AuditEntry(
    sequence: Long,
    table: String,
    entity: String,
    operation: String,
    actor: String,
    at: String,
    before: Option[String],
    after: Option[String]
)

object SqliteAudit:
  private[persistence] def initialize(session: SqlSession): Unit =
    val _ = session.execute(
      "CREATE TABLE IF NOT EXISTS audit_history (sequence INTEGER PRIMARY KEY AUTOINCREMENT, entity_table TEXT NOT NULL, entity_id TEXT NOT NULL, operation TEXT NOT NULL, actor TEXT NOT NULL, occurred_at TEXT NOT NULL, before_json TEXT, after_json TEXT)"
    )
    val _ = session.execute(
      "CREATE TRIGGER IF NOT EXISTS audit_history_no_update BEFORE UPDATE ON audit_history BEGIN SELECT RAISE(ABORT, 'audit history is immutable'); END"
    )
    val _ = session.execute(
      "CREATE TRIGGER IF NOT EXISTS audit_history_no_delete BEFORE DELETE ON audit_history BEGIN SELECT RAISE(ABORT, 'audit history is immutable'); END"
    )
    val tables = List(
      ("farms", "id"),
      ("plots", "id"),
      ("species", "id"),
      ("cycles", "id"),
      ("activities", "id"),
      ("harvests", "id"),
      ("stock_items", "id"),
      ("stock_balances", "stock_id || ':' || location"),
      ("stock_receipt_origins", "stock_id"),
      ("stock_movements", "sequence"),
      ("harvest_lot_origins", "stock_id"),
      ("users", "id"),
      ("farm_memberships", "farm_id || ':' || user_id")
    )
    tables.foreach { (table, id) => attach(session, table, id) }

  private[persistence] def attach(session: SqlSession, table: String, id: String): Unit =
    val columns = session.query(s"PRAGMA table_info($table)")(_.getString("name"))
    def json(prefix: String): String =
      columns.map(c => s"'$c', $prefix.\"$c\"").mkString("json_object(", ",", ")")
    def key(prefix: String): String =
      id.split(" ").map(c => if columns.contains(c) then s"$prefix.$c" else c).mkString(" ")
    val changed = columns.map(c => s"old.\"$c\" IS NOT new.\"$c\"").mkString(" OR ")
    List(
      ("insert", "new", "NULL", json("new"), ""),
      ("update", "new", json("old"), json("new"), s" WHEN $changed"),
      ("delete", "old", json("old"), "NULL", "")
    ).foreach { (operation, ref, before, after, condition) =>
      val _ = session.execute(
        s"CREATE TRIGGER IF NOT EXISTS audit_${table}_$operation AFTER ${operation.toUpperCase} ON $table$condition BEGIN INSERT INTO audit_history (entity_table, entity_id, operation, actor, occurred_at, before_json, after_json) VALUES ('$table', ${key(ref)}, '$operation', rizoma_actor(), strftime('%Y-%m-%dT%H:%M:%fZ','now'), $before, $after); END"
      )
    }

final class SqliteAudit(db: Database):
  def history(table: String, entity: String): Try[List[AuditEntry]] = Try {
    db.query(
      "SELECT sequence, entity_table, entity_id, operation, actor, occurred_at, before_json, after_json FROM audit_history WHERE entity_table = ? AND entity_id = ? ORDER BY sequence",
      Seq(table, entity)
    ) { r =>
      AuditEntry(
        r.getLong(1),
        r.getString(2),
        r.getString(3),
        r.getString(4),
        r.getString(5),
        r.getString(6),
        Option(r.getString(7)),
        Option(r.getString(8))
      )
    }
  }
