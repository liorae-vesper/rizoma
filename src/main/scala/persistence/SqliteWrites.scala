package rizoma.persistence

import rizoma.domain.ActionError
import rizoma.prelude.jdbc.{Database, SqlParam, SqlSession}

import scala.util.{Failure, Success, Try}

private[persistence] abstract class SqliteWrites(db: Database, table: String, tableName: String):
  protected def schema(sql: String): Unit = db.transaction(session => schema(session, sql))

  protected def schema(session: SqlSession, sql: String): Unit =
    val _ = session.execute(sql)
    val columns = session.query(s"PRAGMA table_info($tableName)")(_.getString("name"))
    if !columns.contains("deleted") then
      val _ =
        session.execute(s"ALTER TABLE $tableName ADD COLUMN deleted INTEGER NOT NULL DEFAULT 0")

  protected def write(sql: String, params: Seq[SqlParam]): Try[Unit] =
    Try(db.execute(sql, params)).flatMap { count =>
      if count == 1 then Success(())
      else Failure(ActionError.InvalidReference(table))
    }

  protected def writeForId(sql: String, params: Seq[SqlParam], id: String): Try[Unit] =
    Try(db.execute(sql, params)).flatMap { count =>
      if count == 1 then Success(()) else Failure(ActionError.NotFound(table, id))
    }

  protected def writeExisting(
      sql: String,
      params: Seq[SqlParam],
      id: String,
      onExisting: SqlSession => ActionError
  ): Try[Unit] =
    Try(db.transaction { session =>
      val count = session.execute(sql, params)
      if count == 1 then Success(())
      else
        val exists = session
          .queryOne(s"SELECT 1 FROM $tableName WHERE id = ? AND deleted = 0", Seq(id))(_ => true)
          .getOrElse(false)
        if exists then Failure(onExisting(session)) else Failure(ActionError.NotFound(table, id))
    }).flatten
