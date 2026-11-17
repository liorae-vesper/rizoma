package rizoma.prelude.jdbc

import java.sql.{Connection, PreparedStatement, ResultSet, Types}
import javax.sql.DataSource
import scala.util.Using

/** Types accepted as positional SQL parameters.
  *
  * Domain-specific types must be converted to one of these types before being passed to a database
  * operation. Use null for SQL NULL values.
  */
type SqlParam =
  String | Int | Long | Double | Float | Boolean | Array[Byte] | BigDecimal | Null

/** Internal JDBC operations shared by Database and SqlSession.
  *
  * These functions do not own their Connection. The caller manages its lifecycle.
  */
// JDBC represents SQL NULL with null at the driver boundary.
@SuppressWarnings(Array("scalafix:DisableSyntax"))
private object Jdbc:
  /** Binds positional parameters in order, starting at index 1. */
  def bind(stmt: PreparedStatement, params: Seq[SqlParam]): Unit =
    params.zipWithIndex.foreach { case (value, index) =>
      val i = index + 1

      value match
        case null           => stmt.setNull(i, Types.NULL)
        case v: String      => stmt.setString(i, v)
        case v: Int         => stmt.setInt(i, v)
        case v: Long        => stmt.setLong(i, v)
        case v: Double      => stmt.setDouble(i, v)
        case v: Float       => stmt.setFloat(i, v)
        case v: Boolean     => stmt.setBoolean(i, v)
        case v: Array[Byte] => stmt.setBytes(i, v)
        case v: BigDecimal  => stmt.setBigDecimal(i, v.bigDecimal)
    }

  /** Prepares a statement and executes a callback.
    *
    * The statement is closed after the callback finishes, even if an exception occurs. The
    * connection remains open.
    */
  def prepare[A](conn: Connection, sql: String, params: Seq[SqlParam])(
      run: PreparedStatement => A
  ): A =
    Using.resource(conn.prepareStatement(sql)) { stmt =>
      bind(stmt, params)
      run(stmt)
    }

  /** Executes SQL that produces a ResultSet, including SELECT and statements with RETURNING.
    *
    * All rows are decoded before the ResultSet is closed.
    *
    * @return
    *   decoded rows as a List
    */
  def query[A](conn: Connection, sql: String, params: Seq[SqlParam])(
      decode: ResultSet => A
  ): List[A] =
    prepare(conn, sql, params) { stmt =>
      Using.resource(stmt.executeQuery()) { rs =>
        val builder = List.newBuilder[A]

        while rs.next() do builder += decode(rs)

        builder.result()
      }
    }

  /** Executes SQL that produces a ResultSet and decodes only the first row, if present.
    *
    * Additional rows are not decoded. Does not enforce uniqueness or add a SQL LIMIT.
    *
    * @return
    *   Some(value) if a row exists, otherwise None
    */
  def queryOne[A](conn: Connection, sql: String, params: Seq[SqlParam])(
      decode: ResultSet => A
  ): Option[A] =
    prepare(conn, sql, params) { stmt =>
      Using.resource(stmt.executeQuery()) { rs =>
        if rs.next() then Some(decode(rs))
        else None
      }
    }

  /** Executes SQL without a returned ResultSet.
    *
    * Suitable for INSERT, UPDATE, DELETE, and other statements supported by executeUpdate.
    *
    * @return
    *   affected row count as reported by JDBC
    */
  def execute(conn: Connection, sql: String, params: Seq[SqlParam]): Int =
    prepare(conn, sql, params)(_.executeUpdate())

/** A database session bound to an existing connection.
  *
  * Sessions are created by Database.transaction and Database.transactionEither. All operations
  * share the same connection and participate in the same transaction.
  *
  * The session does not own the connection and cannot commit, roll back, or close it. A session
  * must not escape its transaction callback or be shared across threads.
  */
final class SqlSession private[jdbc] (private val conn: Connection):
  /** Executes SQL returning zero or more rows, including SELECT and statements with RETURNING.
    *
    * @param sql
    *   SQL statement
    * @param params
    *   optional positional parameters
    * @param decode
    *   row decoder
    * @return
    *   decoded rows
    */
  def query[A](sql: String, params: Seq[SqlParam] = Seq.empty)(decode: ResultSet => A): List[A] =
    Jdbc.query(conn, sql, params)(decode)

  /** Executes SQL returning at most one decoded row, without enforcing uniqueness.
    *
    * @return
    *   Some(value) or None
    */
  def queryOne[A](sql: String, params: Seq[SqlParam] = Seq.empty)(
      decode: ResultSet => A
  ): Option[A] =
    Jdbc.queryOne(conn, sql, params)(decode)

  /** Executes SQL without a returned ResultSet.
    *
    * @return
    *   affected row count
    */
  def execute(sql: String, params: Seq[SqlParam] = Seq.empty): Int =
    Jdbc.execute(conn, sql, params)

/** Synchronous JDBC wrapper backed by a DataSource.
  *
  * Each independent operation obtains and closes a connection. Transaction callbacks receive a
  * SqlSession, allowing multiple operations to share one connection atomically.
  *
  * Domain failures can be represented with Either through transactionEither. JDBC failures and
  * unexpected exceptions propagate normally.
  *
  * @param ds
  *   connection provider
  */
final class Database(ds: DataSource):
  /** Obtains a connection, executes a callback, and closes it on completion or failure. */
  private def withConnection[A](run: Connection => A): A =
    Using.resource(ds.getConnection)(run)

  /** Executes SQL producing a ResultSet, including SELECT and statements with RETURNING.
    *
    * Each invocation obtains its own connection.
    */
  def query[A](sql: String, params: Seq[SqlParam] = Seq.empty)(decode: ResultSet => A): List[A] =
    withConnection { conn =>
      Jdbc.query(conn, sql, params)(decode)
    }

  /** Executes SQL producing a ResultSet and decodes only the first row.
    *
    * Each invocation obtains its own connection.
    *
    * @return
    *   Some(value) or None
    */
  def queryOne[A](sql: String, params: Seq[SqlParam] = Seq.empty)(
      decode: ResultSet => A
  ): Option[A] =
    withConnection { conn =>
      Jdbc.queryOne(conn, sql, params)(decode)
    }

  /** Executes SQL without a returned ResultSet, obtaining its own connection.
    *
    * @return
    *   affected row count
    */
  def execute(sql: String, params: Seq[SqlParam] = Seq.empty): Int =
    withConnection { conn =>
      Jdbc.execute(conn, sql, params)
    }

  /** Executes a transaction using a configurable commit decision.
    *
    * Obtains a connection, saves and disables autoCommit, executes the callback, and commits or
    * rolls back based on shouldCommit. Finally restores autoCommit and closes the connection.
    *
    * Exceptions from the callback, decision function, or commit trigger a rollback attempt. A
    * commit failure may leave the transaction outcome uncertain. Rollback is attempted at most
    * once; rollback and restoration failures are suppressed on the original exception.
    *
    * The callback must not leak the SqlSession or start additional transactions on the same
    * session. Nested transactions are not supported.
    *
    * @param run
    *   transaction body
    * @param shouldCommit
    *   decision based on the body result
    * @return
    *   result produced by the body
    */
  // JDBC cleanup tracks rollback attempts and rethrows the original error after attaching failures.
  @SuppressWarnings(Array("scalafix:DisableSyntax", "scalafix:SwallowedException"))
  private def runTransaction[A](run: SqlSession => A)(shouldCommit: A => Boolean): A =
    withConnection { conn =>
      val previousAutoCommit = conn.getAutoCommit
      conn.setAutoCommit(false)

      var rollbackAttempted = false
      var failure: Option[Throwable] = None

      try
        try
          val result = run(new SqlSession(conn))

          if shouldCommit(result) then conn.commit()
          else
            rollbackAttempted = true
            conn.rollback()

          result

        catch
          case error: Throwable =>
            failure = Some(error)

            if !rollbackAttempted then
              rollbackAttempted = true

              try conn.rollback()
              catch
                case rollbackError: Throwable =>
                  if rollbackError ne error then error.addSuppressed(rollbackError)

            throw error

      finally
        try conn.setAutoCommit(previousAutoCommit)
        catch
          case restoreError: Throwable =>
            failure match
              case Some(error) =>
                if restoreError ne error then error.addSuppressed(restoreError)
              case None => throw restoreError
    }

  /** Executes a transaction. A normal return commits; an exception triggers rollback.
    *
    * Returning Left, None, false, or another ordinary value does not cause rollback. Use
    * transactionEither when Left should roll back.
    *
    * @param run
    *   transaction body
    * @return
    *   result of the committed transaction
    */
  def transaction[A](run: SqlSession => A): A =
    runTransaction(run)(_ => true)

  /** Executes a transaction returning Either[E, A].
    *
    * Right commits; Left rolls back. Exceptions trigger a rollback attempt and propagate. E can be
    * any domain error type. JDBC failures are not converted into Left.
    *
    * @tparam E
    *   domain error type
    * @tparam A
    *   success value type
    * @param run
    *   transaction body
    * @return
    *   original Either after commit or rollback
    */
  def transactionEither[E, A](run: SqlSession => Either[E, A]): Either[E, A] =
    runTransaction(run)(_.isRight)
