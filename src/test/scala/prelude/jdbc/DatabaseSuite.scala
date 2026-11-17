package rizoma.prelude.jdbc

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.sql.{Connection, PreparedStatement, ResultSet, SQLException, Types}
import javax.sql.DataSource
import scala.collection.mutable.ArrayBuffer

// JDBC test doubles exercise resource ownership and driver failures without requiring a database.
@SuppressWarnings(Array("scalafix:DisableSyntax"))
class DatabaseSuite extends munit.FunSuite:
  private final class Driver(
      rows: List[Int] = List(10, 20),
      initialAutoCommit: Boolean = true,
      failures: Map[String, SQLException] = Map.empty
  ):
    val events: ArrayBuffer[String] = ArrayBuffer.empty
    val bindings: ArrayBuffer[(String, List[AnyRef])] = ArrayBuffer.empty
    private var remaining = rows
    private var current = 0

    private def record(event: String): Unit =
      val _ = events += event
      failures.get(event).foreach(error => throw error)

    private def proxy[A](api: Class[A])(run: (String, List[AnyRef]) => AnyRef): A =
      api.cast(
        Proxy.newProxyInstance(
          api.getClassLoader,
          Array(api),
          new InvocationHandler:
            override def invoke(instance: Any, method: Method, args: Array[AnyRef]): AnyRef =
              run(method.getName, Option(args).fold(List.empty[AnyRef])(_.toList))
        )
      )

    private val resultSet = proxy(classOf[ResultSet]) { (method, _) =>
      record(s"resultSet.$method")
      method match
        case "next" =>
          remaining match
            case value :: rest =>
              current = value
              remaining = rest
              java.lang.Boolean.TRUE
            case Nil => java.lang.Boolean.FALSE
        case "getInt" => java.lang.Integer.valueOf(current)
        case "close"  => null
        case _        => throw new UnsupportedOperationException(method)
    }

    private val statement = proxy(classOf[PreparedStatement]) { (method, args) =>
      record(s"statement.$method")
      method match
        case "executeQuery"                     => resultSet
        case "executeUpdate"                    => java.lang.Integer.valueOf(3)
        case "close"                            => null
        case setter if setter.startsWith("set") =>
          val _ = bindings += ((setter, args))
          null
        case _ => throw new UnsupportedOperationException(method)
    }

    private val connection = proxy(classOf[Connection]) { (method, args) =>
      val event = if method == "setAutoCommit" then s"connection.$method(${args.mkString})"
      else s"connection.$method"
      record(event)
      method match
        case "prepareStatement" => statement
        case "getAutoCommit"    => java.lang.Boolean.valueOf(initialAutoCommit)
        case "setAutoCommit" | "commit" | "rollback" | "close" => null
        case _ => throw new UnsupportedOperationException(method)
    }

    private val dataSource = proxy(classOf[DataSource]) { (method, _) =>
      record(s"dataSource.$method")
      method match
        case "getConnection" => connection
        case _               => throw new UnsupportedOperationException(method)
    }

    val database = new Database(dataSource)

    def count(event: String): Int = events.count(_ == event)

  test("query binds every supported parameter in positional order"):
    val driver = new Driver()
    val bytes = Array[Byte](1, 2)
    val decimal = BigDecimal("123.45")
    val params: Seq[SqlParam] = Seq("value", 1, 2L, 3.0, 4.0f, true, bytes, decimal, null)

    assertEquals(driver.database.query("select ?", params)(_.getInt(1)), List(10, 20))
    assertEquals(
      driver.bindings.toList,
      List(
        ("setString", List(Int.box(1), "value")),
        ("setInt", List(Int.box(2), Int.box(1))),
        ("setLong", List(Int.box(3), Long.box(2L))),
        ("setDouble", List(Int.box(4), Double.box(3.0))),
        ("setFloat", List(Int.box(5), Float.box(4.0f))),
        ("setBoolean", List(Int.box(6), Boolean.box(true))),
        ("setBytes", List(Int.box(7), bytes)),
        ("setBigDecimal", List(Int.box(8), decimal.bigDecimal)),
        ("setNull", List(Int.box(9), Int.box(Types.NULL)))
      )
    )

  test("query closes the result set, statement, and connection in order"):
    val driver = new Driver()
    assertEquals(driver.database.query("select value")(_.getInt(1)), List(10, 20))
    assertEquals(
      driver.events.takeRight(3).toList,
      List("resultSet.close", "statement.close", "connection.close")
    )

  test("query returns an empty list when no rows exist"):
    val driver = new Driver(rows = Nil)
    assertEquals(driver.database.query("select value")(_.getInt(1)), Nil)

  test("queryOne decodes only the first row"):
    val driver = new Driver()
    assertEquals(driver.database.queryOne("select value")(_.getInt(1)), Some(10))
    assertEquals(driver.count("resultSet.next"), 1)
    assertEquals(driver.count("resultSet.getInt"), 1)
    assertEquals(driver.count("resultSet.close"), 1)
    assertEquals(driver.count("statement.close"), 1)
    assertEquals(driver.count("connection.close"), 1)

  test("queryOne returns None when no rows exist"):
    val driver = new Driver(rows = Nil)
    assertEquals(driver.database.queryOne("select value")(_.getInt(1)), None)

  test("execute returns the affected row count and each operation obtains a connection"):
    val driver = new Driver()
    assertEquals(driver.database.execute("update values set value = ?", Seq(1)), 3)
    assertEquals(driver.database.execute("delete from values"), 3)
    assertEquals(driver.count("dataSource.getConnection"), 2)
    assertEquals(driver.count("statement.close"), 2)
    assertEquals(driver.count("connection.close"), 2)

  test("decoder failures propagate after all query resources are closed"):
    val driver = new Driver()
    val failure = new IllegalStateException("decode failed")
    val thrown = intercept[IllegalStateException]:
      driver.database.query[Int]("select value")(_ => throw failure)
    assert(thrown eq failure)
    assertEquals(
      driver.events.takeRight(3).toList,
      List("resultSet.close", "statement.close", "connection.close")
    )

  List("statement.setInt", "statement.executeQuery", "resultSet.next").foreach { event =>
    test(s"query propagates $event failures and closes acquired resources"):
      val failure = new SQLException(event)
      val driver = new Driver(failures = Map(event -> failure))
      val thrown = intercept[SQLException]:
        driver.database.query("select ?", Seq(1))(_.getInt(1))
      assert(thrown eq failure)
      assertEquals(driver.count("statement.close"), 1)
      assertEquals(driver.count("connection.close"), 1)
  }

  test("transactions share one connection and commit before restoring autoCommit and closing"):
    val driver = new Driver()
    val result = driver.database.transaction { session =>
      assertEquals(session.execute("update values", Seq(1)), 3)
      assertEquals(session.query("select value", Seq(2))(_.getInt(1)), List(10, 20))
      assertEquals(session.queryOne("select value", Seq(3))(_.getInt(1)), None)
      assertEquals(driver.count("connection.close"), 0)
      "done"
    }
    assertEquals(result, "done")
    assertEquals(driver.count("dataSource.getConnection"), 1)
    assertEquals(driver.count("connection.rollback"), 0)
    assertEquals(
      driver.events.takeRight(3).toList,
      List("connection.commit", "connection.setAutoCommit(true)", "connection.close")
    )

  test("ordinary transaction commits even when its result is Left"):
    val driver = new Driver()
    assertEquals(driver.database.transaction(_ => Left("domain error")), Left("domain error"))
    assertEquals(driver.count("connection.commit"), 1)
    assertEquals(driver.count("connection.rollback"), 0)

  test("transactionEither commits Right and returns it unchanged"):
    val driver = new Driver()
    assertEquals(driver.database.transactionEither(_ => Right(42)), Right(42))
    assertEquals(driver.count("connection.commit"), 1)
    assertEquals(driver.count("connection.rollback"), 0)

  test("transactionEither rolls back Left and returns it unchanged"):
    val driver = new Driver()
    assertEquals(driver.database.transactionEither(_ => Left("domain error")), Left("domain error"))
    assertEquals(driver.count("connection.commit"), 0)
    assertEquals(driver.count("connection.rollback"), 1)

  test("transaction callback failures roll back and preserve the original exception"):
    val driver = new Driver()
    val failure = new IllegalStateException("body failed")
    val thrown = intercept[IllegalStateException]:
      driver.database.transaction(_ => throw failure)
    assert(thrown eq failure)
    assertEquals(driver.count("connection.commit"), 0)
    assertEquals(driver.count("connection.rollback"), 1)
    assertEquals(driver.count("connection.setAutoCommit(true)"), 1)
    assertEquals(driver.count("connection.close"), 1)

  test("JDBC failures in transactionEither propagate rather than becoming Left"):
    val failure = new SQLException("execute failed")
    val driver = new Driver(failures = Map("statement.executeUpdate" -> failure))
    val thrown = intercept[SQLException]:
      driver.database.transactionEither(session => Right(session.execute("update values")))
    assert(thrown eq failure)
    assertEquals(driver.count("connection.rollback"), 1)

  test("commit failures trigger one rollback attempt"):
    val failure = new SQLException("commit failed")
    val driver = new Driver(failures = Map("connection.commit" -> failure))
    val thrown = intercept[SQLException]:
      driver.database.transaction(_ => 42)
    assert(thrown eq failure)
    assertEquals(driver.count("connection.rollback"), 1)
    assertEquals(driver.count("connection.close"), 1)

  test("rollback failures on Left are propagated without retrying rollback"):
    val failure = new SQLException("rollback failed")
    val driver = new Driver(failures = Map("connection.rollback" -> failure))
    val thrown = intercept[SQLException]:
      driver.database.transactionEither(_ => Left("domain error"))
    assert(thrown eq failure)
    assertEquals(driver.count("connection.rollback"), 1)
    assertEquals(driver.count("connection.setAutoCommit(true)"), 1)
    assertEquals(driver.count("connection.close"), 1)

  test("rollback, restoration, and close failures are suppressed on the original failure"):
    val failure = new IllegalStateException("body failed")
    val rollback = new SQLException("rollback failed")
    val restore = new SQLException("restore failed")
    val close = new SQLException("close failed")
    val driver = new Driver(failures =
      Map(
        "connection.rollback" -> rollback,
        "connection.setAutoCommit(true)" -> restore,
        "connection.close" -> close
      )
    )
    val thrown = intercept[IllegalStateException]:
      driver.database.transaction(_ => throw failure)
    assert(thrown eq failure)
    assertEquals(thrown.getSuppressed.toList, List(rollback, restore, close))
    assertEquals(driver.count("connection.rollback"), 1)

  test("a repeated exception instance does not trigger self-suppression"):
    val failure = new SQLException("shared failure")
    val driver = new Driver(failures =
      Map(
        "connection.rollback" -> failure,
        "connection.setAutoCommit(true)" -> failure
      )
    )
    val thrown = intercept[SQLException]:
      driver.database.transaction(_ => throw failure)
    assert(thrown eq failure)
    assertEquals(thrown.getSuppressed.toList, Nil)

  test("restoration failures after commit propagate and still close the connection"):
    val failure = new SQLException("restore failed")
    val driver = new Driver(failures = Map("connection.setAutoCommit(true)" -> failure))
    val thrown = intercept[SQLException]:
      driver.database.transaction(_ => 42)
    assert(thrown eq failure)
    assertEquals(driver.count("connection.commit"), 1)
    assertEquals(driver.count("connection.rollback"), 0)
    assertEquals(driver.count("connection.close"), 1)

  test("transactions restore an originally disabled autoCommit setting"):
    val driver = new Driver(initialAutoCommit = false)
    assertEquals(driver.database.transaction(_ => 42), 42)
    assertEquals(driver.count("connection.setAutoCommit(false)"), 2)
    assertEquals(driver.count("connection.setAutoCommit(true)"), 0)

  List("connection.getAutoCommit", "connection.setAutoCommit(false)").foreach { event =>
    test(s"transaction setup failure at $event closes the connection without running the body"):
      val failure = new SQLException(event)
      val driver = new Driver(failures = Map(event -> failure))
      val thrown = intercept[SQLException]:
        driver.database.transaction(_ => fail("transaction body must not run"))
      assert(thrown eq failure)
      assertEquals(driver.count("connection.rollback"), 0)
      assertEquals(driver.count("connection.close"), 1)
  }
