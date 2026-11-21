package rizoma.prelude.jdbc

import java.sql.SQLException

@SuppressWarnings(Array("scalafix:DisableSyntax"))
class DatabaseTransactionSuite extends JdbcTestDriver:
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
