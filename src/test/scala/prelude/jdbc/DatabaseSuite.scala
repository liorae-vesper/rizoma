package rizoma.prelude.jdbc

import java.sql.{SQLException, Types}

@SuppressWarnings(Array("scalafix:DisableSyntax"))
class DatabaseSuite extends JdbcTestDriver:
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
