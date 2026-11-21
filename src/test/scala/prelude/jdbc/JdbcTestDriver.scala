package rizoma.prelude.jdbc

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.sql.{Connection, PreparedStatement, ResultSet, SQLException}
import javax.sql.DataSource
import scala.collection.mutable.ArrayBuffer

// JDBC doubles exercise resource ownership and failures without a database.
@SuppressWarnings(Array("scalafix:DisableSyntax"))
private[jdbc] trait JdbcTestDriver extends munit.FunSuite:
  protected final class Driver(
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
