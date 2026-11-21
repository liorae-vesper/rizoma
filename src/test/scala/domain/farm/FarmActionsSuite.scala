package rizoma.domain.farm

import org.sqlite.SQLiteDataSource
import rizoma.domain.{Area, Position, UnitOfArea}
import rizoma.persistence.SqliteFarms
import rizoma.prelude.Prelude.IdGenerator
import rizoma.prelude.jdbc.Database
import rizoma.prelude.{Id, InvalidGeneration}

import scala.util.{Failure, Try}

class FarmActionsSuite extends munit.FunSuite:
  test("registration propagates ID generation failures without persisting"):
    val cause = new IllegalStateException("random source unavailable")
    val failure = InvalidGeneration(Some(cause))
    val ids = new IdGenerator:
      def next(): Either[InvalidGeneration, Id] = Left(failure)
    val store = new Store:
      def insert(id: Id, input: Registration): Try[Unit] = fail("persistence must not run")
      def update(input: Patch): Try[Unit] = fail("persistence must not run")
    val location = Position.from(0, 0).getOrElse(fail("invalid position"))
    val area = Area.from(1).fold(e => fail(e.toString), identity)
    val input = Registration.from("Farm", location, area).fold(e => fail(e.toString), identity)
    assertEquals(
      register(input)(using store, ids),
      Left(rizoma.domain.ActionError.Generation(failure))
    )
    assertEquals(failure.getCause, cause)

  test("registration preserves unexpected storage failures"):
    val error = new IllegalStateException("database unavailable")
    val store = new Store:
      def insert(id: Id, input: Registration): Try[Unit] = Failure(error)
      def update(input: Patch): Try[Unit] = Failure(error)
    val id = Id.generate().fold(failure => fail(failure.toString), identity)
    val ids = new IdGenerator:
      def next(): Either[InvalidGeneration, Id] = Right(id)
    val location = Position.from(0, 0).getOrElse(fail("invalid position"))
    val area = Area.from(1).fold(e => fail(e.toString), identity)
    val input = Registration.from("Farm", location, area).fold(e => fail(e.toString), identity)
    assertEquals(
      register(input)(using store, ids),
      Left(rizoma.domain.ActionError.PersistenceFailure(error))
    )

  test("farm registration and editing persist validated values and retain identity"):
    val ds = new SQLiteDataSource()
    ds.setUrl(s"jdbc:sqlite:file:${java.util.UUID.randomUUID()}?mode=memory&cache=shared")
    val anchor = ds.getConnection()
    try
      val db = new Database(ds)
      val store = new SqliteFarms(db)
      store.initialize()
      val id = Id.generate().fold(error => fail(error.toString), identity)
      given IdGenerator with
        def next(): Either[rizoma.prelude.InvalidGeneration, Id] = Right(id)
      given Store = store
      val location = Position.from(12, 34, Some(56)).getOrElse(fail("invalid position"))
      val area = Area.from(7, UnitOfArea.Hectares).fold(e => fail(e.toString), identity)
      val registration =
        Registration.from(" Farm ", location, area).fold(e => fail(e.toString), identity)
      assertEquals(register(registration), Right(id))
      val patch = View(id, registration.name, location, area)
        .patch(" New name ", location, area)
        .fold(e => fail(e.toString), identity)
      assertEquals(edit(patch), Right(()))
      val rows = db.query("SELECT id, name, location, total_area, unit_of_area FROM farms")(r =>
        (r.getString(1), r.getString(2), r.getString(3), r.getDouble(4), r.getString(5))
      )
      assertEquals(rows, List((id.value, "New name", "[12.0,34.0,56.0]", 7.0, "hectares")))
      assert(
        edit(patch.copy(id = Id.generate().fold(error => fail(error.toString), identity))).isLeft
      )
    finally anchor.close()
