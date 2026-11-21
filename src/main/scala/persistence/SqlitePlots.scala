package rizoma.persistence

import io.circe.Json
import io.circe.syntax.*
import rizoma.domain.{ActionError, plot}
import rizoma.prelude.Id
import rizoma.prelude.jdbc.Database

import scala.util.{Failure, Success, Try}

final class SqlitePlots(db: Database) extends SqliteWrites(db, "plot", "plots") with plot.Store:
  def initialize(): Unit = db.transaction(session => initialize(session))
  private[persistence] def initialize(session: rizoma.prelude.jdbc.SqlSession): Unit = schema(
    session,
    """CREATE TABLE IF NOT EXISTS plots (
    id TEXT PRIMARY KEY, farm_id TEXT NOT NULL REFERENCES farms(id), name TEXT NOT NULL,
    geometry TEXT NOT NULL, area REAL NOT NULL, unit_of_area TEXT NOT NULL, status TEXT NOT NULL
  )"""
  )
  def insert(id: Id, input: plot.Registration): Try[Unit] = if !input.geometry.isValid then
    Failure(ActionError.InvalidState("Invalid plot boundary topology"))
  else
    write(
      "INSERT INTO plots (id, farm_id, name, geometry, area, unit_of_area, status) SELECT ?, ?, ?, ?, ?, ?, ? WHERE EXISTS (SELECT 1 FROM farms WHERE id = ?)",
      Seq(
        id.value,
        input.farmId.value,
        input.name.toString,
        input.geometry.asJson.noSpaces,
        input.area.totalArea,
        input.area.unitOfArea.asString,
        input.status.toString,
        input.farmId.value
      )
    )
  private def transact[A](run: rizoma.prelude.jdbc.SqlSession => Either[ActionError, A]): Try[A] =
    Try(db.transactionEither(run)).flatMap(_.fold(Failure(_), Success(_)))

  def update(input: plot.Patch): Try[Unit] = transact { session =>
    val locked = session.execute(
      "UPDATE plots SET id = id WHERE id = ? AND deleted = 0",
      Seq(input.plotId.toString)
    )
    for
      _ <- Either.cond(locked == 1, (), ActionError.NotFound("plot", input.plotId.toString))
      _ <- SqliteLifecycle.checkBoundary(session, input.plotId.toString, input.newGeometry)
      count = session.execute(
        "UPDATE plots SET name = ?, geometry = ?, area = ?, unit_of_area = ?, status = ? WHERE id = ? AND farm_id = ? AND deleted = 0 AND name = ? AND geometry = ? AND area = ? AND unit_of_area = ? AND status = ?",
        Seq(
          input.newName.toString,
          input.newGeometry.asJson.noSpaces,
          input.newArea.totalArea,
          input.newArea.unitOfArea.asString,
          input.status.toString,
          input.plotId.toString,
          input.currentFarm.value,
          input.expected.name.toString,
          input.expected.geometry.asJson.noSpaces,
          input.expected.area.totalArea,
          input.expected.area.unitOfArea.asString,
          input.expected.status.toString
        )
      )
      _ <- Either.cond(count == 1, (), ActionError.Conflict("plot", input.plotId.toString))
    yield ()
  }
  def delete(input: plot.Delete): Try[Unit] =
    writeExisting(
      "UPDATE plots SET deleted = 1 WHERE id = ? AND deleted = 0 AND NOT EXISTS (SELECT 1 FROM cycles WHERE cycles.plot_id = plots.id AND cycles.deleted = 0) AND NOT EXISTS (SELECT 1 FROM tasks WHERE tasks.plot_id = plots.id AND tasks.deleted = 0 AND tasks.status IN ('Planned','InProgress'))",
      Seq(input.plotId.toString),
      input.plotId.toString,
      _ => ActionError.HasDependents("plot", input.plotId.toString)
    )

  def mark(input: plot.Mark): Try[Unit] = transact { session =>
    val locked = session.execute(
      "UPDATE plots SET id = id WHERE id = ? AND deleted = 0",
      Seq(input.plotId.toString)
    )
    for
      _ <- Either.cond(locked == 1, (), ActionError.NotFound("plot", input.plotId.toString))
      _ <- SqliteLifecycle.checkBoundary(session, input.plotId.toString, input.geometry)
    yield
      val _ = session.execute(
        "UPDATE plots SET geometry = ? WHERE id = ?",
        Seq(input.geometry.asJson.noSpaces, input.plotId.toString)
      )
  }

  def demark(input: plot.Demark): Try[Unit] =
    val emptyGeometry =
      Json.obj("type" -> Json.fromString("MultiPolygon"), "coordinates" -> Json.arr())
    writeForId(
      "UPDATE plots SET geometry = ? WHERE id = ? AND deleted = 0",
      Seq(emptyGeometry.noSpaces, input.plotId.toString),
      input.plotId.toString
    )

  def remeasure(input: plot.Remeasure): Try[Unit] =
    writeForId(
      "UPDATE plots SET area = ?, unit_of_area = ? WHERE id = ? AND deleted = 0",
      Seq(input.area.totalArea, input.area.unitOfArea.asString, input.plotId.toString),
      input.plotId.toString
    )

  def activate(input: plot.Activate): Try[Unit] =
    writeForId(
      "UPDATE plots SET status = ? WHERE id = ? AND deleted = 0",
      Seq(plot.Status.Active.toString, input.plotId.toString),
      input.plotId.toString
    )

  def resting(input: plot.Resting): Try[Unit] =
    writeForId(
      "UPDATE plots SET status = ? WHERE id = ? AND deleted = 0",
      Seq(plot.Status.Resting.toString, input.plotId.toString),
      input.plotId.toString
    )
