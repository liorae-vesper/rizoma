package rizoma.persistence

import io.circe.syntax.*
import rizoma.domain.{ActionError, farm}
import rizoma.prelude.Id
import rizoma.prelude.jdbc.Database

import scala.util.{Failure, Success, Try}

final class SqliteFarms(db: Database) extends farm.Store:
  def initialize(): Unit = db.transaction(session => initialize(session))
  private[persistence] def initialize(session: rizoma.prelude.jdbc.SqlSession): Unit =
    val _ = session.execute("""CREATE TABLE IF NOT EXISTS farms (
      id TEXT PRIMARY KEY, name TEXT NOT NULL, location TEXT NOT NULL,
      total_area REAL NOT NULL, unit_of_area TEXT NOT NULL
    )""")

  def insert(id: Id, input: farm.Registration): Try[Unit] = Try {
    val _ = db.execute(
      "INSERT INTO farms (id, name, location, total_area, unit_of_area) VALUES (?, ?, ?, ?, ?)",
      Seq(
        id.value,
        input.name.value,
        input.location.asJson.noSpaces,
        input.area.totalArea,
        input.area.unitOfArea.asString
      )
    )
  }

  def update(input: farm.Patch): Try[Unit] =
    Try(
      db.execute(
        "UPDATE farms SET name = ?, location = ?, total_area = ?, unit_of_area = ? WHERE id = ?",
        Seq(
          input.name.value,
          input.location.asJson.noSpaces,
          input.area.totalArea,
          input.area.unitOfArea.asString,
          input.id.value
        )
      )
    ).flatMap { count =>
      if count == 1 then Success(())
      else Failure(ActionError.NotFound("farm", input.id.value))
    }
