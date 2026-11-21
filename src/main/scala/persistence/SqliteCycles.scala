package rizoma.persistence

import io.circe.syntax.*
import rizoma.domain.ActionError
import rizoma.domain.crop.cycle
import rizoma.prelude.Id
import rizoma.prelude.jdbc.Database

import java.time.Instant
import scala.util.{Failure, Success, Try}

final class SqliteCycles(db: Database) extends SqliteWrites(db, "cycle", "cycles") with cycle.Store:
  def initialize(): Unit = db.transaction(session => initialize(session))
  private[persistence] def initialize(session: rizoma.prelude.jdbc.SqlSession): Unit = schema(
    session,
    """CREATE TABLE IF NOT EXISTS cycles (
    id TEXT PRIMARY KEY, plot_id TEXT NOT NULL REFERENCES plots(id),
    species_id TEXT NOT NULL REFERENCES species(id), occupied_land TEXT NOT NULL,
    started_at TEXT, ended_at TEXT
  )"""
  )
  private def transact[A](run: rizoma.prelude.jdbc.SqlSession => Either[ActionError, A]): Try[A] =
    Try(db.transactionEither(run)).flatMap(_.fold(Failure(_), Success(_)))

  def insert(id: Id, input: cycle.Registration): Try[Unit] = transact { session =>
    val locked = session.execute(
      "UPDATE plots SET id = id WHERE id = ? AND deleted = 0",
      Seq(input.plotId.toString)
    )
    for
      _ <- Either.cond(locked == 1, (), ActionError.InvalidReference("cycle"))
      boundary <- SqliteLifecycle.plotBoundary(session, input.plotId.toString)
      _ <- SqliteLifecycle.fits(boundary, input.occupiedLand)
      count = session.execute(
        "INSERT INTO cycles (id, plot_id, species_id, occupied_land, started_at, ended_at) SELECT ?, ?, ?, ?, ?, ? WHERE EXISTS (SELECT 1 FROM species WHERE id = ? AND deleted = 0)",
        Seq(
          id.value,
          input.plotId.toString,
          input.speciesId.toString,
          input.occupiedLand.asJson.noSpaces,
          input.period.startedAt.map(_.toString).orNull,
          input.period.endedAt.map(_.toString).orNull,
          input.speciesId.toString
        )
      )
      _ <- Either.cond(count == 1, (), ActionError.InvalidReference("cycle"))
    yield ()
  }

  def update(input: cycle.Patch): Try[Unit] = transact { session =>
    val locked = session.execute(
      "UPDATE plots SET id = id WHERE id = ? AND deleted = 0",
      Seq(input.plotId.toString)
    )
    for
      _ <- Either.cond(locked == 1, (), ActionError.InvalidReference("cycle"))
      boundary <- SqliteLifecycle.plotBoundary(session, input.plotId.toString)
      _ <- Either.cond(
        input.occupiedLand.isValid,
        (),
        ActionError.InvalidState("Invalid occupied land topology")
      )
      _ <-
        if input.period.endedAt.nonEmpty && input.expected.period.endedAt.nonEmpty then Right(())
        else SqliteLifecycle.fits(boundary, input.occupiedLand)
      _ <- SqliteLifecycle.harvestsWithin(
        session,
        input.cycleId.toString,
        input.period.startedAt,
        input.period.endedAt
      )
      count = session.execute(
        "UPDATE cycles SET species_id = ?, occupied_land = ?, started_at = ?, ended_at = ? WHERE id = ? AND plot_id = ? AND deleted = 0 AND EXISTS (SELECT 1 FROM species WHERE species.id = ? AND species.deleted = 0) AND species_id = ? AND occupied_land = ? AND started_at IS ? AND ended_at IS ?",
        Seq(
          input.speciesId.toString,
          input.occupiedLand.asJson.noSpaces,
          input.period.startedAt.map(_.toString).orNull,
          input.period.endedAt.map(_.toString).orNull,
          input.cycleId.toString,
          input.plotId.toString,
          input.speciesId.toString,
          input.expected.speciesId.toString,
          input.expected.occupiedLand.asJson.noSpaces,
          input.expected.period.startedAt.map(_.toString).orNull,
          input.expected.period.endedAt.map(_.toString).orNull
        )
      )
      _ <-
        if count == 1 then Right(())
        else
          val exists = session
            .queryOne(
              "SELECT 1 FROM cycles WHERE id = ? AND deleted = 0",
              Seq(input.cycleId.toString)
            )(_ => true)
            .getOrElse(false)
          val speciesExists = session
            .queryOne(
              "SELECT 1 FROM species WHERE id = ? AND deleted = 0",
              Seq(input.speciesId.toString)
            )(_ => true)
            .getOrElse(false)
          Left(if !exists then ActionError.NotFound("cycle", input.cycleId.toString)
          else if !speciesExists then ActionError.InvalidReference("cycle")
          else ActionError.Conflict("cycle", input.cycleId.toString))
    yield ()
  }
  def delete(input: cycle.Delete): Try[Unit] =
    writeExisting(
      "UPDATE cycles SET deleted = 1 WHERE id = ? AND deleted = 0 AND NOT EXISTS (SELECT 1 FROM activities WHERE activities.cycle_id = cycles.id AND activities.deleted = 0) AND NOT EXISTS (SELECT 1 FROM harvests WHERE harvests.cycle_id = cycles.id AND harvests.deleted = 0) AND NOT EXISTS (SELECT 1 FROM tasks WHERE tasks.cycle_id = cycles.id AND tasks.deleted = 0 AND tasks.status IN ('Planned','InProgress'))",
      Seq(input.cycleId.toString),
      input.cycleId.toString,
      _ => ActionError.HasDependents("cycle", input.cycleId.toString)
    )

  def finish(input: cycle.Finish): Try[Unit] =
    Try(db.transactionEither { session =>
      val _ = session.execute(
        "UPDATE cycles SET id = id WHERE id = ? AND deleted = 0",
        Seq(input.cycleId.toString)
      )
      val current = session.queryOne(
        "SELECT started_at, ended_at FROM cycles WHERE id = ? AND deleted = 0",
        Seq(input.cycleId.toString)
      ) { row =>
        (Option(row.getString(1)), Option(row.getString(2)))
      }
      current match
        case None               => Left(ActionError.NotFound("cycle", input.cycleId.toString))
        case Some((_, Some(_))) => Left(ActionError.InvalidState("Cycle is already finished"))
        case Some((None, None)) => Left(ActionError.InvalidState("Cycle has not started"))
        case Some((Some(start), None)) =>
          Try(Instant.parse(start)).toEither.flatMap { startedAt =>
            if input.finishedAt.isBefore(startedAt) then
              Left(ActionError.InvalidState("Finish precedes planting"))
            else
              SqliteLifecycle
                .harvestsWithin(
                  session,
                  input.cycleId.toString,
                  Some(startedAt),
                  Some(input.finishedAt)
                )
                .flatMap { _ =>
                  val count = session.execute(
                    "UPDATE cycles SET ended_at = ? WHERE id = ? AND ended_at IS NULL AND deleted = 0",
                    Seq(input.finishedAt.toString, input.cycleId.toString)
                  )
                  Either.cond(count == 1, (), ActionError.Conflict("cycle", input.cycleId.toString))
                }
          }
    }).flatMap(_.fold(Failure(_), Success(_)))
