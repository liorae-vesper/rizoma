package rizoma.persistence

import io.circe.parser
import io.circe.syntax.*
import rizoma.domain.ActionError
import rizoma.domain.crop.{activity, cycle}
import rizoma.prelude.Id
import rizoma.prelude.jdbc.Database

import java.sql.ResultSet
import java.time.Instant
import scala.util.{Failure, Success, Try}

final class SqliteActivities(db: Database)
    extends SqliteWrites(db, "activity", "activities")
    with activity.Store:
  import JournalCodec.given

  def initialize(): Unit = db.transaction(session => initialize(session))
  private[persistence] def initialize(session: rizoma.prelude.jdbc.SqlSession): Unit =
    schema(
      session,
      """CREATE TABLE IF NOT EXISTS activities (
      id TEXT PRIMARY KEY, cycle_id TEXT NOT NULL REFERENCES cycles(id),
      event TEXT NOT NULL, occurred_at TEXT NOT NULL
    )"""
    )
    val columns = session.query("PRAGMA table_info(activities)")(_.getString("name"))
    if !columns.contains("journal") then
      val _ = session.execute("ALTER TABLE activities ADD COLUMN journal TEXT")

  private def transact[A](run: rizoma.prelude.jdbc.SqlSession => Either[ActionError, A]): Try[A] =
    Try(db.transactionEither(run)).flatMap(_.fold(Failure(_), Success(_)))

  def insert(id: Id, input: activity.Registration): Try[Unit] = transact { session =>
    insertIn(session, id, input)
  }

  private[persistence] def insertIn(
      session: rizoma.prelude.jdbc.SqlSession,
      id: Id,
      input: activity.Registration
  ): Either[ActionError, Unit] =
    val count = session.execute(
      "INSERT INTO activities (id, cycle_id, event, occurred_at, journal) SELECT ?, ?, ?, ?, ? WHERE EXISTS (SELECT 1 FROM cycles WHERE id = ? AND deleted = 0)",
      Seq(
        id.value,
        input.cycleId.toString,
        input.event.toString,
        input.occurredAt.toString,
        input.journal.asJson.noSpaces,
        input.cycleId.toString
      )
    )
    if count != 1 then Left(ActionError.InvalidReference("cycle"))
    else
      JournalStock(session, id.value, input.cycleId.toString, input.journal, true, input.occurredAt)

  def update(input: activity.Patch): Try[Unit] = transact { session =>
    val current = session.queryOne(
      "SELECT id, cycle_id, event, occurred_at, journal FROM activities WHERE id = ? AND deleted = 0",
      Seq(input.activityId.toString)
    )(decode)
    current.toRight(ActionError.NotFound("activity", input.activityId.toString)).flatMap {
      decoded =>
        decoded.left
          .map(error =>
            ActionError
              .PersistenceFailure(new IllegalStateException(s"Invalid stored activity: $error"))
          )
          .flatMap { view =>
            if view != input.expected then
              Left(ActionError.Conflict("activity", input.activityId.toString))
            else
              val count = session.execute(
                "UPDATE activities SET event = ?, occurred_at = ?, journal = ? WHERE id = ? AND cycle_id = ? AND deleted = 0",
                Seq(
                  input.event.toString,
                  input.occurredAt.toString,
                  input.journal.asJson.noSpaces,
                  input.activityId.toString,
                  input.cycleId.toString
                )
              )
              if count != 1 then Left(ActionError.Conflict("activity", input.activityId.toString))
              else if view.journal.inputs == input.journal.inputs && view.occurredAt == input.occurredAt
              then Right(())
              else
                for
                  _ <- JournalStock(
                    session,
                    input.activityId.toString,
                    input.cycleId.toString,
                    view.journal,
                    false,
                    input.occurredAt
                  )
                  _ <- JournalStock(
                    session,
                    input.activityId.toString,
                    input.cycleId.toString,
                    input.journal,
                    true,
                    input.occurredAt
                  )
                yield ()
          }
    }
  }

  def delete(input: activity.Delete): Try[Unit] = transact { session =>
    val current = session.queryOne(
      "SELECT id, cycle_id, event, occurred_at, journal FROM activities WHERE id = ? AND deleted = 0",
      Seq(input.activityId.toString)
    )(decode)
    current.toRight(ActionError.NotFound("activity", input.activityId.toString)).flatMap {
      decoded =>
        decoded.left
          .map(error =>
            ActionError
              .PersistenceFailure(new IllegalStateException(s"Invalid stored activity: $error"))
          )
          .flatMap { view =>
            val count = session.execute(
              "UPDATE activities SET deleted = 1 WHERE id = ? AND deleted = 0 AND NOT EXISTS (SELECT 1 FROM tasks WHERE tasks.activity_id = activities.id AND tasks.deleted = 0)",
              Seq(input.activityId.toString)
            )
            if count != 1 then Left(ActionError.Conflict("activity", input.activityId.toString))
            else
              JournalStock(
                session,
                input.activityId.toString,
                view.cycleId.toString,
                view.journal,
                false,
                Instant.now()
              )
          }
    }
  }

  private[persistence] def decode(row: ResultSet): Either[String, activity.View] =
    for
      id <- Id.parse(row.getString("id")).left.map(_.toString)
      cycleId <- Id.parse(row.getString("cycle_id")).left.map(_.toString)
      event <- activity.Event.from(row.getString("event")).left.map(_.toString)
      occurredAt <- Try(Instant.parse(row.getString("occurred_at"))).toEither.left.map(_.toString)
      journal <- Option(row.getString("journal")) match
        case None       => Right(activity.Journal.empty)
        case Some(json) => parser.decode[activity.Journal](json).left.map(_.toString)
    yield activity.View(
      activity.ActivityId.from(id),
      cycle.CycleId.from(cycleId),
      event,
      occurredAt,
      journal
    )

  private def validated(value: Either[String, activity.View]): Try[activity.View] =
    value.fold(
      error => Failure(new IllegalStateException(s"Invalid stored activity: $error")),
      Success(_)
    )

  def get(id: activity.ActivityId): Try[Option[activity.View]] =
    Try(
      db.queryOne(
        "SELECT id, cycle_id, event, occurred_at, journal FROM activities WHERE id = ? AND deleted = 0",
        Seq(id.toString)
      )(decode)
    )
      .flatMap {
        case Some(value) => validated(value).map(Some(_))
        case None        => Success(None)
      }

  def listByCycle(id: cycle.CycleId): Try[List[activity.View]] =
    Try(
      db.query(
        "SELECT id, cycle_id, event, occurred_at, journal FROM activities WHERE cycle_id = ? AND deleted = 0",
        Seq(id.toString)
      )(decode)
    )
      .flatMap(
        _.foldLeft[Try[List[activity.View]]](Success(Nil)) { (result, value) =>
          for rows <- result; row <- validated(value) yield row :: rows
        }.map(
          _.sortBy(row =>
            (row.occurredAt.getEpochSecond, row.occurredAt.getNano, row.activityId.toString)
          )
        )
      )
