package rizoma.persistence

import rizoma.domain.crop.{activity, cycle}
import rizoma.domain.{ActionError, plot, task}
import rizoma.prelude.Id
import rizoma.prelude.jdbc.{Database, SqlSession}

import java.time.Instant
import scala.util.{Failure, Success, Try}

object SqliteTasks:
  private[persistence] def initialize(session: SqlSession): Unit =
    val _ = session.execute("""CREATE TABLE IF NOT EXISTS tasks (
      id TEXT PRIMARY KEY, farm_id TEXT NOT NULL REFERENCES farms(id), title TEXT NOT NULL,
      due_at TEXT, assignee TEXT REFERENCES users(id), plot_id TEXT REFERENCES plots(id),
      cycle_id TEXT REFERENCES cycles(id), status TEXT NOT NULL, activity_id TEXT REFERENCES activities(id),
      deleted INTEGER NOT NULL DEFAULT 0
    )""")
    val _ = session.execute("CREATE INDEX IF NOT EXISTS tasks_farm_idx ON tasks(farm_id, id)")

final class SqliteTasks(db: Database) extends task.Store:
  private val activities = new SqliteActivities(db)
  private def transact[A](run: SqlSession => Either[ActionError, A]): Try[A] =
    Try(db.transactionEither(run)).flatMap(_.fold(Failure(_), Success(_)))

  private def validate(session: SqlSession, input: task.Create): Either[ActionError, Unit] =
    val farm = session
      .queryOne("SELECT 1 FROM farms WHERE id = ?", Seq(input.farmId.value))(_ => true)
      .getOrElse(false)
    val member = input.assignee.forall(id =>
      session
        .queryOne(
          "SELECT 1 FROM farm_memberships WHERE farm_id = ? AND user_id = ?",
          Seq(input.farmId.value, id.value)
        )(_ => true)
        .getOrElse(false)
    )
    val validPlot = input.plotId.forall(id =>
      session
        .queryOne(
          "SELECT 1 FROM plots WHERE id = ? AND farm_id = ? AND deleted = 0",
          Seq(id.toString, input.farmId.value)
        )(_ => true)
        .getOrElse(false)
    )
    val validCycle = input.cycleId.forall(id =>
      session
        .queryOne(
          "SELECT p.id FROM cycles c JOIN plots p ON p.id = c.plot_id WHERE c.id = ? AND p.farm_id = ? AND c.deleted = 0 AND p.deleted = 0",
          Seq(id.toString, input.farmId.value)
        )(_.getString(1))
        .exists(p => input.plotId.forall(_.toString == p))
    )
    Either.cond(farm && member && validPlot && validCycle, (), ActionError.InvalidReference("task"))

  def insert(id: Id, input: task.Create): Try[Unit] = transact { session =>
    val _ = session.execute("UPDATE farms SET id = id WHERE id = ?", Seq(input.farmId.value))
    validate(session, input).map { _ =>
      val _ = session.execute(
        "INSERT INTO tasks (id, farm_id, title, due_at, assignee, plot_id, cycle_id, status) VALUES (?, ?, ?, ?, ?, ?, ?, 'Planned')",
        Seq(
          id.value,
          input.farmId.value,
          input.title,
          input.dueAt.map(_.toString).orNull,
          input.assignee.map(_.value).orNull,
          input.plotId.map(_.toString).orNull,
          input.cycleId.map(_.toString).orNull
        )
      )
    }
  }

  def update(id: task.TaskId, input: task.Create): Try[Unit] = transact { session =>
    val locked =
      session.execute("UPDATE tasks SET id = id WHERE id = ? AND deleted = 0", Seq(id.toString))
    for
      _ <- Either.cond(locked == 1, (), ActionError.NotFound("task", id.toString))
      _ <- validate(session, input)
      count = session.execute(
        "UPDATE tasks SET title = ?, due_at = ?, assignee = ?, plot_id = ?, cycle_id = ? WHERE id = ? AND farm_id = ? AND deleted = 0 AND status IN ('Planned','InProgress')",
        Seq(
          input.title,
          input.dueAt.map(_.toString).orNull,
          input.assignee.map(_.value).orNull,
          input.plotId.map(_.toString).orNull,
          input.cycleId.map(_.toString).orNull,
          id.toString,
          input.farmId.value
        )
      )
      _ <- Either.cond(count == 1, (), ActionError.InvalidState("Task cannot be edited"))
    yield ()
  }

  def status(id: task.TaskId, value: task.Status): Try[Unit] = transact { session =>
    val allowed =
      value == task.Status.Planned || value == task.Status.InProgress || value == task.Status.Cancelled
    val count = if allowed then
      session.execute(
        "UPDATE tasks SET status = ? WHERE id = ? AND deleted = 0 AND status IN ('Planned','InProgress')",
        Seq(value.toString, id.toString)
      )
    else 0
    Either.cond(count == 1, (), ActionError.InvalidState("Invalid task status transition"))
  }

  def complete(id: task.TaskId, record: Option[(Id, activity.Registration)]): Try[Unit] = transact {
    session =>
      val count = session.execute(
        "UPDATE tasks SET status = 'Completed' WHERE id = ? AND deleted = 0 AND status IN ('Planned','InProgress')",
        Seq(id.toString)
      )
      for
        _ <- Either.cond(count == 1, (), ActionError.InvalidState("Task cannot be completed"))
        _ <- record match
          case None                   => Right(())
          case Some((eventId, input)) =>
            val matches = session
              .queryOne("SELECT cycle_id FROM tasks WHERE id = ?", Seq(id.toString))(r =>
                Option(r.getString(1))
              )
              .flatten
              .contains(input.cycleId.toString)
            if !matches then Left(ActionError.InvalidReference("task activity cycle"))
            else
              activities.insertIn(session, eventId, input).map { _ =>
                val _ = session.execute(
                  "UPDATE tasks SET activity_id = ? WHERE id = ?",
                  Seq(eventId.value, id.toString)
                )
              }
      yield ()
  }

  def delete(id: task.TaskId): Try[Unit] = transact { session =>
    val count =
      session.execute("UPDATE tasks SET deleted = 1 WHERE id = ? AND deleted = 0", Seq(id.toString))
    Either.cond(count == 1, (), ActionError.NotFound("task", id.toString))
  }

  private def view(session: SqlSession, id: String): Either[ActionError, Option[task.View]] =
    val row = session.queryOne(
      "SELECT id, farm_id, title, due_at, assignee, plot_id, cycle_id, status, activity_id FROM tasks WHERE id = ? AND deleted = 0",
      Seq(id)
    )(r =>
      (
        r.getString(1),
        r.getString(2),
        r.getString(3),
        Option(r.getString(4)),
        Option(r.getString(5)),
        Option(r.getString(6)),
        Option(r.getString(7)),
        r.getString(8),
        Option(r.getString(9))
      )
    )
    def parse(raw: String): Either[ActionError, Id] = Id
      .parse(raw)
      .left
      .map(e => ActionError.PersistenceFailure(new IllegalStateException(e.toString)))
    def optional(raw: Option[String]): Either[ActionError, Option[Id]] = raw match
      case None        => Right(None)
      case Some(value) => parse(value).map(Some(_))
    row match
      case None                                                                    => Right(None)
      case Some((key, farm, title, due, assignee, plotId, cycleId, status, event)) =>
        for
          keyId <- parse(key)
          farmId <- parse(farm)
          dueAt <- due match
            case None      => Right(None)
            case Some(raw) =>
              Try(Instant.parse(raw)).toEither.left
                .map(ActionError.PersistenceFailure.apply)
                .map(Some(_))
          user <- optional(assignee)
          plotRef <- optional(plotId)
          cycleRef <- optional(cycleId)
          eventRef <- optional(event)
          details <- task.Create
            .from(
              farmId,
              title,
              dueAt,
              user,
              plotRef.map(plot.PlotId.from),
              cycleRef.map(cycle.CycleId.from)
            )
            .left
            .map(e => ActionError.PersistenceFailure(new IllegalStateException(e)))
          state <- task.Status.values
            .find(_.toString == status)
            .toRight(
              ActionError.PersistenceFailure(new IllegalStateException("Invalid task status"))
            )
        yield Some(
          task.View(task.TaskId.from(keyId), details, state, eventRef.map(activity.ActivityId.from))
        )

  def get(id: task.TaskId): Try[Option[task.View]] = transact(session => view(session, id.toString))
  def listByFarm(id: Id): Try[List[task.View]] = transact { session =>
    session
      .query("SELECT id FROM tasks WHERE farm_id = ? AND deleted = 0 ORDER BY id", Seq(id.value))(
        _.getString(1)
      )
      .foldLeft[Either[ActionError, List[task.View]]](Right(Nil)) { (acc, key) =>
        for rows <- acc; item <- view(session, key) yield rows ++ item.toList
      }
  }
