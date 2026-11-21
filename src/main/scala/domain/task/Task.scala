package rizoma.domain.task

import rizoma.domain.crop.{activity, cycle}
import rizoma.domain.{ActionError, plot}
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

import java.time.Instant
import scala.util.Try

opaque type TaskId = Id
object TaskId:
  def from(id: Id): TaskId = id

enum Status:
  case Planned, InProgress, Completed, Cancelled

final case class Create private (
    farmId: Id,
    title: String,
    dueAt: Option[Instant],
    assignee: Option[Id],
    plotId: Option[plot.PlotId],
    cycleId: Option[cycle.CycleId]
)
object Create:
  def from(
      farmId: Id,
      title: String,
      dueAt: Option[Instant],
      assignee: Option[Id],
      plotId: Option[plot.PlotId],
      cycleId: Option[cycle.CycleId]
  ): Either[String, Create] =
    Either.cond(
      title.trim.nonEmpty,
      Create(farmId, title.trim, dueAt, assignee, plotId, cycleId),
      "Task title cannot be blank"
    )

final case class View(
    id: TaskId,
    details: Create,
    status: Status,
    activityId: Option[activity.ActivityId]
)
trait Store:
  def insert(id: Id, input: Create): Try[Unit]
  def update(id: TaskId, input: Create): Try[Unit]
  def status(id: TaskId, status: Status): Try[Unit]
  def complete(id: TaskId, record: Option[(Id, activity.Registration)]): Try[Unit]
  def delete(id: TaskId): Try[Unit]
  def get(id: TaskId): Try[Option[View]]
  def listByFarm(id: Id): Try[List[View]]

def create(input: Create)(using store: Store, ids: IdGenerator): Either[ActionError, TaskId] =
  ids
    .next()
    .left
    .map(ActionError.Generation.apply)
    .flatMap(id =>
      store.insert(id, input).toEither.left.map(ActionError.fromThrowable).map(_ => TaskId.from(id))
    )
def update(id: TaskId, input: Create)(using store: Store): Either[ActionError, Unit] =
  store.update(id, input).toEither.left.map(ActionError.fromThrowable)
def status(id: TaskId, value: Status)(using store: Store): Either[ActionError, Unit] =
  store.status(id, value).toEither.left.map(ActionError.fromThrowable)
def complete(id: TaskId, record: Option[activity.Registration])(using
    store: Store,
    ids: IdGenerator
): Either[ActionError, Unit] =
  val withId = record match
    case None        => Right(None)
    case Some(input) =>
      ids.next().left.map(ActionError.Generation.apply).map(id => Some((id, input)))
  withId.flatMap(value => store.complete(id, value).toEither.left.map(ActionError.fromThrowable))
def delete(id: TaskId)(using store: Store): Either[ActionError, Unit] =
  store.delete(id).toEither.left.map(ActionError.fromThrowable)
def get(id: TaskId)(using store: Store): Either[ActionError, Option[View]] =
  store.get(id).toEither.left.map(ActionError.fromThrowable)
def listByFarm(id: Id)(using store: Store): Either[ActionError, List[View]] =
  store.listByFarm(id).toEither.left.map(ActionError.fromThrowable)
