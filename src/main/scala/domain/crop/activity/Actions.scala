package rizoma.domain.crop.activity

import rizoma.domain.ActionError
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

import scala.util.Try

trait Store:
  def insert(id: Id, input: Registration): Try[Unit]
  def update(input: Patch): Try[Unit]
  def delete(input: Delete): Try[Unit]
  def get(id: ActivityId): Try[Option[View]]
  def listByCycle(id: rizoma.domain.crop.cycle.CycleId): Try[List[View]]

def register(
    input: Registration
)(using store: Store, ids: IdGenerator): Either[ActionError, ActivityId] =
  ids
    .next()
    .left
    .map(ActionError.Generation.apply)
    .flatMap(id =>
      store
        .insert(id, input)
        .toEither
        .left
        .map(ActionError.fromThrowable)
        .map(_ => ActivityId.from(id))
    )

def record(
    input: Registration
)(using store: Store, ids: IdGenerator): Either[ActionError, ActivityId] =
  register(input)

def edit(input: Patch)(using store: Store): Either[ActionError, Unit] =
  store.update(input).toEither.left.map(ActionError.fromThrowable)

def delete(input: Delete)(using store: Store): Either[ActionError, Unit] =
  store.delete(input).toEither.left.map(ActionError.fromThrowable)

def get(id: ActivityId)(using store: Store): Either[ActionError, Option[View]] =
  store.get(id).toEither.left.map(ActionError.fromThrowable)

def listByCycle(id: rizoma.domain.crop.cycle.CycleId)(using
    store: Store
): Either[ActionError, List[View]] =
  store.listByCycle(id).toEither.left.map(ActionError.fromThrowable)
