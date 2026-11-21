package rizoma.domain.farm

import rizoma.domain.ActionError
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

import scala.util.Try

trait Store:
  def insert(id: Id, input: Registration): Try[Unit]
  def update(input: Patch): Try[Unit]

def register(input: Registration)(using store: Store, ids: IdGenerator): Either[ActionError, Id] =
  ids.next().left.map(ActionError.Generation.apply).flatMap { id =>
    store.insert(id, input).toEither.left.map(ActionError.fromThrowable).map(_ => id)
  }

def edit(input: Patch)(using store: Store): Either[ActionError, Unit] =
  store.update(input).toEither.left.map(ActionError.fromThrowable)
