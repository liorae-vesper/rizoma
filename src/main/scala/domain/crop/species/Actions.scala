package rizoma.domain.crop.species

import rizoma.domain.ActionError
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

import scala.util.Try

trait Store:
  def insert(id: Id, input: Registration): Try[Unit]
  def update(input: Patch): Try[Unit]
  def delete(input: Delete): Try[Unit]
  def get(id: SpeciesId): Try[Option[View]]
  def list(): Try[List[View]]

def register(
    input: Registration
)(using store: Store, ids: IdGenerator): Either[ActionError, SpeciesId] =
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
        .map(_ => SpeciesId.from(id))
    )

def edit(input: Patch)(using store: Store): Either[ActionError, Unit] =
  store.update(input).toEither.left.map(ActionError.fromThrowable)

def delete(input: Delete)(using store: Store): Either[ActionError, Unit] =
  store.delete(input).toEither.left.map(ActionError.fromThrowable)

def remove(input: Delete)(using store: Store): Either[ActionError, Unit] =
  delete(input)

def get(id: SpeciesId)(using store: Store): Either[ActionError, Option[View]] =
  store.get(id).toEither.left.map(ActionError.fromThrowable)

def list()(using store: Store): Either[ActionError, List[View]] =
  store.list().toEither.left.map(ActionError.fromThrowable)
