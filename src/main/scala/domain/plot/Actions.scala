package rizoma.domain.plot

import rizoma.domain.ActionError
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

import scala.util.Try

trait Store:
  def insert(id: Id, input: Registration): Try[Unit]
  def update(input: Patch): Try[Unit]
  def delete(input: Delete): Try[Unit]
  def mark(input: Mark): Try[Unit]
  def demark(input: Demark): Try[Unit]
  def remeasure(input: Remeasure): Try[Unit]
  def activate(input: Activate): Try[Unit]
  def resting(input: Resting): Try[Unit]

def register(
    input: Registration
)(using store: Store, ids: IdGenerator): Either[ActionError, PlotId] =
  ids
    .next()
    .left
    .map(ActionError.Generation.apply)
    .flatMap(id =>
      store.insert(id, input).toEither.left.map(ActionError.fromThrowable).map(_ => PlotId.from(id))
    )

def edit(input: Patch)(using store: Store): Either[ActionError, Unit] =
  store.update(input).toEither.left.map(ActionError.fromThrowable)

def delete(input: Delete)(using store: Store): Either[ActionError, Unit] =
  store.delete(input).toEither.left.map(ActionError.fromThrowable)

def mark(input: Mark)(using store: Store): Either[ActionError, Unit] =
  store.mark(input).toEither.left.map(ActionError.fromThrowable)

def demark(input: Demark)(using store: Store): Either[ActionError, Unit] =
  store.demark(input).toEither.left.map(ActionError.fromThrowable)

def remeasure(input: Remeasure)(using store: Store): Either[ActionError, Unit] =
  store.remeasure(input).toEither.left.map(ActionError.fromThrowable)

def activate(input: Activate)(using store: Store): Either[ActionError, Unit] =
  store.activate(input).toEither.left.map(ActionError.fromThrowable)

def resting(input: Resting)(using store: Store): Either[ActionError, Unit] =
  store.resting(input).toEither.left.map(ActionError.fromThrowable)
