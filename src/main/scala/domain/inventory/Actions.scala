package rizoma.domain.inventory

import rizoma.domain.ActionError
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

import scala.util.Try

trait Store:
  def receive(id: Id, input: Receipt): Try[Unit]
  def recordOrigin(input: Origin): Try[Unit]
  def use(input: Use): Try[Unit]
  def transfer(input: Transfer): Try[Unit]
  def adjust(input: Adjustment): Try[Unit]
  def get(id: StockId): Try[Option[View]]
  def listByFarm(id: Id): Try[List[View]]
  def history(id: StockId): Try[List[Movement]]

def receive(input: Receipt)(using store: Store, ids: IdGenerator): Either[ActionError, StockId] =
  ids
    .next()
    .left
    .map(ActionError.Generation.apply)
    .flatMap(id =>
      store
        .receive(id, input)
        .toEither
        .left
        .map(ActionError.fromThrowable)
        .map(_ => StockId.from(id))
    )
def use(input: Use)(using store: Store): Either[ActionError, Unit] =
  store.use(input).toEither.left.map(ActionError.fromThrowable)
def recordOrigin(input: Origin)(using store: Store): Either[ActionError, Unit] =
  store.recordOrigin(input).toEither.left.map(ActionError.fromThrowable)
def transfer(input: Transfer)(using store: Store): Either[ActionError, Unit] =
  store.transfer(input).toEither.left.map(ActionError.fromThrowable)
def adjust(input: Adjustment)(using store: Store): Either[ActionError, Unit] =
  store.adjust(input).toEither.left.map(ActionError.fromThrowable)
def get(id: StockId)(using store: Store): Either[ActionError, Option[View]] =
  store.get(id).toEither.left.map(ActionError.fromThrowable)
def listByFarm(id: Id)(using store: Store): Either[ActionError, List[View]] =
  store.listByFarm(id).toEither.left.map(ActionError.fromThrowable)
def history(id: StockId)(using store: Store): Either[ActionError, List[Movement]] =
  store.history(id).toEither.left.map(ActionError.fromThrowable)
