package rizoma.domain.harvest.lot

import rizoma.domain.ActionError
import rizoma.domain.harvest.HarvestId
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

import scala.util.Try

trait Store:
  def create(id: Id, input: Create): Try[Unit]
  def get(id: LotId): Try[Option[View]]
  def listByHarvest(id: HarvestId): Try[List[View]]

def create(input: Create)(using store: Store, ids: IdGenerator): Either[ActionError, LotId] =
  ids
    .next()
    .left
    .map(ActionError.Generation.apply)
    .flatMap(id =>
      store.create(id, input).toEither.left.map(ActionError.fromThrowable).map(_ => LotId.from(id))
    )
def get(id: LotId)(using store: Store): Either[ActionError, Option[View]] =
  store.get(id).toEither.left.map(ActionError.fromThrowable)
def listByHarvest(id: HarvestId)(using store: Store): Either[ActionError, List[View]] =
  store.listByHarvest(id).toEither.left.map(ActionError.fromThrowable)
