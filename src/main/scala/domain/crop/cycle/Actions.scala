package rizoma.domain.crop.cycle

import rizoma.domain.{ActionError, harvest as harvestDomain}
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

import scala.util.Try

trait Store:
  def insert(id: Id, input: Registration): Try[Unit]
  def update(input: Patch): Try[Unit]
  def delete(input: Delete): Try[Unit]
  def finish(input: Finish): Try[Unit]

def register(
    input: Registration
)(using store: Store, ids: IdGenerator): Either[ActionError, CycleId] =
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
        .map(_ => CycleId.from(id))
    )

def plant(input: Plant)(using store: Store, ids: IdGenerator): Either[ActionError, CycleId] =
  Registration
    .from(input.speciesId, input.plotId, input.occupiedLand, Some(input.plantedAt))
    .left
    .map[ActionError](error => ActionError.InvalidState(error.toString))
    .flatMap(register(_))

def harvest(input: harvestDomain.Registration)(using
    store: harvestDomain.Store,
    ids: IdGenerator
): Either[ActionError, harvestDomain.HarvestId] =
  harvestDomain.register(input)

def edit(input: Patch)(using store: Store): Either[ActionError, Unit] =
  store.update(input).toEither.left.map(ActionError.fromThrowable)

def delete(input: Delete)(using store: Store): Either[ActionError, Unit] =
  store.delete(input).toEither.left.map(ActionError.fromThrowable)

def finish(input: Finish)(using store: Store): Either[ActionError, Unit] =
  store.finish(input).toEither.left.map(ActionError.fromThrowable)
