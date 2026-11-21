package rizoma.domain.traceability

import rizoma.domain.crop.{activity as cropActivity, cycle}
import rizoma.domain.harvest.lot as harvestLot
import rizoma.domain.{ActionError, inventory}

import java.time.Instant
import scala.util.Try

final case class TracedInput(
    activity: cropActivity.View,
    input: cropActivity.InputUse,
    stock: Option[inventory.View]
)
final case class LotTrace(
    lot: harvestLot.View,
    harvestedAt: Instant,
    activities: List[cropActivity.View],
    inputs: List[TracedInput]
):
  def hasUnlinkedInputs: Boolean = inputs.exists(_.stock.isEmpty)
  def hasUnknownInputOrigins: Boolean =
    inputs.exists(i => i.stock.exists(s => s.supplier.isEmpty || s.supplierBatch.isEmpty))
final case class CycleImpact(
    cycleId: cycle.CycleId,
    activities: List[cropActivity.View],
    lots: List[LotTrace]
)
final case class InputTrace(stock: inventory.View, cycles: List[CycleImpact]):
  def lots: List[LotTrace] = cycles.flatMap(_.lots)

trait Store:
  def lot(id: harvestLot.LotId): Try[Option[LotTrace]]
  def input(id: inventory.StockId): Try[Option[InputTrace]]

def lot(id: harvestLot.LotId)(using store: Store): Either[ActionError, Option[LotTrace]] =
  store.lot(id).toEither.left.map(ActionError.fromThrowable)
def input(id: inventory.StockId)(using store: Store): Either[ActionError, Option[InputTrace]] =
  store.input(id).toEither.left.map(ActionError.fromThrowable)
