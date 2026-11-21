package rizoma.domain.harvest.lot

import rizoma.domain.crop.cycle.CycleId
import rizoma.domain.crop.species.SpeciesId
import rizoma.domain.harvest.HarvestId
import rizoma.domain.inventory
import rizoma.domain.plot.PlotId
import rizoma.prelude.Id

import java.time.Instant

opaque type LotId = Id
object LotId:
  def from(id: Id): LotId = id
  def toStockId(id: LotId): inventory.StockId = inventory.StockId.from(id)
  def fromStockId(id: inventory.StockId): LotId = inventory.StockId.toId(id)

final case class Create private (
    harvestId: HarvestId,
    name: String,
    location: String,
    quantity: BigDecimal,
    occurredAt: Instant
)
object Create:
  def from(
      harvestId: HarvestId,
      name: String,
      location: String,
      quantity: BigDecimal,
      occurredAt: Instant
  ): Either[String, Create] =
    val cleanedName = name.trim
    val cleanedLocation = location.trim
    for
      _ <- Either.cond(cleanedName.nonEmpty, (), "Lot name cannot be blank")
      _ <- Either.cond(cleanedLocation.nonEmpty, (), "Location cannot be blank")
      _ <- Either.cond(quantity > 0, (), "Quantity must be positive")
    yield Create(harvestId, cleanedName, cleanedLocation, quantity, occurredAt)

/** A lot preserves its original harvest while stock tracks current locations and balances. */
final case class View(
    id: LotId,
    harvestId: HarvestId,
    cycleId: CycleId,
    plotId: PlotId,
    speciesId: SpeciesId,
    allocated: BigDecimal,
    stock: inventory.View
)
