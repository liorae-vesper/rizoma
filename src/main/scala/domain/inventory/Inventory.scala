package rizoma.domain.inventory

import rizoma.domain.crop.activity.ActivityId
import rizoma.prelude.Id

import java.time.Instant

opaque type StockId = Id
object StockId:
  def from(id: Id): StockId = id
  def toId(id: StockId): Id = id

private[inventory] object Validation:
  def text(value: String, label: String): Either[String, String] =
    val cleaned = value.trim
    Either.cond(cleaned.nonEmpty, cleaned, s"$label cannot be blank")
  def positive(value: BigDecimal): Either[String, BigDecimal] =
    Either.cond(value > 0, value, "Quantity must be positive")

/** A new input stock batch. Amounts and locations are specific to this farm. */
final case class Receipt private (
    farmId: Id,
    name: String,
    unit: String,
    location: String,
    quantity: BigDecimal,
    occurredAt: Instant,
    supplier: Option[String],
    supplierBatch: Option[String]
)
object Receipt:
  def from(
      farmId: Id,
      name: String,
      unit: String,
      location: String,
      quantity: BigDecimal,
      occurredAt: Instant,
      supplier: Option[String] = None,
      supplierBatch: Option[String] = None
  ): Either[String, Receipt] =
    for
      n <- Validation.text(name, "Name")
      u <- Validation.text(unit, "Unit")
      l <- Validation.text(location, "Location")
      q <- Validation.positive(quantity)
      s <- optionalText(supplier, "Supplier")
      b <- optionalText(supplierBatch, "Supplier batch")
    yield Receipt(farmId, n, u, l, q, occurredAt, s, b)

private def optionalText(value: Option[String], label: String): Either[String, Option[String]] =
  value match
    case None       => Right(None)
    case Some(text) => Validation.text(text, label).map(Some(_))

final case class Origin private (
    stockId: StockId,
    supplier: Option[String],
    supplierBatch: Option[String]
)
object Origin:
  def from(
      stockId: StockId,
      supplier: Option[String],
      supplierBatch: Option[String]
  ): Either[String, Origin] =
    for
      s <- optionalText(supplier, "Supplier")
      b <- optionalText(supplierBatch, "Supplier batch")
      _ <- Either.cond(s.nonEmpty || b.nonEmpty, (), "At least one origin detail is required")
    yield Origin(stockId, s, b)

final case class Use private (
    stockId: StockId,
    location: String,
    quantity: BigDecimal,
    occurredAt: Instant
)
object Use:
  def from(
      stockId: StockId,
      location: String,
      quantity: BigDecimal,
      occurredAt: Instant
  ): Either[String, Use] =
    for
      l <- Validation.text(location, "Location")
      q <- Validation.positive(quantity)
    yield Use(stockId, l, q, occurredAt)

final case class Transfer private (
    stockId: StockId,
    fromLocation: String,
    toLocation: String,
    quantity: BigDecimal,
    occurredAt: Instant
)
object Transfer:
  def from(
      stockId: StockId,
      fromLocation: String,
      toLocation: String,
      quantity: BigDecimal,
      occurredAt: Instant
  ): Either[String, Transfer] =
    for
      source <- Validation.text(fromLocation, "Source")
      destination <- Validation.text(toLocation, "Destination")
      _ <- Either.cond(source != destination, (), "Locations must differ")
      q <- Validation.positive(quantity)
    yield Transfer(stockId, source, destination, q, occurredAt)

/** Replace a location's balance with the amount physically counted. */
final case class Adjustment private (
    stockId: StockId,
    location: String,
    quantity: BigDecimal,
    occurredAt: Instant
)
object Adjustment:
  def from(
      stockId: StockId,
      location: String,
      quantity: BigDecimal,
      occurredAt: Instant
  ): Either[String, Adjustment] =
    for
      l <- Validation.text(location, "Location")
      _ <- Either.cond(quantity >= 0, (), "Count cannot be negative")
    yield Adjustment(stockId, l, quantity, occurredAt)

enum Kind:
  case Input, Harvest

final case class Balance(location: String, quantity: BigDecimal)
final case class View(
    id: StockId,
    farmId: Id,
    name: String,
    unit: String,
    kind: Kind,
    harvestId: Option[rizoma.domain.harvest.HarvestId],
    capacity: Option[BigDecimal],
    balances: List[Balance],
    supplier: Option[String] = None,
    supplierBatch: Option[String] = None
)
enum MovementKind:
  case Receipt, Use, Transfer, Adjustment, JournalUse, JournalReversal, Harvest

final case class Movement(
    sequence: Long,
    kind: MovementKind,
    fromLocation: Option[String],
    toLocation: Option[String],
    quantity: BigDecimal,
    occurredAt: Instant,
    activityId: Option[ActivityId]
)
