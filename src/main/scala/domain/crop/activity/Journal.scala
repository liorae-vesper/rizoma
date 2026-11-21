package rizoma.domain.crop.activity

import rizoma.domain.inventory.StockId

import java.util.Locale

/** A positive quantity in the unit recorded by the user; no conversion is implied. */
final case class Quantity private (amount: BigDecimal, unit: String)
object Quantity:
  def from(amount: BigDecimal, unit: String): Either[String, Quantity] =
    for
      _ <- Either.cond(amount > 0, (), "Quantity must be positive")
      cleaned <- Journal.text(unit, "Unit")
    yield Quantity(amount, cleaned)

/** A nonnegative total cost or value, in a three-letter currency code. */
final case class Money private (amount: BigDecimal, currency: String)
object Money:
  def from(amount: BigDecimal, currency: String): Either[String, Money] =
    val code = currency.trim.toUpperCase(Locale.ROOT)
    for
      _ <- Either.cond(amount >= 0, (), "Money amount cannot be negative")
      _ <- Either.cond(code.matches("[A-Z]{3}"), (), "Currency must be a three-letter code")
    yield Money(amount, code)

/** Whether an input was purchased, produced on the farm, or has another or unknown origin. */
enum InputSource:
  case Purchased, FarmProduced, Other, Unknown

/** Identifies stock and the location from which this input was used. */
final case class StockLink private (stockId: StockId, location: String)
object StockLink:
  def from(stockId: StockId, location: String): Either[String, StockLink] =
    Journal.text(location, "Stock location").map(StockLink(stockId, _))

/** An input applied during the activity, including an optional total cost. */
final case class InputUse private (
    name: String,
    quantity: Quantity,
    method: Option[String],
    cost: Option[Money],
    source: InputSource,
    stock: Option[StockLink]
)
object InputUse:
  def from(
      name: String,
      quantity: Quantity,
      method: Option[String],
      cost: Option[Money],
      source: InputSource = InputSource.Unknown,
      stock: Option[StockLink] = None
  ): Either[String, InputUse] =
    for
      cleaned <- Journal.text(name, "Input name")
      cleanedMethod <- Journal.optionalText(method, "Application method")
    yield InputUse(cleaned, quantity, cleanedMethod, cost, source, stock)

/** Labor used for the activity; hours and optional total cost are recorded separately. */
final case class Labor private (worker: String, hours: BigDecimal, cost: Option[Money])
object Labor:
  def from(worker: String, hours: BigDecimal, cost: Option[Money]): Either[String, Labor] =
    for
      cleaned <- Journal.text(worker, "Worker")
      _ <- Either.cond(hours > 0, (), "Labor hours must be positive")
    yield Labor(cleaned, hours, cost)

enum Disposition:
  case Sold, Stored, Consumed, Other

/** Output and its destination, with an optional total value or sales revenue. */
final case class Output private (
    name: String,
    quantity: Quantity,
    disposition: Disposition,
    value: Option[Money]
)
object Output:
  def from(
      name: String,
      quantity: Quantity,
      disposition: Disposition,
      value: Option[Money]
  ): Either[String, Output] =
    Journal.text(name, "Output name").map(Output(_, quantity, disposition, value))

/** Additional field-journal details attached to a crop activity. */
final case class Journal private (
    operator: Option[String],
    notes: Option[String],
    inputs: List[InputUse],
    labor: List[Labor],
    outputs: List[Output]
)
object Journal:
  val empty: Journal = Journal(None, None, Nil, Nil, Nil)

  def from(
      operator: Option[String],
      notes: Option[String],
      inputs: List[InputUse],
      labor: List[Labor],
      outputs: List[Output]
  ): Either[String, Journal] =
    for
      cleanedOperator <- optionalText(operator, "Operator")
      cleanedNotes <- optionalText(notes, "Notes")
    yield Journal(cleanedOperator, cleanedNotes, inputs, labor, outputs)

  private[activity] def text(value: String, label: String): Either[String, String] =
    val cleaned = value.trim
    Either.cond(cleaned.nonEmpty, cleaned, s"$label cannot be blank")

  private[activity] def optionalText(
      value: Option[String],
      label: String
  ): Either[String, Option[String]] =
    value match
      case Some(raw) => text(raw, label).map(Some(_))
      case None      => Right(None)
