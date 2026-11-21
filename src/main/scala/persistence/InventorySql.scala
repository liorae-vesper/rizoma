package rizoma.persistence

import rizoma.domain.ActionError
import rizoma.prelude.jdbc.SqlSession

import java.time.Instant
import scala.util.Try

/** Transaction-scoped stock operations shared by inventory, harvest lots, and the journal. */
private[persistence] object InventorySql:
  final case class Stock(
      farmId: String,
      name: String,
      unit: String,
      kind: String,
      harvestId: Option[String],
      capacity: Option[BigDecimal]
  )

  def decimal(value: BigDecimal): String = value.bigDecimal.stripTrailingZeros.toString
  def add(left: BigDecimal, right: BigDecimal): BigDecimal = BigDecimal(
    left.bigDecimal.add(right.bigDecimal)
  )
  def subtract(left: BigDecimal, right: BigDecimal): BigDecimal = BigDecimal(
    left.bigDecimal.subtract(right.bigDecimal)
  )
  def negate(value: BigDecimal): BigDecimal = BigDecimal(value.bigDecimal.negate())
  def parse(value: String): Either[ActionError, BigDecimal] =
    Try(BigDecimal(value)).toEither.left.map(ActionError.PersistenceFailure.apply)

  def nonnegative(value: String): Either[ActionError, BigDecimal] =
    parse(value).flatMap(amount =>
      Either.cond(
        amount >= 0,
        amount,
        ActionError.PersistenceFailure(new IllegalStateException("Negative stored stock balance"))
      )
    )

  def positive(value: String): Either[ActionError, BigDecimal] =
    parse(value).flatMap(amount =>
      Either.cond(
        amount > 0,
        amount,
        ActionError.PersistenceFailure(new IllegalStateException("Invalid stored lot capacity"))
      )
    )

  /** Obtain SQLite's write lock before reading a balance. */
  def lock(session: SqlSession, id: String): Either[ActionError, Unit] =
    val count = session.execute("UPDATE stock_items SET id = id WHERE id = ?", Seq(id))
    Either.cond(count == 1, (), ActionError.NotFound("stock", id))

  def stock(session: SqlSession, id: String): Either[ActionError, Stock] =
    session.queryOne(
      "SELECT farm_id, name, unit, kind, harvest_id, capacity FROM stock_items WHERE id = ?",
      Seq(id)
    ) { row =>
      (
        row.getString(1),
        row.getString(2),
        row.getString(3),
        row.getString(4),
        Option(row.getString(5)),
        Option(row.getString(6))
      )
    } match
      case None => Left(ActionError.NotFound("stock", id))
      case Some((farm, name, unit, kind, harvest, capacity)) =>
        (kind, harvest, capacity) match
          case ("Input", None, None) => Right(Stock(farm, name, unit, kind, None, None))
          case ("Harvest", Some(origin), Some(value)) =>
            positive(value).map(v => Stock(farm, name, unit, kind, Some(origin), Some(v)))
          case _ =>
            Left(
              ActionError.PersistenceFailure(new IllegalStateException("Invalid stock provenance"))
            )

  def balance(
      session: SqlSession,
      stockId: String,
      location: String
  ): Either[ActionError, BigDecimal] =
    session.queryOne(
      "SELECT quantity FROM stock_balances WHERE stock_id = ? AND location = ?",
      Seq(stockId, location)
    )(_.getString(1)) match
      case Some(value) => nonnegative(value)
      case None        => Right(BigDecimal(0))

  def change(
      session: SqlSession,
      stockId: String,
      location: String,
      delta: BigDecimal
  ): Either[ActionError, Unit] =
    balance(session, stockId, location).flatMap { current =>
      val next = add(current, delta)
      if next < 0 then
        Left(ActionError.InsufficientStock(stockId, location, current, negate(delta)))
      else
        val _ = session.execute(
          "INSERT INTO stock_balances (stock_id, location, quantity) VALUES (?, ?, ?) ON CONFLICT(stock_id, location) DO UPDATE SET quantity = excluded.quantity",
          Seq(stockId, location, decimal(next))
        )
        Right(())
    }

  def movement(
      session: SqlSession,
      stockId: String,
      kind: String,
      from: Option[String],
      to: Option[String],
      quantity: BigDecimal,
      at: Instant,
      activityId: Option[String] = None
  ): Unit =
    val _ = session.execute(
      "INSERT INTO stock_movements (stock_id, kind, from_location, to_location, quantity, occurred_at, activity_id) VALUES (?, ?, ?, ?, ?, ?, ?)",
      Seq(stockId, kind, from.orNull, to.orNull, decimal(quantity), at.toString, activityId.orNull)
    )
