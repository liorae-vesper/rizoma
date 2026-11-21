package rizoma.persistence

import rizoma.domain.crop.activity.ActivityId
import rizoma.domain.harvest.HarvestId
import rizoma.domain.{ActionError, inventory}
import rizoma.prelude.Id
import rizoma.prelude.jdbc.Database

import java.time.Instant
import scala.util.{Failure, Success, Try}

final class SqliteInventory(db: Database) extends inventory.Store:
  def initialize(): Unit = db.transaction(session => initialize(session))
  private[persistence] def initialize(session: rizoma.prelude.jdbc.SqlSession): Unit =
    val _ = session.execute("""CREATE TABLE IF NOT EXISTS stock_items (
      id TEXT PRIMARY KEY, farm_id TEXT NOT NULL REFERENCES farms(id), name TEXT NOT NULL,
      unit TEXT NOT NULL, kind TEXT NOT NULL, harvest_id TEXT REFERENCES harvests(id), capacity TEXT,
      CHECK ((kind = 'Input' AND harvest_id IS NULL AND capacity IS NULL) OR
             (kind = 'Harvest' AND harvest_id IS NOT NULL AND capacity IS NOT NULL))
    )""")
    val _ = session.execute("""CREATE TABLE IF NOT EXISTS stock_balances (
      stock_id TEXT NOT NULL REFERENCES stock_items(id), location TEXT NOT NULL, quantity TEXT NOT NULL,
      PRIMARY KEY(stock_id, location)
    )""")
    val _ = session.execute("""CREATE TABLE IF NOT EXISTS stock_receipt_origins (
      stock_id TEXT PRIMARY KEY REFERENCES stock_items(id), supplier TEXT, supplier_batch TEXT
    )""")
    val _ = session.execute("""CREATE TABLE IF NOT EXISTS stock_movements (
      sequence INTEGER PRIMARY KEY AUTOINCREMENT, stock_id TEXT NOT NULL REFERENCES stock_items(id),
      kind TEXT NOT NULL, from_location TEXT, to_location TEXT, quantity TEXT NOT NULL,
      occurred_at TEXT NOT NULL, activity_id TEXT REFERENCES activities(id)
    )""")
    val _ = session.execute("""CREATE TABLE IF NOT EXISTS harvest_lot_origins (
      stock_id TEXT PRIMARY KEY REFERENCES stock_items(id),
      cycle_id TEXT NOT NULL REFERENCES cycles(id), plot_id TEXT NOT NULL REFERENCES plots(id),
      species_id TEXT NOT NULL REFERENCES species(id)
    )""")
    val _ = session.execute(
      """INSERT OR IGNORE INTO harvest_lot_origins (stock_id, cycle_id, plot_id, species_id)
      SELECT s.id, c.id, c.plot_id, c.species_id FROM stock_items s
      JOIN harvests h ON h.id = s.harvest_id JOIN cycles c ON c.id = h.cycle_id
      WHERE s.kind = 'Harvest'"""
    )
    val _ =
      session.execute("CREATE INDEX IF NOT EXISTS stock_items_farm_idx ON stock_items(farm_id, id)")
    val _ = session.execute(
      "CREATE INDEX IF NOT EXISTS stock_items_harvest_idx ON stock_items(harvest_id, id)"
    )
    val _ = session.execute(
      "CREATE INDEX IF NOT EXISTS stock_movements_stock_idx ON stock_movements(stock_id, sequence)"
    )

  private def transact[A](run: rizoma.prelude.jdbc.SqlSession => Either[ActionError, A]): Try[A] =
    Try(db.transactionEither(run)).flatMap(_.fold(Failure(_), Success(_)))

  def receive(id: Id, input: inventory.Receipt): Try[Unit] = transact { session =>
    val count = session.execute(
      "INSERT INTO stock_items (id, farm_id, name, unit, kind) SELECT ?, ?, ?, ?, 'Input' WHERE EXISTS (SELECT 1 FROM farms WHERE id = ?)",
      Seq(id.value, input.farmId.value, input.name, input.unit, input.farmId.value)
    )
    if count != 1 then Left(ActionError.InvalidReference("farm"))
    else
      val _ = session.execute(
        "INSERT INTO stock_receipt_origins (stock_id, supplier, supplier_batch) VALUES (?, ?, ?)",
        Seq(id.value, input.supplier.orNull, input.supplierBatch.orNull)
      )
      InventorySql.change(session, id.value, input.location, input.quantity).map { _ =>
        InventorySql.movement(
          session,
          id.value,
          "Receipt",
          None,
          Some(input.location),
          input.quantity,
          input.occurredAt
        )
      }
  }

  def use(input: inventory.Use): Try[Unit] = transact { session =>
    for
      _ <- InventorySql.lock(session, input.stockId.toString)
      _ <- InventorySql.stock(session, input.stockId.toString)
      _ <- InventorySql.change(
        session,
        input.stockId.toString,
        input.location,
        InventorySql.negate(input.quantity)
      )
    yield InventorySql.movement(
      session,
      input.stockId.toString,
      "Use",
      Some(input.location),
      None,
      input.quantity,
      input.occurredAt
    )
  }

  def recordOrigin(input: inventory.Origin): Try[Unit] = transact { session =>
    for
      _ <- InventorySql.lock(session, input.stockId.toString)
      stock <- getIn(session, input.stockId.toString)
      _ <- Either.cond(
        stock.kind == inventory.Kind.Input,
        (),
        ActionError.InvalidState("Origin is only available for input stock")
      )
      _ <- Either.cond(
        input.supplier.forall(s => stock.supplier.forall(_ == s)) && input.supplierBatch.forall(b =>
          stock.supplierBatch.forall(_ == b)
        ),
        (),
        ActionError.InvalidState("Existing input origin cannot be replaced")
      )
    yield
      val _ = session.execute(
        "INSERT INTO stock_receipt_origins (stock_id, supplier, supplier_batch) VALUES (?, ?, ?) ON CONFLICT(stock_id) DO UPDATE SET supplier = COALESCE(stock_receipt_origins.supplier, excluded.supplier), supplier_batch = COALESCE(stock_receipt_origins.supplier_batch, excluded.supplier_batch)",
        Seq(input.stockId.toString, input.supplier.orNull, input.supplierBatch.orNull)
      )
  }

  def transfer(input: inventory.Transfer): Try[Unit] = transact { session =>
    for
      _ <- InventorySql.lock(session, input.stockId.toString)
      _ <- InventorySql.stock(session, input.stockId.toString)
      _ <- InventorySql.change(
        session,
        input.stockId.toString,
        input.fromLocation,
        InventorySql.negate(input.quantity)
      )
      _ <- InventorySql.change(session, input.stockId.toString, input.toLocation, input.quantity)
    yield InventorySql.movement(
      session,
      input.stockId.toString,
      "Transfer",
      Some(input.fromLocation),
      Some(input.toLocation),
      input.quantity,
      input.occurredAt
    )
  }

  def adjust(input: inventory.Adjustment): Try[Unit] = transact { session =>
    for
      _ <- InventorySql.lock(session, input.stockId.toString)
      stock <- InventorySql.stock(session, input.stockId.toString)
      current <- InventorySql.balance(session, input.stockId.toString, input.location)
      all <- session
        .query(
          "SELECT quantity FROM stock_balances WHERE stock_id = ?",
          Seq(input.stockId.toString)
        )(_.getString(1))
        .foldLeft[Either[ActionError, BigDecimal]](Right(BigDecimal(0)))((sum, value) =>
          for a <- sum; b <- InventorySql.nonnegative(value) yield InventorySql.add(a, b)
        )
      _ <- Either.cond(
        stock.capacity.forall(
          InventorySql.add(InventorySql.subtract(all, current), input.quantity) <= _
        ),
        (),
        ActionError.InvalidState("Harvest lot balance exceeds allocated quantity")
      )
      delta = InventorySql.subtract(input.quantity, current)
      _ <-
        if delta == 0 then Right(())
        else InventorySql.change(session, input.stockId.toString, input.location, delta)
    yield
      if delta != 0 then
        InventorySql.movement(
          session,
          input.stockId.toString,
          "Adjustment",
          Some(input.location),
          Some(input.location),
          delta,
          input.occurredAt
        )
  }

  private[persistence] def getIn(
      session: rizoma.prelude.jdbc.SqlSession,
      id: String
  ): Either[ActionError, inventory.View] =
    for
      meta <- InventorySql.stock(session, id)
      stockId <- Id
        .parse(id)
        .left
        .map(e => ActionError.PersistenceFailure(new IllegalStateException(e.toString)))
      farmId <- Id
        .parse(meta.farmId)
        .left
        .map(e => ActionError.PersistenceFailure(new IllegalStateException(e.toString)))
      kind <- inventory.Kind.values
        .find(_.toString == meta.kind)
        .toRight(ActionError.PersistenceFailure(new IllegalStateException("Invalid stock kind")))
      _ <- Either.cond(
        (kind == inventory.Kind.Input && meta.harvestId.isEmpty && meta.capacity.isEmpty) || (kind == inventory.Kind.Harvest && meta.harvestId.nonEmpty && meta.capacity
          .exists(_ > 0)),
        (),
        ActionError.PersistenceFailure(new IllegalStateException("Invalid stock provenance"))
      )
      origin <- meta.harvestId match
        case None        => Right(None)
        case Some(value) =>
          Id.parse(value)
            .left
            .map(e => ActionError.PersistenceFailure(new IllegalStateException(e.toString)))
            .map(id => Some(HarvestId.from(id)))
      balances <- session
        .query(
          "SELECT location, quantity FROM stock_balances WHERE stock_id = ? ORDER BY location",
          Seq(id)
        )(r => (r.getString(1), r.getString(2)))
        .foldLeft[Either[ActionError, List[inventory.Balance]]](Right(Nil)) {
          case (acc, (location, value)) =>
            for rows <- acc; quantity <- InventorySql.nonnegative(value)
            yield inventory.Balance(location, quantity) :: rows
        }
        .map(_.reverse)
      receiptOrigin = session.queryOne(
        "SELECT supplier, supplier_batch FROM stock_receipt_origins WHERE stock_id = ?",
        Seq(id)
      )(r => (Option(r.getString(1)), Option(r.getString(2))))
    yield inventory.View(
      inventory.StockId.from(stockId),
      farmId,
      meta.name,
      meta.unit,
      kind,
      origin,
      meta.capacity,
      balances,
      receiptOrigin.flatMap(_._1),
      receiptOrigin.flatMap(_._2)
    )

  def get(id: inventory.StockId): Try[Option[inventory.View]] = transact { session =>
    getIn(session, id.toString) match
      case Left(_: ActionError.NotFound) => Right(None)
      case Left(error)                   => Left(error)
      case Right(value)                  => Right(Some(value))
  }

  def listByFarm(id: Id): Try[List[inventory.View]] = transact { session =>
    session
      .query("SELECT id FROM stock_items WHERE farm_id = ? ORDER BY id", Seq(id.value))(
        _.getString(1)
      )
      .foldLeft[Either[ActionError, List[inventory.View]]](Right(Nil)) { (acc, value) =>
        for rows <- acc; item <- getIn(session, value) yield item :: rows
      }
      .map(_.reverse)
  }

  def history(id: inventory.StockId): Try[List[inventory.Movement]] = transact { session =>
    InventorySql.stock(session, id.toString).flatMap { _ =>
      session
        .query(
          "SELECT sequence, kind, from_location, to_location, quantity, occurred_at, activity_id FROM stock_movements WHERE stock_id = ? ORDER BY sequence",
          Seq(id.toString)
        ) { r =>
          (
            r.getLong(1),
            r.getString(2),
            Option(r.getString(3)),
            Option(r.getString(4)),
            r.getString(5),
            r.getString(6),
            Option(r.getString(7))
          )
        }
        .foldLeft[Either[ActionError, List[inventory.Movement]]](Right(Nil)) {
          case (acc, (sequence, kind, from, to, amount, timestamp, activityId)) =>
            for
              rows <- acc
              quantity <- InventorySql.parse(amount)
              movementKind <- inventory.MovementKind.values
                .find(_.toString == kind)
                .toRight(
                  ActionError.PersistenceFailure(new IllegalStateException("Invalid movement kind"))
                )
              sourceActivity <- activityId match
                case None        => Right(None)
                case Some(value) =>
                  Id.parse(value)
                    .left
                    .map(e => ActionError.PersistenceFailure(new IllegalStateException(e.toString)))
                    .map(id => Some(ActivityId.from(id)))
              at <- Try(Instant.parse(timestamp)).toEither.left
                .map(ActionError.PersistenceFailure.apply)
            yield inventory.Movement(
              sequence,
              movementKind,
              from,
              to,
              quantity,
              at,
              sourceActivity
            ) :: rows
        }
        .map(_.reverse)
    }
  }
