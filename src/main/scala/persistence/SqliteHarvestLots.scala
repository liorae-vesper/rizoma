package rizoma.persistence

import rizoma.domain.crop.{cycle, species}
import rizoma.domain.harvest.lot
import rizoma.domain.{ActionError, harvest, plot}
import rizoma.prelude.Id
import rizoma.prelude.jdbc.{Database, SqlSession}

import scala.util.{Failure, Success, Try}

final class SqliteHarvestLots(db: Database) extends lot.Store:
  private val stocks = new SqliteInventory(db)
  private def transact[A](run: SqlSession => Either[ActionError, A]): Try[A] =
    Try(db.transactionEither(run)).flatMap(_.fold(Failure(_), Success(_)))

  def create(id: Id, input: lot.Create): Try[Unit] = transact { session =>
    val locked = session.execute(
      "UPDATE harvests SET id = id WHERE id = ? AND deleted = 0",
      Seq(input.harvestId.toString)
    )
    val origin = session.queryOne(
      "SELECT p.farm_id, h.amount, h.unit_of_yield, c.id, c.plot_id, c.species_id FROM harvests h JOIN cycles c ON c.id = h.cycle_id JOIN plots p ON p.id = c.plot_id WHERE h.id = ? AND h.deleted = 0 AND c.deleted = 0 AND p.deleted = 0",
      Seq(input.harvestId.toString)
    ) { r =>
      (
        r.getString(1),
        r.getDouble(2),
        r.getString(3),
        r.getString(4),
        r.getString(5),
        r.getString(6)
      )
    }
    (if locked == 1 then origin else None)
      .toRight(ActionError.InvalidReference("harvest"))
      .flatMap { (farmId, amount, unit, cycleId, plotId, speciesId) =>
        if !amount.isFinite || amount <= 0 then
          Left(ActionError.InvalidState("Stored harvest amount is invalid"))
        else
          val allocated = session
            .query(
              "SELECT capacity FROM stock_items WHERE harvest_id = ? AND kind = 'Harvest'",
              Seq(input.harvestId.toString)
            )(_.getString(1))
            .foldLeft[Either[ActionError, BigDecimal]](Right(BigDecimal(0)))((sum, value) =>
              for a <- sum; b <- InventorySql.positive(value) yield InventorySql.add(a, b)
            )
          allocated.flatMap { used =>
            if InventorySql.add(used, input.quantity) > BigDecimal.decimal(amount) then
              Left(ActionError.InvalidState("Lot allocation exceeds harvest quantity"))
            else
              val _ = session.execute(
                "INSERT INTO stock_items (id, farm_id, name, unit, kind, harvest_id, capacity) VALUES (?, ?, ?, ?, 'Harvest', ?, ?)",
                Seq(
                  id.value,
                  farmId,
                  input.name,
                  unit,
                  input.harvestId.toString,
                  InventorySql.decimal(input.quantity)
                )
              )
              val _ = session.execute(
                "INSERT INTO harvest_lot_origins (stock_id, cycle_id, plot_id, species_id) VALUES (?, ?, ?, ?)",
                Seq(id.value, cycleId, plotId, speciesId)
              )
              InventorySql.change(session, id.value, input.location, input.quantity).map { _ =>
                InventorySql.movement(
                  session,
                  id.value,
                  "Harvest",
                  None,
                  Some(input.location),
                  input.quantity,
                  input.occurredAt
                )
              }
          }
      }
  }

  private[persistence] def getIn(session: SqlSession, id: String): Either[ActionError, lot.View] =
    for
      metadata <- InventorySql.stock(session, id)
      _ <- Either.cond(metadata.kind == "Harvest", (), ActionError.NotFound("harvest lot", id))
      origin <- metadata.harvestId.toRight(
        ActionError.PersistenceFailure(new IllegalStateException("Harvest lot lacks origin"))
      )
      allocated <- metadata.capacity.toRight(
        ActionError.PersistenceFailure(new IllegalStateException("Harvest lot lacks capacity"))
      )
      harvestId <- Id
        .parse(origin)
        .left
        .map(error => ActionError.PersistenceFailure(new IllegalStateException(error.toString)))
      lotId <- Id
        .parse(id)
        .left
        .map(error => ActionError.PersistenceFailure(new IllegalStateException(error.toString)))
      source <- session
        .queryOne(
          "SELECT o.cycle_id, o.plot_id, o.species_id, p.farm_id, h.unit_of_yield FROM harvest_lot_origins o JOIN plots p ON p.id = o.plot_id JOIN harvests h ON h.id = ? AND h.cycle_id = o.cycle_id WHERE o.stock_id = ?",
          Seq(origin, id)
        ) { row =>
          (row.getString(1), row.getString(2), row.getString(3), row.getString(4), row.getString(5))
        }
        .toRight(
          ActionError.PersistenceFailure(new IllegalStateException("Harvest lot origin is missing"))
        )
      _ <- Either.cond(
        source._4 == metadata.farmId && source._5 == metadata.unit,
        (),
        ActionError.PersistenceFailure(
          new IllegalStateException("Harvest lot origin does not match its stock")
        )
      )
      cycleId <- Id
        .parse(source._1)
        .left
        .map(error => ActionError.PersistenceFailure(new IllegalStateException(error.toString)))
      plotId <- Id
        .parse(source._2)
        .left
        .map(error => ActionError.PersistenceFailure(new IllegalStateException(error.toString)))
      speciesId <- Id
        .parse(source._3)
        .left
        .map(error => ActionError.PersistenceFailure(new IllegalStateException(error.toString)))
      view <- stocks.getIn(session, id)
    yield lot.View(
      lot.LotId.from(lotId),
      harvest.HarvestId.from(harvestId),
      cycle.CycleId.from(cycleId),
      plot.PlotId.from(plotId),
      species.SpeciesId.from(speciesId),
      allocated,
      view
    )

  def get(id: lot.LotId): Try[Option[lot.View]] = transact { session =>
    getIn(session, id.toString) match
      case Left(_: ActionError.NotFound) => Right(None)
      case Left(error)                   => Left(error)
      case Right(value)                  => Right(Some(value))
  }

  def listByHarvest(id: harvest.HarvestId): Try[List[lot.View]] = transact { session =>
    session
      .query(
        "SELECT id FROM stock_items WHERE harvest_id = ? AND kind = 'Harvest' ORDER BY id",
        Seq(id.toString)
      )(_.getString(1))
      .foldLeft[Either[ActionError, List[lot.View]]](Right(Nil)) { (acc, value) =>
        for rows <- acc; item <- getIn(session, value) yield item :: rows
      }
      .map(_.reverse)
  }
