package rizoma.persistence

import rizoma.domain.crop.activity
import rizoma.domain.harvest.lot as harvestLot
import rizoma.domain.{ActionError, inventory, traceability}
import rizoma.prelude.jdbc.{Database, SqlParam, SqlSession}

import java.time.Instant
import scala.util.{Failure, Success, Try}

final class SqliteTraceability(db: Database) extends traceability.Store:
  private val stocks = new SqliteInventory(db)
  private val lots = new SqliteHarvestLots(db)
  private val activities = new SqliteActivities(db)
  private def transact[A](run: SqlSession => Either[ActionError, A]): Try[A] =
    Try(db.transactionEither(run)).flatMap(_.fold(Failure(_), Success(_)))

  private def records(
      session: SqlSession,
      where: String,
      args: Seq[SqlParam]
  ): Either[ActionError, List[activity.View]] =
    session
      .query(
        s"SELECT id, cycle_id, event, occurred_at, journal FROM activities WHERE deleted = 0 AND $where",
        args
      )(activities.decode)
      .foldLeft[Either[ActionError, List[activity.View]]](Right(Nil)) { (acc, record) =>
        for
          rows <- acc
          row <- record.left.map(e =>
            ActionError.PersistenceFailure(
              new IllegalStateException(s"Invalid stored activity: $e")
            )
          )
        yield row :: rows
      }
      .map(
        _.sortBy(a => (a.occurredAt.getEpochSecond, a.occurredAt.getNano, a.activityId.toString))
      )

  private def lotIn(session: SqlSession, id: String): Either[ActionError, traceability.LotTrace] =
    for
      view <- lots.getIn(session, id)
      timestamp <- session
        .queryOne("SELECT harvested_at FROM harvests WHERE id = ?", Seq(view.harvestId.toString))(
          _.getString(1)
        )
        .toRight(ActionError.PersistenceFailure(new IllegalStateException("Missing harvest")))
      harvestedAt <- Try(Instant.parse(timestamp)).toEither.left
        .map(ActionError.PersistenceFailure.apply)
      all <- records(session, "cycle_id = ?", Seq(view.cycleId.toString))
      relevant = all.filter(a => !a.occurredAt.isAfter(harvestedAt))
      inputs <- relevant
        .flatMap(a => a.journal.inputs.map(a -> _))
        .foldLeft[Either[ActionError, List[traceability.TracedInput]]](Right(Nil)) {
          case (acc, (event, input)) =>
            for
              rows <- acc
              stock <- input.stock match
                case None       => Right(None)
                case Some(link) =>
                  stocks
                    .getIn(session, link.stockId.toString)
                    .left
                    .map {
                      case _: ActionError.NotFound =>
                        ActionError.PersistenceFailure(
                          new IllegalStateException("Journal input references missing stock")
                        )
                      case error => error
                    }
                    .map(Some(_))
            yield traceability.TracedInput(event, input, stock) :: rows
        }
        .map(_.reverse)
    yield traceability.LotTrace(view, harvestedAt, relevant, inputs)

  def lot(id: harvestLot.LotId): Try[Option[traceability.LotTrace]] = transact { session =>
    lotIn(session, id.toString) match
      case Left(_: ActionError.NotFound) => Right(None)
      case Left(error)                   => Left(error)
      case Right(value)                  => Right(Some(value))
  }

  def input(id: inventory.StockId): Try[Option[traceability.InputTrace]] = transact { session =>
    stocks.getIn(session, id.toString) match
      case Left(_: ActionError.NotFound)                      => Right(None)
      case Left(error)                                        => Left(error)
      case Right(stock) if stock.kind != inventory.Kind.Input => Right(None)
      case Right(stock)                                       =>
        for
          all <- records(session, "1 = 1", Nil)
          linked = all.filter(_.journal.inputs.exists(_.stock.exists(_.stockId == id)))
          impacts <- linked
            .groupBy(_.cycleId)
            .toList
            .sortBy(_._1.toString)
            .foldLeft[Either[ActionError, List[traceability.CycleImpact]]](Right(Nil)) {
              case (acc, (cycleId, events)) =>
                val lotIds = session.query(
                  "SELECT o.stock_id FROM harvest_lot_origins o WHERE o.cycle_id = ? ORDER BY o.stock_id",
                  Seq(cycleId.toString)
                )(_.getString(1))
                for
                  rows <- acc
                  traces <- lotIds
                    .foldLeft[Either[ActionError, List[traceability.LotTrace]]](Right(Nil)) {
                      (result, lotId) =>
                        for previous <- result; trace <- lotIn(session, lotId)
                        yield trace :: previous
                    }
                    .map(
                      _.reverse
                        .filter(t => events.exists(e => !e.occurredAt.isAfter(t.harvestedAt)))
                    )
                yield traceability.CycleImpact(cycleId, events, traces) :: rows
            }
            .map(_.reverse)
        yield Some(traceability.InputTrace(stock, impacts))
  }
