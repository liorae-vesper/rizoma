package rizoma.persistence

import rizoma.domain.{ActionError, harvest}
import rizoma.prelude.Id
import rizoma.prelude.jdbc.Database

import scala.util.{Failure, Success, Try}

final class SqliteHarvests(db: Database)
    extends SqliteWrites(db, "harvest", "harvests")
    with harvest.Store:
  def initialize(): Unit = db.transaction(session => initialize(session))
  private[persistence] def initialize(session: rizoma.prelude.jdbc.SqlSession): Unit = schema(
    session,
    """CREATE TABLE IF NOT EXISTS harvests (
    id TEXT PRIMARY KEY, cycle_id TEXT NOT NULL REFERENCES cycles(id),
    amount REAL NOT NULL, unit_of_yield TEXT NOT NULL, harvested_at TEXT NOT NULL
  )"""
  )
  private def transact[A](run: rizoma.prelude.jdbc.SqlSession => Either[ActionError, A]): Try[A] =
    Try(db.transactionEither(run)).flatMap(_.fold(Failure(_), Success(_)))

  def insert(id: Id, input: harvest.Registration): Try[Unit] = transact { session =>
    val locked = session.execute(
      "UPDATE cycles SET id = id WHERE id = ? AND deleted = 0",
      Seq(input.cycleId.toString)
    )
    for
      _ <- Either.cond(locked == 1, (), ActionError.InvalidReference("harvest"))
      _ <- SqliteLifecycle.harvestPeriod(session, input.cycleId.toString, input.harvestedAt, true)
    yield
      val _ = session.execute(
        "INSERT INTO harvests (id, cycle_id, amount, unit_of_yield, harvested_at) VALUES (?, ?, ?, ?, ?)",
        Seq(
          id.value,
          input.cycleId.toString,
          input.yieldAmount.amount,
          input.yieldAmount.unitOfYield.toString,
          input.harvestedAt.toString
        )
      )
  }

  def update(input: harvest.Patch): Try[Unit] = transact { session =>
    val locked = session.execute(
      "UPDATE harvests SET id = id WHERE id = ? AND deleted = 0",
      Seq(input.harvestId.toString)
    )
    for
      _ <- Either.cond(locked == 1, (), ActionError.NotFound("harvest", input.harvestId.toString))
      _ <- SqliteLifecycle.harvestPeriod(session, input.cycleId.toString, input.harvestedAt, false)
      count = session.execute(
        "UPDATE harvests SET amount = ?, unit_of_yield = ?, harvested_at = ? WHERE id = ? AND cycle_id = ? AND deleted = 0 AND NOT EXISTS (SELECT 1 FROM stock_items WHERE harvest_id = harvests.id)",
        Seq(
          input.yieldAmount.amount,
          input.yieldAmount.unitOfYield.toString,
          input.harvestedAt.toString,
          input.harvestId.toString,
          input.cycleId.toString
        )
      )
      _ <- Either.cond(
        count == 1,
        (),
        ActionError.HasDependents("harvest", input.harvestId.toString)
      )
    yield ()
  }
  def delete(input: harvest.Delete): Try[Unit] =
    writeExisting(
      "UPDATE harvests SET deleted = 1 WHERE id = ? AND deleted = 0 AND NOT EXISTS (SELECT 1 FROM stock_items WHERE harvest_id = harvests.id)",
      Seq(input.harvestId.toString),
      input.harvestId.toString,
      _ => ActionError.HasDependents("harvest", input.harvestId.toString)
    )
