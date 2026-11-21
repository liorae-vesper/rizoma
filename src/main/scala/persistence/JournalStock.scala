package rizoma.persistence

import rizoma.domain.ActionError
import rizoma.domain.crop.activity.Journal
import rizoma.prelude.jdbc.SqlSession

import java.time.Instant

/** Records or reverses linked journal consumption inside the caller's transaction. */
private[persistence] object JournalStock:
  def apply(
      session: SqlSession,
      activityId: String,
      cycleId: String,
      journal: Journal,
      consume: Boolean,
      at: Instant
  ): Either[ActionError, Unit] =
    val linked = journal.inputs.flatMap(input => input.stock.map(link => (input, link)))
    if linked.isEmpty then Right(())
    else
      val farm = session.queryOne(
        "SELECT plots.farm_id FROM cycles JOIN plots ON plots.id = cycles.plot_id WHERE cycles.id = ? AND cycles.deleted = 0 AND plots.deleted = 0",
        Seq(cycleId)
      )(_.getString(1))
      farm.toRight(ActionError.InvalidReference("cycle")).flatMap { farmId =>
        linked.foldLeft[Either[ActionError, Unit]](Right(())) { case (acc, (input, link)) =>
          acc.flatMap { _ =>
            val id = link.stockId.toString
            for
              stock <- InventorySql.stock(session, id)
              _ <- Either.cond(
                stock.kind == "Input" && stock.farmId == farmId && stock.unit == input.quantity.unit && stock.name == input.name,
                (),
                ActionError.InvalidReference("journal input stock")
              )
              amount = input.quantity.amount
              _ <- InventorySql.change(
                session,
                id,
                link.location,
                if consume then InventorySql.negate(amount) else amount
              )
            yield InventorySql.movement(
              session,
              id,
              if consume then "JournalUse" else "JournalReversal",
              if consume then Some(link.location) else None,
              if consume then None else Some(link.location),
              amount,
              at,
              Some(activityId)
            )
          }
        }
      }
