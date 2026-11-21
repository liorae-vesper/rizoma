package rizoma.persistence

import io.circe.parser
import rizoma.domain.{ActionError, MultiPolygon, Polygon}
import rizoma.prelude.jdbc.SqlSession

import java.time.Instant
import scala.util.Try

private[persistence] object SqliteLifecycle:
  def timestamp(value: String): Either[ActionError, Instant] =
    Try(Instant.parse(value)).toEither.left.map(ActionError.PersistenceFailure.apply)

  def harvestPeriod(
      session: SqlSession,
      cycleId: String,
      at: Instant,
      requireActive: Boolean
  ): Either[ActionError, Unit] =
    session.queryOne(
      "SELECT started_at, ended_at FROM cycles WHERE id = ? AND deleted = 0",
      Seq(cycleId)
    )(r => (Option(r.getString(1)), Option(r.getString(2)))) match
      case None                     => Left(ActionError.InvalidReference("harvest"))
      case Some((None, _))          => Left(ActionError.InvalidState("Cycle has not started"))
      case Some((Some(start), end)) =>
        for
          started <- timestamp(start)
          ended <- end match
            case None      => Right(None)
            case Some(raw) => timestamp(raw).map(Some(_))
          _ <- Either.cond(
            !at.isBefore(started) && ended.forall(!at.isAfter(_)),
            (),
            ActionError.InvalidState("Harvest is outside the cycle period")
          )
          _ <- Either.cond(
            !requireActive || ended.isEmpty,
            (),
            ActionError.InvalidState("Cycle is already finished")
          )
        yield ()

  def harvestsWithin(
      session: SqlSession,
      cycleId: String,
      started: Option[Instant],
      ended: Option[Instant]
  ): Either[ActionError, Unit] =
    session
      .query("SELECT harvested_at FROM harvests WHERE cycle_id = ? AND deleted = 0", Seq(cycleId))(
        _.getString(1)
      )
      .foldLeft[Either[ActionError, Unit]](Right(())) { (result, raw) =>
        for
          _ <- result
          at <- timestamp(raw)
          _ <- Either.cond(
            started.exists(!at.isBefore(_)) && ended.forall(!at.isAfter(_)),
            (),
            ActionError.InvalidState("Cycle period excludes an existing harvest")
          )
        yield ()
      }

  def fits(boundary: MultiPolygon, land: Polygon): Either[ActionError, Unit] =
    if !boundary.isValid || !land.isValid then
      Left(ActionError.InvalidState("Invalid boundary or occupied land topology"))
    else if boundary.polygons.forall(_.vertices.isEmpty) || land.vertices.isEmpty || boundary
        .contains(land)
    then Right(())
    else Left(ActionError.InvalidState("Occupied land lies outside the plot boundary"))

  def plotBoundary(session: SqlSession, id: String): Either[ActionError, MultiPolygon] =
    session
      .queryOne("SELECT geometry FROM plots WHERE id = ? AND deleted = 0", Seq(id))(_.getString(1))
      .toRight(ActionError.InvalidReference("cycle"))
      .flatMap(raw =>
        parser.decode[MultiPolygon](raw).left.map(e => ActionError.PersistenceFailure(e))
      )

  def checkBoundary(
      session: SqlSession,
      id: String,
      boundary: MultiPolygon
  ): Either[ActionError, Unit] =
    if !boundary.isValid then Left(ActionError.InvalidState("Invalid plot boundary topology"))
    else
      session
        .query(
          "SELECT occupied_land FROM cycles WHERE plot_id = ? AND deleted = 0 AND ended_at IS NULL",
          Seq(id)
        )(_.getString(1))
        .foldLeft[Either[ActionError, Unit]](Right(())) { (result, raw) =>
          for
            _ <- result
            land <- parser.decode[Polygon](raw).left.map(e => ActionError.PersistenceFailure(e))
            _ <- fits(boundary, land)
          yield ()
        }
