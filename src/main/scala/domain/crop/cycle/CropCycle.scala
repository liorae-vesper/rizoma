package rizoma
package domain.crop.cycle

import rizoma.domain.*
import rizoma.prelude.Id

import java.time.Instant

/** A species grown on an occupied polygon within a plot, with optional start and end timestamps. */

opaque type CycleId = Id

object CycleId:
  /** Wraps a validated application identifier without checking cycle existence. */
  def from(id: Id): CycleId = id

/** Derived from the presence of validated start and end timestamps. */
enum Status:
  case Planned, Active, Completed

enum Constraints:
  case EndWithoutStart
  case EndBeforeStart
  case CannotClearStart
  case CannotClearEnd

/** Validated timestamps: an end requires a start and cannot precede it. */
final case class Period private (startedAt: Option[Instant], endedAt: Option[Instant]):
  def status: Status =
    (startedAt, endedAt) match
      case (None, _)       => Status.Planned
      case (Some(_), None) => Status.Active
      case _               => Status.Completed

object Period:
  /** Accepts a planned, active, or completed period. Equal timestamps are allowed. */
  def from(startedAt: Option[Instant], endedAt: Option[Instant]): Either[Constraints, Period] =
    (startedAt, endedAt) match
      case (None, Some(_))                                 => Left(Constraints.EndWithoutStart)
      case (Some(start), Some(end)) if end.isBefore(start) => Left(Constraints.EndBeforeStart)
      case _                                               => Right(Period(startedAt, endedAt))

/** Registration values. Polygon validity is owned by Polygon; containment is not checked here. */
final case class Registration private (
    speciesId: crop.species.SpeciesId,
    plotId: plot.PlotId,
    occupiedLand: Polygon,
    period: Period
)

object Registration:
  /** Validates timestamps, defaulting to a planned cycle, and retains the supplied references. */
  def from(
      speciesId: crop.species.SpeciesId,
      plotId: plot.PlotId,
      occupiedLand: Polygon,
      startedAt: Option[Instant] = None,
      endedAt: Option[Instant] = None
  ): Either[Constraints, Registration] =
    Period.from(startedAt, endedAt).map(Registration(speciesId, plotId, occupiedLand, _))

/** Editable cycle values, retaining the existing plot and cycle identities. */
final case class Patch private[cycle] (
    speciesId: crop.species.SpeciesId,
    occupiedLand: Polygon,
    period: Period,
    plotId: plot.PlotId,
    cycleId: CycleId
)

final case class Destroy(cycleId: CycleId)

/** A cycle snapshot with a period validated by Period.from. */
final case class View(
    cycleId: CycleId,
    speciesId: crop.species.SpeciesId,
    plotId: plot.PlotId,
    occupiedLand: Polygon,
    period: Period
):
  /** Replaces editable values, preserving timestamps by default.
    *
    * A started cycle cannot return to planned, and a completed cycle cannot be reopened. Existing
    * timestamps may be corrected as long as chronological order is retained.
    */
  def patch(
      speciesId: crop.species.SpeciesId,
      occupiedLand: Polygon,
      startedAt: Option[Instant] = period.startedAt,
      endedAt: Option[Instant] = period.endedAt
  ): Either[Constraints, Patch] =
    if period.startedAt.isDefined && startedAt.isEmpty then Left(Constraints.CannotClearStart)
    else if period.endedAt.isDefined && endedAt.isEmpty then Left(Constraints.CannotClearEnd)
    else
      Period.from(startedAt, endedAt).map { replacement =>
        Patch(speciesId, occupiedLand, replacement, plotId, cycleId)
      }
