package rizoma
package domain.harvest

import rizoma.domain.*

import java.time.Instant

import prelude.Id

/** Harvest quantities recorded against a crop cycle. */

opaque type HarvestId = Id

object HarvestId:
  /** Wraps a validated application identifier without checking harvest existence. */
  def from(id: Id): HarvestId = id

/** A positive, finite harvested amount with its measurement or counting unit. */
final case class Yield private (amount: Double, unitOfYield: UnitOfYield):
  /** Returns the numeric amount in its stored unit without conversion. */
  def toDouble: Double = amount

/** Units for harvest quantities. No conversion between units is implied.
  *
  * Tonnes are metric; US customary units are named explicitly to distinguish their definitions.
  */
enum UnitOfYield:
  case Grams, Kilograms, Tonnes
  case Pounds, UsShortTons
  case Liters, UsGallons, UsBushels
  case Units, Dozens

object Yield:
  enum Error:
    case NotFinite
    case NotGreaterThanZero

  /** Validates the amount without rounding or conversion, defaulting to kilograms. */
  def from(
      amount: Double,
      unitOfYield: UnitOfYield = UnitOfYield.Kilograms
  ): Either[Error, Yield] =
    if !amount.isFinite then Left(Error.NotFinite)
    else if amount <= 0 then Left(Error.NotGreaterThanZero)
    else Right(Yield(amount, unitOfYield))

enum Constraints:
  case InvalidYield(error: Yield.Error)

/** Validated registration values; cycle existence is checked by the application. */
final case class Registration private (
    cycleId: crop.cycle.CycleId,
    yieldAmount: Yield,
    harvestedAt: Instant
)

object Registration:
  /** Validates yield and retains its unit, cycle, and harvest timestamp. Defaults to kilograms.
    */
  def from(
      cycleId: crop.cycle.CycleId,
      yieldAmount: Double,
      harvestedAt: Instant,
      unitOfYield: UnitOfYield = UnitOfYield.Kilograms
  ): Either[Constraints, Registration] =
    Yield.from(yieldAmount, unitOfYield).left.map(Constraints.InvalidYield.apply).map { amount =>
      Registration(cycleId, amount, harvestedAt)
    }

/** Replacement values retaining harvest and crop cycle identities. */
final case class Patch private[harvest] (
    yieldAmount: Yield,
    harvestedAt: Instant,
    cycleId: crop.cycle.CycleId,
    harvestId: HarvestId
)

final case class Destroy(harvestId: HarvestId)

/** A harvest snapshot used to prepare commands for the same harvest and cycle. */
final case class View(
    harvestId: HarvestId,
    cycleId: crop.cycle.CycleId,
    yieldAmount: Yield,
    harvestedAt: Instant
):
  /** Replaces yield and timestamp, retaining the current unit unless a replacement is supplied.
    */
  def patch(
      yieldAmount: Double,
      harvestedAt: Instant,
      unitOfYield: UnitOfYield = this.yieldAmount.unitOfYield
  ): Either[Constraints, Patch] =
    Yield.from(yieldAmount, unitOfYield).left.map(Constraints.InvalidYield.apply).map { amount =>
      Patch(amount, harvestedAt, cycleId, harvestId)
    }
