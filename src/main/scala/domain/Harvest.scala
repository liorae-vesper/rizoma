package rizoma
package domain

import java.time.Instant

/** Units for harvest quantities. No conversion between units is implied.
  *
  * Bags, boxes, crates, bunches, and bales count packages without assuming a fixed size or weight.
  * Tonnes are metric; US customary units are named explicitly to distinguish their definitions.
  */
enum UnitOfYield:
  case Grams, Kilograms, Tonnes
  case Pounds, UsShortTons
  case Liters, UsGallons, UsBushels
  case Units, Dozens
  case Bunches, Boxes, Crates, Bags, Bales

/** Harvest quantities recorded against a crop cycle. */
object Harvest:
  opaque type HarvestId = Id

  object HarvestId:
    /** Wraps a validated application identifier without checking harvest existence. */
    def from(id: Id): HarvestId = id

  /** A positive, finite harvested amount with its measurement or counting unit. */
  final case class Yield private (amount: Double, unitOfYield: UnitOfYield):
    /** Returns the numeric amount in its stored unit without conversion. */
    def toDouble: Double = amount

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
  final case class Register private (
      cycleId: CropCycle.CycleId,
      yieldAmount: Yield,
      harvestedAt: Instant
  )

  object Register:
    /** Validates yield and retains its unit, cycle, and harvest timestamp. Defaults to kilograms.
      */
    def from(
        cycleId: CropCycle.CycleId,
        yieldAmount: Double,
        harvestedAt: Instant,
        unitOfYield: UnitOfYield = UnitOfYield.Kilograms
    ): Either[Constraints, Register] =
      Yield.from(yieldAmount, unitOfYield).left.map(Constraints.InvalidYield.apply).map { amount =>
        Register(cycleId, amount, harvestedAt)
      }

  /** Replacement values retaining harvest and crop cycle identities. */
  final case class Patch private[Harvest] (
      yieldAmount: Yield,
      harvestedAt: Instant,
      cycleId: CropCycle.CycleId,
      harvestId: HarvestId
  )

  final case class Destroy(harvestId: HarvestId)

  /** A harvest snapshot used to prepare commands for the same harvest and cycle. */
  final case class View(
      harvestId: HarvestId,
      cycleId: CropCycle.CycleId,
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
