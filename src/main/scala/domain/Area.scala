package rizoma
package domain

/** Units used to express an area. */
enum UnitOfArea:
  /** Area measured in square meters. */
  case SquareMeters

  /** Area measured in hectares. */
  case Hectares

  /** Area measured in acres. */
  case Acres

  def asString: String =
    this match
      case UnitOfArea.SquareMeters => "square_meters"
      case UnitOfArea.Hectares     => "hectares"
      case UnitOfArea.Acres        => "acres"

object UnitOfArea:
  def from(raw: String): Option[UnitOfArea] =
    raw match
      case "square_meters" => Some(SquareMeters)
      case "hectares"      => Some(Hectares)
      case "acres"         => Some(Acres)
      case _               => None

/** Reasons an area measurement is invalid. */
enum AreaError:
  /** The area value is not finite. */
  case AreaNotFinite

  /** The area value is zero or negative. */
  case AreaNotGreaterThanZero

/** A positive, finite area measurement and its unit. */
final case class Area private (totalArea: Double, unitOfArea: UnitOfArea)
object Area:
  /** Creates an area when `totalArea` is finite and greater than zero.
    *
    * @param totalArea
    *   the measured area, which must be finite and positive
    * @param unitOfArea
    *   the unit of measurement; defaults to square meters
    * @return
    *   the validated area, or the reason the value is invalid
    */
  def from(
      totalArea: Double,
      unitOfArea: UnitOfArea = UnitOfArea.SquareMeters
  ): Either[AreaError, Area] =
    totalArea match
      case _ if !totalArea.isFinite => Left(AreaError.AreaNotFinite)
      case _ if totalArea <= 0      => Left(AreaError.AreaNotGreaterThanZero)
      case _                        => Right(Area(totalArea, unitOfArea))
