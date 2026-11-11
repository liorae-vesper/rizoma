package rizoma
package domain


/** Names accepted for farms. */
object FarmName:
  opaque type Type = String

  /** Reasons a farm name cannot be created. */
  enum Error:
    /** The input is empty after trimming whitespace. */
    case Empty

    /** The trimmed name exceeds the supplied maximum length. */
    case TooLong(max: Int)

  private val maxLength = 100

  /** Trims and validates a farm name.
    *
    * @return
    *   `Empty` for blank input or `TooLong(100)` when the trimmed value exceeds 100 characters.
    */
  def from(value: String): Either[Error, Type] =
    val name = value.trim
    name match
      case _ if name.isEmpty            => Left(Error.Empty)
      case _ if name.length > maxLength => Left(Error.TooLong(maxLength))
      case _                            => Right(name)

/** Units used to express a farm's area. */
enum UnitOfArea:
  /** Area measured in square meters. */
  case SquareMeters
  /** Area measured in hectares. */
  case Hectares
  /** Area measured in acres. */
  case Acres

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
  def from(totalArea: Double, unitOfArea: UnitOfArea = UnitOfArea.SquareMeters): Either[AreaError, Area] =
    totalArea match
      case _ if !totalArea.isFinite => Left(AreaError.AreaNotFinite)
      case _ if totalArea <= 0 => Left(AreaError.AreaNotGreaterThanZero)
      case _ => Right(Area(totalArea, unitOfArea))


/** Reasons a farm registration request is rejected. */
enum RegisterFarmError:
  /** The farm name failed [[FarmName.from]] validation. */
  case InvalidName(error: FarmName.Error)

  /** The latitude or longitude is outside its valid range or is non-finite. */
  case InvalidLocation

  /** The total area is not finite and greater than zero. */
  case InvalidArea(error: AreaError)

/** A farm with a validated name, geographic location, and area. */
final case class RegisterFarm private (
    name: FarmName.Type,
    location: GeoPoint,
    area: Area
)

object RegisterFarm:
  /** Creates a farm after validating its coordinates, name, and area.
    *
    * @param name
    *   the farm name; leading and trailing whitespace is removed
    * @param latitude
    *   latitude in the inclusive range -90 to 90 degrees
    * @param longitude
    *   longitude in the inclusive range -180 to 180 degrees
    * @param totalArea
    *   the farm area, which must be finite and greater than zero
    * @param unitOfArea
    *   the unit in which the area was measured
    * @return
    *   `InvalidLocation`, `InvalidName`, or `InvalidArea` when the corresponding value is invalid
    */
  def from(
      name: String,
      latitude: Double,
      longitude: Double,
      totalArea: Double,
      unitOfArea: UnitOfArea
  ): Either[RegisterFarmError, RegisterFarm] =
    for
      point <- GeoPoint.from(latitude, longitude).toRight(RegisterFarmError.InvalidLocation)
      farmName <- FarmName.from(name).left.map(RegisterFarmError.InvalidName.apply)
      area <- Area.from(totalArea, unitOfArea).left.map(RegisterFarmError.InvalidArea.apply)
    yield RegisterFarm(farmName, point, area)

final case class Farm(id: Id, name: FarmName.Type, location: GeoPoint, totalArea: Area)
