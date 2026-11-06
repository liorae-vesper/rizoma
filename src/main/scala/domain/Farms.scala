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

/** Reasons a farm registration request is rejected. */
enum RegisterFarmError:
  /** The farm name failed [[FarmName.from]] validation. */
  case InvalidName(error: FarmName.Error)

  /** The latitude or longitude is outside its valid range or is non-finite. */
  case InvalidLocation

  /** The total area is not finite and greater than zero. */
  case InvalidArea

/** A farm with a validated name, geographic location, and positive finite total area. */
final case class RegisterFarm private (name: FarmName.Type, location: GeoPoint, totalArea: Double)

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
    * @return
    *   `InvalidLocation`, `InvalidName`, or `InvalidArea` when the corresponding value is invalid
    */
  def from(
      name: String,
      latitude: Double,
      longitude: Double,
      totalArea: Double
  ): Either[RegisterFarmError, RegisterFarm] =
    for
      point <- GeoPoint.from(latitude, longitude).toRight(RegisterFarmError.InvalidLocation)
      farmName <- FarmName.from(name).left.map(RegisterFarmError.InvalidName.apply)
      area <- Option
        .when(totalArea.isFinite && totalArea > 0)(totalArea)
        .toRight(RegisterFarmError.InvalidArea)
    yield RegisterFarm(farmName, point, area)
