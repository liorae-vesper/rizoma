package rizoma
package domain

object FarmName:
  opaque type Type = String

  enum Error:
    case Empty
    case TooLong(max: Int)

  private val maxLength = 100

  def from(value: String): Either[Error, Type] =
    val name = value.trim
    name match
      case _ if name.isEmpty => Left(Error.Empty)
      case _ if name.length > maxLength => Left(Error.TooLong(maxLength))
      case _ => Right(name)


enum RegisterFarmError:
  case InvalidName(error: FarmName.Error)
  case InvalidLocation
  case InvalidArea


final case class RegisterFarm private(name: FarmName.Type, location: GeoPoint, totalArea: Double)

object RegisterFarm:
  def from(name: String, latitude: Double, longitude: Double, totalArea: Double): Either[RegisterFarmError, RegisterFarm] =
    for
      point <- GeoPoint.from(latitude, longitude).toRight(RegisterFarmError.InvalidLocation)
      farmName <- FarmName.from(name).left.map(RegisterFarmError.InvalidName.apply)
    yield RegisterFarm(farmName, point, totalArea)



