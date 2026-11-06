package rizoma
package domain

object Coordinates:
  opaque type Latitude = Double
  opaque type Longitude = Double

  object Latitude:
    def from(value: Double): Option[Latitude] =
      Option.when(value.isFinite && value >= -90 && value <= 90)(value)

  object Longitude:
    def from(value: Double): Option[Longitude] =
      Option.when(value.isFinite && value >= -180 && value <= 180)(value)

final case class GeoPoint(latitude: Coordinates.Latitude, longitude: Coordinates.Longitude)

object GeoPoint:
  def from(latitude: Double, longitude: Double): Option[GeoPoint] =
    for
      lat <- Coordinates.Latitude.from(latitude)
      lon <- Coordinates.Longitude.from(longitude)
    yield GeoPoint(lat, lon)