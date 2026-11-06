package rizoma
package domain

/** Validated latitude and longitude value types. */
object Coordinates:
  /** A finite latitude in the inclusive range from -90 to 90 degrees. */
  opaque type Latitude = Double

  /** A finite longitude in the inclusive range from -180 to 180 degrees. */
  opaque type Longitude = Double

  object Latitude:
    /** Creates a latitude when `value` is finite and between -90 and 90 degrees, inclusive. */
    def from(value: Double): Option[Latitude] =
      Option.when(value.isFinite && value >= -90 && value <= 90)(value)

  object Longitude:
    /** Creates a longitude when `value` is finite and between -180 and 180 degrees, inclusive. */
    def from(value: Double): Option[Longitude] =
      Option.when(value.isFinite && value >= -180 && value <= 180)(value)

/** A geographic point whose coordinates have passed range and finiteness checks. */
final case class GeoPoint(latitude: Coordinates.Latitude, longitude: Coordinates.Longitude)

object GeoPoint:
  /** Creates a geographic point only when both coordinates are valid. */
  def from(latitude: Double, longitude: Double): Option[GeoPoint] =
    for
      lat <- Coordinates.Latitude.from(latitude)
      lon <- Coordinates.Longitude.from(longitude)
    yield GeoPoint(lat, lon)
