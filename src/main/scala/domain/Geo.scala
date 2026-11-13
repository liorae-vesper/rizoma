package rizoma
package domain

import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/** Validated latitude and longitude value types. */
object Coordinates:
  /** A finite latitude in the inclusive range from -90 to 90 degrees. */
  opaque type Latitude = Double

  /** A finite longitude in the inclusive range from -180 to 180 degrees. */
  opaque type Longitude = Double

  extension (latitude: Latitude)
    /** Returns the latitude in decimal degrees. */
    def toDouble: Double = latitude

  extension (longitude: Longitude)
    /** Returns the longitude in decimal degrees. */
    @scala.annotation.targetName("longitudeToDouble")
    def toDouble: Double = longitude

  object Latitude:
    /** Creates a latitude when `value` is finite and between -90 and 90 degrees, inclusive. */
    def from(value: Double): Option[Latitude] =
      Option.when(value.isFinite && value >= -90 && value <= 90)(value)

  object Longitude:
    /** Creates a longitude when `value` is finite and between -180 and 180 degrees, inclusive. */
    def from(value: Double): Option[Longitude] =
      Option.when(value.isFinite && value >= -180 && value <= 180)(value)

/** A geographic position used inside geometry coordinate arrays.
  *
  * Use `Position.from` to validate coordinates and optional altitude together.
  *
  * @param latitude
  *   latitude in decimal degrees, between -90 and 90 inclusive
  * @param longitude
  *   longitude in decimal degrees, between -180 and 180 inclusive
  * @param altitude
  *   optional altitude in meters; the factory requires a finite value
  */
final case class Position private (
    latitude: Coordinates.Latitude,
    longitude: Coordinates.Longitude,
    altitude: Option[Double]
)

object Position:
  /** Creates a geographic point only when both coordinates are valid.
    *
    * @return
    *   the position without altitude, or `None` for out-of-range or non-finite coordinates
    */
  def from(latitude: Double, longitude: Double): Option[Position] =
    from(latitude, longitude, None)

  /** Creates a position with an optional finite altitude in meters.
    *
    * @return
    *   the position, or `None` for invalid coordinates or a non-finite altitude
    */
  def from(latitude: Double, longitude: Double, altitude: Option[Double]): Option[Position] =
    for
      lat <- Coordinates.Latitude.from(latitude)
      lon <- Coordinates.Longitude.from(longitude)
      _ <- Option.when(altitude.forall(_.isFinite))(())
    yield new Position(lat, lon, altitude)

  /** GeoJSON positions use longitude, latitude, and optional altitude. */
  given Encoder[Position] = Encoder.instance { point =>
    Encoder[List[Double]].apply(
      List(point.longitude.toDouble, point.latitude.toDouble) ++ point.altitude.toList
    )
  }

  /** Decodes exactly two or three numeric ordinates in longitude, latitude, altitude order. Rejects
    * numeric strings, non-finite values, and coordinates outside the domain ranges.
    */
  given Decoder[Position] = Decoder[List[Json]].emap { values =>
    values
      .foldRight[Either[String, List[Double]]](Right(Nil)) { (value, rest) =>
        for
          number <- value.asNumber.map(_.toDouble).toRight("Position ordinates must be numbers")
          tail <- rest
        yield number :: tail
      }
      .flatMap {
        case longitude :: latitude :: Nil =>
          from(latitude, longitude).toRight("Invalid position")
        case longitude :: latitude :: altitude :: Nil =>
          from(latitude, longitude, Some(altitude)).toRight("Invalid position")
        case _ => Left("A position requires longitude, latitude, and optional altitude")
      }
  }

/** The seven GeoJSON geometry objects.
  *
  * Companion codecs encode the GeoJSON `type` discriminator and coordinate arrays. Geometry
  * constructors are private; companion `from` factories validate inputs and return `None` when
  * construction fails. Decoders use the same factories. Encoders preserve validated values.
  */
sealed trait Geometry

/** A single position, encoded as a GeoJSON Point.
  *
  * @param coordinates
  *   the position represented by this geometry
  */
final case class Point private (coordinates: Position) extends Geometry
object Point:
  /** Wraps a validated position as a Point geometry. */
  def from(coordinates: Position): Option[Point] = Some(new Point(coordinates))

  /** Encodes a Point object with a single position array. */
  given Encoder[Point] = Geometry.encoder("Point", _.coordinates)

  /** Requires the Point discriminator and a valid position. */
  given Decoder[Point] = Geometry.decoder("Point")(Point.from)

/** A collection of positions, which may be empty.
  *
  * @param coordinates
  *   the positions in their original order
  */
final case class MultiPoint private (coordinates: List[Position]) extends Geometry
object MultiPoint:
  /** Creates a collection of validated positions; accepts an empty collection. */
  def from(coordinates: List[Position]): Option[MultiPoint] = Some(new MultiPoint(coordinates))

  /** Encodes a MultiPoint object with an array of positions. */
  given Encoder[MultiPoint] = Geometry.encoder("MultiPoint", _.coordinates)

  /** Requires the MultiPoint discriminator and valid positions; accepts an empty array. */
  given Decoder[MultiPoint] = Geometry.decoder("MultiPoint")(MultiPoint.from)

/** An ordered sequence of positions connected by line segments.
  *
  * @param coordinates
  *   the positions; construction requires at least two
  */
final case class LineString private (coordinates: List[Position]) extends Geometry
object LineString:
  /** Creates a line with at least two positions; returns `None` for shorter input. */
  def from(coordinates: List[Position]): Option[LineString] =
    Option.when(coordinates.size >= 2)(new LineString(coordinates))

  /** Encodes the validated positions of a LineString object. */
  given Encoder[LineString] = Geometry.encoder("LineString", _.coordinates)

  /** Requires the LineString discriminator and at least two valid positions. */
  given Decoder[LineString] =
    Geometry.decoder[LineString, List[Position]]("LineString")(LineString.from)

/** A collection of line strings, which may be empty.
  *
  * @param coordinates
  *   the lines; construction requires at least two positions in each line
  */
final case class MultiLineString private (coordinates: List[List[Position]]) extends Geometry
object MultiLineString:
  /** Creates a collection when every line has at least two positions; accepts no lines. */
  def from(coordinates: List[List[Position]]): Option[MultiLineString] =
    Option.when(coordinates.forall(_.size >= 2))(new MultiLineString(coordinates))

  /** Encodes a MultiLineString object with an array of line coordinate arrays. */
  given Encoder[MultiLineString] = Geometry.encoder("MultiLineString", _.coordinates)

  /** Validates every line length and position; accepts an empty collection. */
  given Decoder[MultiLineString] =
    Geometry.decoder[MultiLineString, List[List[Position]]]("MultiLineString")(MultiLineString.from)

/** Exterior ring followed by optional interior rings (holes).
  *
  * Construction requires each ring to contain at least four positions with identical first and last
  * positions, including altitude. Ring orientation, self-intersections, and containment of holes
  * are not checked. An empty list of rings is accepted as an empty polygon.
  *
  * @param vertices
  *   the exterior ring
  * @param holes
  *   the interior rings, in their original order
  */
final case class Polygon private (vertices: List[Position], holes: List[List[Position]])
    extends Geometry:
  /** Returns exterior and interior rings in GeoJSON order, or `Nil` when both are empty. */
  def coordinates: List[List[Position]] =
    if vertices.isEmpty && holes.isEmpty then Nil else vertices :: holes

object Polygon:
  /** Creates a polygon with closed rings of at least four positions. Returns `None` for an invalid
    * exterior or hole. Empty exterior and holes form an empty polygon.
    */
  def from(vertices: List[Position], holes: List[List[Position]] = Nil): Option[Polygon] =
    val rings = if vertices.isEmpty && holes.isEmpty then Nil else vertices :: holes
    fromRings(rings)

  /** Checks ring length and closure; accepts an empty collection of rings. */
  private def validRings(rings: List[List[Position]]): Boolean =
    rings.forall(ring => ring.size >= 4 && ring.head == ring.last)

  /** Validates GeoJSON rings before splitting exterior and holes. */
  private[domain] def fromRings(rings: List[List[Position]]): Option[Polygon] =
    Option.when(validRings(rings))(new Polygon(rings.headOption.getOrElse(Nil), rings.drop(1)))

  /** Encodes a Polygon object with exterior ring first, followed by holes. */
  given Encoder[Polygon] = Geometry.encoder("Polygon", _.coordinates)

  /** Validates ring length, closure, and positions before constructing the polygon. */
  given Decoder[Polygon] =
    Geometry.decoder[Polygon, List[List[Position]]]("Polygon")(fromRings)

/** A collection of polygons, which may be empty.
  *
  * @param polygons
  *   polygons whose rings are encoded as nested coordinate arrays
  */
final case class MultiPolygon private (polygons: List[Polygon]) extends Geometry
object MultiPolygon:
  /** Creates a collection of validated polygons; accepts an empty collection. */
  def from(polygons: List[Polygon]): Option[MultiPolygon] = Some(new MultiPolygon(polygons))

  /** Encodes polygon coordinate arrays without nested Polygon objects. */
  given Encoder[MultiPolygon] = Geometry.encoder("MultiPolygon", _.polygons.map(_.coordinates))

  /** Validates every polygon ring and position; accepts an empty collection. */
  given Decoder[MultiPolygon] =
    Geometry.decoder[MultiPolygon, List[List[List[Position]]]](
      "MultiPolygon"
    ) { rings =>
      rings
        .foldRight[Option[List[Polygon]]](Some(Nil)) { (polygon, rest) =>
          for
            validated <- Polygon.fromRings(polygon)
            tail <- rest
          yield validated :: tail
        }
        .flatMap(from)
    }

/** A possibly empty collection of geometries, including nested collections.
  *
  * @param geometries
  *   geometries encoded under `geometries` rather than `coordinates`
  */
final case class GeometryCollection private (geometries: List[Geometry]) extends Geometry
object GeometryCollection:
  /** Creates a collection of validated geometries; accepts an empty collection. */
  def from(geometries: List[Geometry]): Option[GeometryCollection] =
    Some(new GeometryCollection(geometries))

  /** Encodes each geometry recursively under the `geometries` member. */
  given Encoder[GeometryCollection] =
    Geometry.encoder("GeometryCollection", _.geometries, "geometries")

  /** Decodes and validates each member recursively; accepts an empty collection. */
  given Decoder[GeometryCollection] =
    Geometry.decoder[GeometryCollection, List[Geometry]]("GeometryCollection", "geometries")(
      GeometryCollection.from
    )

/** Shared codecs and dispatch for values whose concrete geometry type is not known statically. */
object Geometry:
  /** Builds a geometry object using the supplied discriminator and payload member. */
  private[domain] def encoder[A, B: Encoder](
      kind: String,
      coordinates: A => B,
      member: String = "coordinates"
  ): Encoder[A] = Encoder.instance { value =>
    Json.obj("type" -> Json.fromString(kind), member -> Encoder[B].apply(coordinates(value)))
  }

  /** Requires the expected discriminator, decodes the payload, then validates before construction.
    * Unrelated JSON members are ignored.
    */
  private[domain] def decoder[A, B: Decoder](
      kind: String,
      member: String = "coordinates"
  )(create: B => Option[A]): Decoder[A] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap { actual =>
      if actual == kind then
        cursor.get[B](member).flatMap { value =>
          create(value).toRight(DecodingFailure(s"Invalid $kind coordinates", cursor.history))
        }
      else Left(DecodingFailure(s"Expected $kind geometry", cursor.history))
    }
  }

  /** Dispatches to the encoder for the concrete geometry type. */
  given Encoder[Geometry] = Encoder.instance {
    case value: Point              => Encoder[Point].apply(value)
    case value: MultiPoint         => Encoder[MultiPoint].apply(value)
    case value: LineString         => Encoder[LineString].apply(value)
    case value: MultiLineString    => Encoder[MultiLineString].apply(value)
    case value: Polygon            => Encoder[Polygon].apply(value)
    case value: MultiPolygon       => Encoder[MultiPolygon].apply(value)
    case value: GeometryCollection => Encoder[GeometryCollection].apply(value)
  }

  /** Dispatches using the required `type` member; rejects unknown geometry types. */
  given Decoder[Geometry] = Decoder.instance { cursor =>
    cursor.get[String]("type").flatMap {
      case "Point"              => cursor.as[Point]
      case "MultiPoint"         => cursor.as[MultiPoint]
      case "LineString"         => cursor.as[LineString]
      case "MultiLineString"    => cursor.as[MultiLineString]
      case "Polygon"            => cursor.as[Polygon]
      case "MultiPolygon"       => cursor.as[MultiPolygon]
      case "GeometryCollection" => cursor.as[GeometryCollection]
      case kind => Left(DecodingFailure(s"Unknown geometry type: $kind", cursor.history))
    }
  }
