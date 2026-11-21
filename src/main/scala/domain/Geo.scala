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
  * Components follow GeoJSON order: longitude, latitude, and optional altitude. Use `Position.from`
  * to validate them together; named arguments help avoid swapping the two horizontal ordinates. The
  * constructor and `copy` cannot bypass validation. Circe encodes a position as a two-element
  * longitude-latitude array, or a three-element array with altitude, and decodes the same arrays.
  *
  * @param longitude
  *   longitude in decimal degrees, between -180 and 180 inclusive
  * @param latitude
  *   latitude in decimal degrees, between -90 and 90 inclusive
  * @param altitude
  *   optional altitude in meters; the factory requires a finite value
  */
final case class Position private (
    longitude: Coordinates.Longitude,
    latitude: Coordinates.Latitude,
    altitude: Option[Double]
)

object Position:
  /** Creates a geographic point from longitude and latitude, in GeoJSON order.
    *
    * @param longitude
    *   finite longitude in decimal degrees, between -180 and 180 inclusive
    * @param latitude
    *   finite latitude in decimal degrees, between -90 and 90 inclusive
    * @return
    *   the position without altitude, or `None` for out-of-range or non-finite coordinates
    */
  def from(longitude: Double, latitude: Double): Option[Position] =
    from(longitude = longitude, latitude = latitude, altitude = None)

  /** Creates a position from longitude, latitude, and optional altitude, in GeoJSON order.
    *
    * @param longitude
    *   finite longitude in decimal degrees, between -180 and 180 inclusive
    * @param latitude
    *   finite latitude in decimal degrees, between -90 and 90 inclusive
    * @param altitude
    *   optional finite altitude in meters
    * @return
    *   the position, or `None` for invalid coordinates or a non-finite altitude
    */
  def from(longitude: Double, latitude: Double, altitude: Option[Double]): Option[Position] =
    for
      lon <- Coordinates.Longitude.from(longitude)
      lat <- Coordinates.Latitude.from(latitude)
      _ <- Option.when(altitude.forall(_.isFinite))(())
    yield new Position(longitude = lon, latitude = lat, altitude = altitude)

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
          from(longitude = longitude, latitude = latitude).toRight("Invalid position")
        case longitude :: latitude :: altitude :: Nil =>
          from(longitude = longitude, latitude = latitude, altitude = Some(altitude))
            .toRight("Invalid position")
        case _ => Left("A position requires longitude, latitude, and optional altitude")
      }
  }

/** The seven GeoJSON geometry objects.
  *
  * Companion codecs encode the GeoJSON `type` discriminator and coordinate arrays. Geometry
  * constructors are private; companion `from` factories validate inputs and return `None` when
  * construction fails. `copy` cannot bypass validation. Decoders use the same factories and reject
  * missing or malformed payloads, unknown types, and mismatched concrete types. Encoders preserve
  * validated values; concrete and `Geometry` codecs support round trips. Collections use the
  * `geometries` member, while other types use `coordinates`.
  *
  * Spatial predicates treat longitude and latitude as Cartesian coordinates, ignoring altitude.
  * They do not wrap the antimeridian or compute geodesic relationships. Polygon topology is not
  * validated here; predicate results are only guaranteed for topologically valid input. A
  * zero-length line is treated as a point. Polygon holes exclude their interiors, and gaps between
  * multipolygon members remain outside. Nested, mixed geometry collections are treated as unions;
  * multiple members can together contain a geometry even if no single member does.
  */
sealed trait Geometry:
  /** Tests whether the geometry is topologically valid in the planar coordinate model. */
  final def isValid: Boolean = GeometryTopology.isValid(this)

  /** Tests whether every point of `other` belongs to this geometry and their interiors intersect.
    *
    * This relation is directional. A point or line lying entirely on a polygon boundary is not
    * contained, but a line whose endpoints are on the boundary and whose interior is inside can be.
    * An open line contains points in its interior but not its endpoints. Entire segments and areas
    * are checked: endpoints inside a concave polygon do not suffice if the connecting segment
    * leaves it, and a polygon spanning a hole is not contained. Partial overlap alone is not
    * containment.
    *
    * Every component of `other` must be covered; for example, a multipoint with an outside point is
    * not contained. Collection members can jointly cover the target, including its point
    * components. A nonempty geometry contains itself; either empty operand yields false.
    */
  final def contains(other: Geometry): Boolean = GeometryTopology.contains(this, other)

  /** Tests whether the geometries share any point, including edges and vertices.
    *
    * This symmetric relation includes containment, overlapping polygons, crossing lines, and a
    * point or line on a polygon boundary, including a hole boundary. A geometry wholly inside a
    * hole without touching its boundary does not intersect the polygon. Overlapping bounding boxes
    * alone are insufficient.
    *
    * Any shared point in a multi-geometry or collection suffices; gaps remain outside. A nonempty
    * geometry intersects itself. Returns false if either geometry is empty.
    */
  final def intersects(other: Geometry): Boolean = GeometryTopology.intersects(this, other)

  /** Tests whether the geometries touch without their interiors intersecting.
    *
    * Shared edges and vertices count, as do a point or line on a polygon boundary, including a hole
    * boundary. Interior overlap, containment, disjointness, and equal points do not count. A
    * collection that touches in one place but also overlaps the other's interior is not a neighbor.
    * Returns false for empty geometries. Collections are evaluated as unions. This relation is
    * symmetric.
    */
  final def isNeighbor(other: Geometry): Boolean = GeometryTopology.isNeighbor(this, other)

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
  /** Creates a line with at least two positions; returns `None` for shorter input. Repeated
    * positions are accepted, including a line whose two endpoints are identical.
    */
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
    * exterior or hole, including an empty exterior with nonempty holes. Empty exterior and holes
    * form an empty polygon; a single empty ring is not a valid polygon ring.
    */
  def from(vertices: List[Position], holes: List[List[Position]] = Nil): Option[Polygon] =
    val rings = if vertices.isEmpty && holes.isEmpty then Nil else vertices :: holes
    fromRings(rings)

  /** Checks ring length and closure; accepts an empty collection of rings. */
  private def validRings(rings: List[List[Position]]): Boolean =
    rings.forall(ring => ring.size >= 4 && ring.headOption == ring.lastOption)

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
