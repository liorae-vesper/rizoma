package rizoma
package domain

import org.locationtech.jts.geom as jts
import org.locationtech.jts.operation.relateng.{RelateNG, RelatePredicate}

/** Keeps mutable JTS geometry values behind the immutable domain interface. */
private object GeometryTopology:
  private val factory = new jts.GeometryFactory()

  def isValid(geometry: Geometry): Boolean = toJts(geometry).isValid

  def contains(left: Geometry, right: Geometry): Boolean =
    val target = toJts(right)
    RelateNG.relate(containmentSource(left, target), target, RelatePredicate.contains())

  def intersects(left: Geometry, right: Geometry): Boolean =
    RelateNG.relate(toJts(left), toJts(right), RelatePredicate.intersects())

  def isNeighbor(left: Geometry, right: Geometry): Boolean =
    RelateNG.relate(toJts(left), toJts(right), RelatePredicate.touches())

  private def coordinate(position: Position): jts.Coordinate =
    new jts.Coordinate(position.longitude.toDouble, position.latitude.toDouble)

  private def coordinates(positions: List[Position]): Array[jts.Coordinate] =
    positions.map(coordinate).toArray

  private def polygon(value: Polygon): jts.Polygon =
    factory.createPolygon(
      factory.createLinearRing(coordinates(value.vertices)),
      value.holes.map(ring => factory.createLinearRing(coordinates(ring))).toArray
    )

  // JTS 1.20.0 can incorrectly reject collection containment when a source point is outside the
  // target. Such points cannot contribute to containment, so omit them from the source only.
  private def containmentSource(geometry: Geometry, target: jts.Geometry): jts.Geometry =
    geometry match
      case value: Point =>
        val point = factory.createPoint(coordinate(value.coordinates))
        if RelateNG.relate(point, target, RelatePredicate.intersects()) then point
        else factory.createPoint()
      case value: MultiPoint =>
        factory.createMultiPoint(
          value.coordinates
            .map(position => factory.createPoint(coordinate(position)))
            .filter(point => RelateNG.relate(point, target, RelatePredicate.intersects()))
            .toArray
        )
      case value: GeometryCollection =>
        factory.createGeometryCollection(
          value.geometries
            .map(member => containmentSource(member, target))
            .filterNot(_.isEmpty)
            .toArray
        )
      case _ => toJts(geometry)

  private def toJts(geometry: Geometry): jts.Geometry =
    geometry match
      case value: Point      => factory.createPoint(coordinate(value.coordinates))
      case value: MultiPoint => factory.createMultiPointFromCoords(coordinates(value.coordinates))
      case value: LineString => factory.createLineString(coordinates(value.coordinates))
      case value: MultiLineString =>
        factory.createMultiLineString(
          value.coordinates.map(line => factory.createLineString(coordinates(line))).toArray
        )
      case value: Polygon      => polygon(value)
      case value: MultiPolygon => factory.createMultiPolygon(value.polygons.map(polygon).toArray)
      case value: GeometryCollection =>
        factory.createGeometryCollection(value.geometries.map(toJts).toArray)
