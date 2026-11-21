package rizoma
package domain.plot

import rizoma.domain.*

import prelude.Id

/** Plot values and the commands used to register, update, or remove them. */

/** Whether a plot is in active use or resting between crops. */
enum Status:
  case Active, Resting

/** A plot identity distinct from a general application [[Id]]. */
opaque type PlotId = Id

/** Construction of plot identities from validated application identifiers. */
object PlotId:
  /** Gives an existing application identifier the plot-specific type.
    *
    * @param id
    *   the already validated identifier; plot existence is not checked
    * @return
    *   the same identifier represented as a plot identity
    */
  def from(id: Id): PlotId = id

/** A trimmed, nonempty plot name of at most 100 UTF-16 code units. */
opaque type Name = String

/** Construction and validation errors for plot names. */
object Name:
  /** Reasons a plot name cannot be created. */
  enum Error:
    /** The input is empty after trimming whitespace. */
    case Empty

    /** The trimmed name exceeds the supplied maximum length. */
    case TooLong(max: Int)

  /** Trims and validates a plot name, preserving internal whitespace.
    *
    * @param name
    *   the raw name; surrounding characters removed by `String.trim` are discarded
    * @return
    *   the normalized name, or `Empty` / `TooLong(100)` for invalid input
    */
  def from(name: String): Either[Name.Error, Name] =
    val maxNameSize = 100
    name.trim match
      case n if n.isEmpty              => Left(Name.Error.Empty)
      case n if n.length > maxNameSize => Left(Name.Error.TooLong(maxNameSize))
      case n                           => Right(n)

/** Reasons a plot patch request is rejected. */
enum Constraints:
  /** The replacement name failed [[Name.from]] validation. */
  case InvalidName(error: Name.Error)

/** A registration command assembled from already validated values.
  *
  * No additional relationship between geometry, area, and farm is checked here.
  *
  * @param geometry
  *   the plot geometry, which may be empty under the [[MultiPolygon]] contract
  * @param area
  *   the declared positive, finite area and its unit
  * @param name
  *   the normalized plot name
  * @param farmId
  *   the farm to which the plot will belong
  * @param status
  *   the plot's current use; defaults to active
  */
final case class Registration(
    geometry: MultiPolygon,
    area: Area,
    name: Name,
    farmId: Id,
    status: Status = Status.Active
)

/** A replacement of a plot's editable values, constructed within [[plot]].
  *
  * @param newGeometry
  *   the validated replacement geometry
  * @param newArea
  *   the validated replacement declared area
  * @param newName
  *   the normalized replacement name
  * @param currentFarm
  *   the existing farm identity retained by the patch
  * @param plotId
  *   the identity of the plot being updated
  * @param status
  *   the replacement use status
  */
final case class Patch private[plot] (
    newGeometry: MultiPolygon,
    newArea: Area,
    newName: Name,
    currentFarm: Id,
    plotId: PlotId,
    status: Status,
    expected: View
)

/** A command requesting removal of a plot.
  *
  * @param plotId
  *   the identity of the plot to remove
  */
final case class Delete(plotId: PlotId)

/** Replaces the geographic boundary of an existing plot. */
final case class Mark(plotId: PlotId, geometry: MultiPolygon)

/** Removes a plot's boundary, represented by an empty multipolygon. */
final case class Demark(plotId: PlotId)

/** Replaces the declared area without changing the boundary. */
final case class Remeasure(plotId: PlotId, area: Area)

final case class Activate(plotId: PlotId)
final case class Resting(plotId: PlotId)

/** A snapshot of a plot used to prepare commands for that plot.
  *
  * @param geometry
  *   the validated current geometry
  * @param area
  *   the validated current declared area
  * @param name
  *   the validated current name
  * @param currentFarm
  *   the farm identity retained by patches
  * @param plotId
  *   the plot identity retained by patches
  * @param status
  *   the current use status; defaults to active
  */
final case class View(
    geometry: MultiPolygon,
    area: Area,
    name: Name,
    currentFarm: Id,
    plotId: PlotId,
    status: Status = Status.Active
):

  /** Prepares replacement values while retaining the plot and farm identities.
    *
    * Geometry and area are accepted under their own contracts without further contextual checks.
    *
    * @param newGeometry
    *   the already validated replacement geometry
    * @param newArea
    *   the already validated replacement declared area
    * @param newName
    *   the raw replacement name, trimmed and validated by [[Name.from]]
    * @param newStatus
    *   the replacement status; defaults to the current status
    * @return
    *   a patch command, or `InvalidName` when the replacement name is invalid
    */
  def patch(
      newGeometry: MultiPolygon,
      newArea: Area,
      newName: String,
      newStatus: Status = status
  ): Either[Constraints, Patch] =
    for name <- Name.from(newName).left.map(Constraints.InvalidName.apply)
    yield Patch(newGeometry, newArea, name, currentFarm, plotId, newStatus, this)
