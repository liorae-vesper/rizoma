package rizoma
package domain

import prelude.Id

/** Farm values and the commands used to register or update them. */
object Farm:
  /** A trimmed, nonempty farm name of at most 100 UTF-16 code units. */
  opaque type Name = String

  /** Construction and validation errors for farm names. */
  object Name:
    /** Reasons a farm name cannot be created. */
    enum Error:
      /** The input is empty after trimming whitespace. */
      case Empty

      /** The trimmed name exceeds the supplied maximum length. */
      case TooLong(max: Int)

    /** Trims and validates a farm name, preserving internal whitespace.
      *
      * @param value
      *   the raw name; surrounding characters removed by `String.trim` are discarded
      * @return
      *   the normalized name, or `Empty` / `TooLong(100)` for invalid input
      */
    def from(value: String): Either[Name.Error, Name] =
      val maxLength = 100
      value.trim match
        case n if n.isEmpty            => Left(Name.Error.Empty)
        case n if n.length > maxLength => Left(Name.Error.TooLong(maxLength))
        case n                         => Right(n)

  /** Reasons a farm registration or patch request is rejected. */
  enum Constraints:
    /** The farm name failed [[Name.from]] validation. */
    case InvalidName(error: Name.Error)

  /** A registration command containing validated farm values.
    *
    * @param name
    *   the normalized farm name
    * @param location
    *   the validated geographic position
    * @param area
    *   the positive, finite area and its unit
    */
  final case class Register private (
      name: Name,
      location: Position,
      area: Area
  )

  /** Builds registration commands from a raw name and validated compound values. */
  object Register:
    /** Validates the name and assembles a farm registration command.
      *
      * Position and area validation is owned by their respective constructors.
      *
      * @param name
      *   the farm name; leading and trailing whitespace is removed
      * @param position
      *   the already validated geographic position of the farm
      * @param area
      *   the already validated total area available in the farm
      * @return
      *   either an error or the data to register a farm
      */
    def from(
        name: String,
        position: Position,
        area: Area
    ): Either[Constraints, Register] =
      for farmName <- Name.from(name).left.map(Constraints.InvalidName.apply)
      yield Register(farmName, position, area)

  /** A replacement of a farm's editable values, constructed within [[Farm]].
    *
    * @param name
    *   the normalized replacement name
    * @param location
    *   the validated replacement position
    * @param area
    *   the validated replacement total area
    * @param id
    *   the identity of the farm being updated
    */
  final case class Patch private[Farm] (name: Name, location: Position, area: Area, id: Id)

  /** A snapshot of a farm used to prepare commands for that farm.
    *
    * @param id
    *   the farm identity retained by patches
    * @param name
    *   the validated current name
    * @param location
    *   the validated current position
    * @param totalArea
    *   the validated current total area
    */
  final case class View(id: Id, name: Name, location: Position, totalArea: Area):

    /** Prepares a replacement of the editable values while retaining this farm's identity.
      *
      * @param name
      *   the raw replacement name, trimmed and validated by [[Name.from]]
      * @param location
      *   the already validated replacement position
      * @param area
      *   the already validated replacement total area
      * @return
      *   a patch command, or `InvalidName` when the replacement name is invalid
      */
    def patch(name: String, location: Position, area: Area): Either[Constraints, Patch] =
      for farmName <- Name.from(name).left.map(Constraints.InvalidName.apply)
      yield Patch(farmName, location, area, id)
