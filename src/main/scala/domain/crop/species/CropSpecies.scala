package rizoma
package domain.crop.species

import rizoma.domain.*

import prelude.Id

/** Crop species and the commands used to register, update, or remove them. */

/** A species identity, distinct from plot and crop cycle identities. */
opaque type SpeciesId = Id

object SpeciesId:
  /** Wraps a validated application identifier without checking species existence. */
  def from(id: Id): SpeciesId = id

/** A trimmed, nonempty common name of at most 100 UTF-16 code units. */
opaque type CommonName = String

object CommonName:
  enum Error:
    case Empty
    case TooLong(max: Int)

  /** Validates a common name, preserving internal whitespace and Unicode. */
  def from(value: String): Either[Error, CommonName] =
    val maxLength = 100
    value.trim match
      case name if name.isEmpty            => Left(Error.Empty)
      case name if name.length > maxLength => Left(Error.TooLong(maxLength))
      case name                            => Right(name)

/** A trimmed, nonempty scientific name of at most 100 UTF-16 code units. */
opaque type ScientificName = String

object ScientificName:
  enum Error:
    case Empty
    case TooLong(max: Int)

  /** Validates the name as text; botanical nomenclature is not checked. */
  def from(value: String): Either[Error, ScientificName] =
    val maxLength = 100
    value.trim match
      case name if name.isEmpty            => Left(Error.Empty)
      case name if name.length > maxLength => Left(Error.TooLong(maxLength))
      case name                            => Right(name)

enum Constraints:
  case InvalidCommonName(error: CommonName.Error)
  case InvalidScientificName(error: ScientificName.Error)

/** Validated values for registering a species. */
final case class Registration private (commonName: CommonName, scientificName: ScientificName)

object Registration:
  /** Normalizes and validates both names before constructing the registration command. */
  def from(commonName: String, scientificName: String): Either[Constraints, Registration] =
    for
      common <- CommonName.from(commonName).left.map(Constraints.InvalidCommonName.apply)
      scientific <- ScientificName
        .from(scientificName)
        .left
        .map(Constraints.InvalidScientificName.apply)
    yield Registration(common, scientific)

/** Replacement names for an existing species. */
final case class Patch private[species] (
    commonName: CommonName,
    scientificName: ScientificName,
    speciesId: SpeciesId
)

/** Requests removal; relationships and species existence are checked by the application. */
final case class Delete(speciesId: SpeciesId)

/** A species snapshot used to prepare commands while retaining its identity. */
final case class View(
    speciesId: SpeciesId,
    commonName: CommonName,
    scientificName: ScientificName
):
  /** Replaces both names after validating their raw input. */
  def patch(commonName: String, scientificName: String): Either[Constraints, Patch] =
    Registration.from(commonName, scientificName).map { registration =>
      Patch(registration.commonName, registration.scientificName, speciesId)
    }
