package rizoma.domain

import rizoma.prelude.InvalidGeneration

/** Expected failures returned by business actions. */
sealed abstract class ActionError(message: String, cause: Option[Throwable] = None)
    extends RuntimeException(message, cause.orNull)

object ActionError:
  final case class Generation(error: InvalidGeneration)
      extends ActionError("ID generation failed", Some(error))
  final case class NotFound(entity: String, id: String)
      extends ActionError(s"$entity $id was not found")
  final case class HasDependents(entity: String, id: String)
      extends ActionError(s"$entity $id has active dependents")
  final case class Conflict(entity: String, id: String)
      extends ActionError(s"$entity $id has changed")
  final case class InvalidReference(entity: String)
      extends ActionError(s"Invalid reference for $entity")
  final case class InsufficientStock(
      stockId: String,
      location: String,
      available: BigDecimal,
      requested: BigDecimal
  ) extends ActionError(s"Insufficient stock $stockId at $location")
  final case class InvalidState(reason: String) extends ActionError(reason)
  final case class PersistenceFailure(error: Throwable)
      extends ActionError("Persistence failed", Some(error))

  def fromThrowable(error: Throwable): ActionError = error match
    case known: ActionError            => known
    case generation: InvalidGeneration => Generation(generation)
    case other                         => PersistenceFailure(other)
