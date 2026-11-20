package rizoma
package domain.crop.activity

import rizoma.domain.*
import rizoma.prelude.Id

import java.time.Instant
import java.util.Locale

/** Timestamped events associated with a crop cycle. */

opaque type ActivityId = Id

object ActivityId:
  /** Wraps a validated application identifier without checking activity existence. */
  def from(id: Id): ActivityId = id

/** Recognized farm operations, with an open-ended fallback that preserves unrecognized text. */
enum EventType:
  case SoilPreparation, Sowing, Planting, Transplanting, Irrigation
  case Fertilization, Weeding, Pruning, PestControl, Harvesting
  case Unknown(raw: String)

object EventType:
  /** Recognizes operation names case-insensitively; unmatched input is preserved exactly.
    *
    * Multiword names accept spaces, underscores, hyphens, or the enum's camel-case spelling. Blank
    * input also maps to Unknown; Event.from enforces nonempty activity text separately.
    */
  def from(raw: String): EventType =
    raw.trim.toLowerCase(Locale.ROOT) match
      case "soil preparation" | "soil_preparation" | "soil-preparation" | "soilpreparation" =>
        EventType.SoilPreparation
      case "sowing"        => EventType.Sowing
      case "planting"      => EventType.Planting
      case "transplanting" => EventType.Transplanting
      case "irrigation"    => EventType.Irrigation
      case "fertilization" => EventType.Fertilization
      case "weeding"       => EventType.Weeding
      case "pruning"       => EventType.Pruning
      case "pest control" | "pest_control" | "pest-control" | "pestcontrol" =>
        EventType.PestControl
      case "harvesting" => EventType.Harvesting
      case _            => EventType.Unknown(raw)

/** A trimmed, nonempty event description. Its text is retained alongside a derived event type. */
opaque type Event = String

object Event:
  enum Error:
    case Empty

  /** Trims surrounding whitespace while preserving the event's internal text. */
  def from(value: String): Either[Error, Event] =
    value.trim match
      case event if event.isEmpty => Left(Error.Empty)
      case event                  => Right(event)

  extension (event: Event)
    /** Classifies the stored text without changing it. */
    def eventType: EventType = EventType.from(event)

enum Constraints:
  case InvalidEvent(error: Event.Error)

/** Validated registration values; cycle existence is checked by the application. */
final case class Registration private (
    cycleId: crop.cycle.CycleId,
    event: Event,
    occurredAt: Instant
):
  def eventType: EventType = event.eventType

object Registration:
  /** Validates the event and retains the supplied cycle and event timestamp. */
  def from(
      cycleId: crop.cycle.CycleId,
      event: String,
      occurredAt: Instant
  ): Either[Constraints, Registration] =
    Event.from(event).left.map(Constraints.InvalidEvent.apply).map { description =>
      Registration(cycleId, description, occurredAt)
    }

/** Replacement event values, retaining activity and crop cycle identities. */
final case class Patch private[activity] (
    event: Event,
    occurredAt: Instant,
    cycleId: crop.cycle.CycleId,
    activityId: ActivityId
):
  def eventType: EventType = event.eventType

final case class Destroy(activityId: ActivityId)

/** An activity snapshot used to prepare commands for the same activity and cycle. */
final case class View(
    activityId: ActivityId,
    cycleId: crop.cycle.CycleId,
    event: Event,
    occurredAt: Instant
):
  def eventType: EventType = event.eventType

  /** Validates a replacement event and replaces its occurrence timestamp. */
  def patch(event: String, occurredAt: Instant): Either[Constraints, Patch] =
    Event.from(event).left.map(Constraints.InvalidEvent.apply).map { description =>
      Patch(description, occurredAt, cycleId, activityId)
    }
