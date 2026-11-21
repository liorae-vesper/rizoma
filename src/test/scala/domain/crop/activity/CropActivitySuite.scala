package rizoma
package domain

import org.scalacheck.Prop.forAll

import java.time.Instant

class CropActivitySuite extends munit.ScalaCheckSuite:
  import DomainGenerators.*

  private val occurredAt = Instant.parse("2026-02-01T14:30:00Z")

  property("activity events trim surrounding whitespace"):
    forAll(names, padding, padding) { (event, before, after) =>
      assertEquals(crop.activity.Event.from(before + event + after).map(_.toString), Right(event))
    }

  property("activity events reject empty and whitespace-only input"):
    forAll(padding) { event =>
      assertEquals(crop.activity.Event.from(event), Left(crop.activity.Event.Error.Empty))
    }

  test("activity events preserve descriptive text, Unicode, and internal whitespace"):
    val event = "Irrigação  no talhão 🌱\n" + "field notes " * 100
    assertEquals(crop.activity.Event.from(s" $event ").map(_.toString), Right(event.trim))

  test("event types recognize common farm operations case-insensitively"):
    val examples = List(
      "soil preparation" -> crop.activity.EventType.SoilPreparation,
      "sowing" -> crop.activity.EventType.Sowing,
      "planting" -> crop.activity.EventType.Planting,
      "transplanting" -> crop.activity.EventType.Transplanting,
      "irrigation" -> crop.activity.EventType.Irrigation,
      "fertilization" -> crop.activity.EventType.Fertilization,
      "weeding" -> crop.activity.EventType.Weeding,
      "pruning" -> crop.activity.EventType.Pruning,
      "pest control" -> crop.activity.EventType.PestControl,
      "harvesting" -> crop.activity.EventType.Harvesting
    )
    examples.foreach { (raw, expected) =>
      assertEquals(crop.activity.EventType.from(raw), expected)
      assertEquals(
        crop.activity.EventType.from(s"  ${raw.toUpperCase(java.util.Locale.ROOT)}  "),
        expected
      )
      assertEquals(crop.activity.Event.from(s" $raw ").map(_.eventType), Right(expected))
    }
    List("SoilPreparation", "soil_preparation", "soil-preparation").foreach { raw =>
      assertEquals(crop.activity.EventType.from(raw), crop.activity.EventType.SoilPreparation)
    }
    List("PestControl", "pest_control", "pest-control").foreach { raw =>
      assertEquals(crop.activity.EventType.from(raw), crop.activity.EventType.PestControl)
    }

  test("unrecognized event types default to Unknown and preserve the raw string"):
    List("", " ", "Custom  task 🌱", "Irrigação", "  New operation\n").foreach { raw =>
      assertEquals(crop.activity.EventType.from(raw), crop.activity.EventType.Unknown(raw))
    }
    val raw = "  Custom  task 🌱  "
    assertEquals(
      crop.activity.Event.from(raw).map(_.eventType),
      Right(crop.activity.EventType.Unknown(raw.trim))
    )

  property("activity commands derive their event type while retaining the validated text"):
    forAll(ids) { id =>
      val result = for
        command <- crop.activity.Registration.from(
          crop.cycle.CycleId.from(id),
          " PLANTING ",
          occurredAt
        )
        view = crop.activity.View(
          crop.activity.ActivityId.from(id),
          command.cycleId,
          command.event,
          command.occurredAt
        )
        known <- view.patch("Irrigation", occurredAt)
        unknown <- view.patch("Check  soil moisture", occurredAt)
      yield
        assertEquals(command.event.toString, "PLANTING")
        assertEquals(command.eventType, crop.activity.EventType.Planting)
        assertEquals(view.eventType, crop.activity.EventType.Planting)
        assertEquals(known.eventType, crop.activity.EventType.Irrigation)
        assertEquals(unknown.eventType, crop.activity.EventType.Unknown("Check  soil moisture"))
        assertEquals(unknown.event.toString, "Check  soil moisture")
      assertEquals(result, Right(()))
    }

  property("activity registration validates the event and retains its cycle and timestamp"):
    forAll(ids, names) { (id, event) =>
      val cycleId = crop.cycle.CycleId.from(id)
      val result =
        crop.activity.Registration.from(cycleId, s" $event ", occurredAt).map { command =>
          assertEquals(command.cycleId, cycleId)
          assertEquals(command.event.toString, event)
          assertEquals(command.occurredAt, occurredAt)
        }
      assertEquals(result, Right(()))
      assertEquals(
        crop.activity.Registration.from(cycleId, " ", occurredAt),
        Left(crop.activity.Constraints.InvalidEvent(crop.activity.Event.Error.Empty))
      )
    }

  property("activity patches retain identities and validate replacement events"):
    forAll(ids, ids, names) { (id, cycle, event) =>
      val result = for
        original <- crop.activity.Event.from("Planting")
        view = crop.activity.View(
          crop.activity.ActivityId.from(id),
          crop.cycle.CycleId.from(cycle),
          original,
          occurredAt
        )
        patch <- view.patch(s" $event ", occurredAt.plusNanos(1))
      yield
        assertEquals(patch.activityId, view.activityId)
        assertEquals(patch.cycleId, view.cycleId)
        assertEquals(patch.event.toString, event)
        assertEquals(patch.occurredAt, occurredAt.plusNanos(1))
        assertEquals(
          view.patch(" ", occurredAt),
          Left(crop.activity.Constraints.InvalidEvent(crop.activity.Event.Error.Empty))
        )
      assertEquals(result, Right(()))
    }

  test("events, activity identities, and commands cannot bypass their boundaries"):
    assert(compileErrors("val event: crop.activity.Event = \"Planting\"").nonEmpty)
    assert(compileErrors("""
      for id <- Id.generate() yield
        val activityId: crop.activity.ActivityId = harvest.HarvestId.from(id)
        activityId
    """).nonEmpty)
    assert(compileErrors("crop.activity.Registration(null, null, null, null)").nonEmpty)
    assert(compileErrors("crop.activity.Patch(null, null, null, null, null, null)").nonEmpty)
