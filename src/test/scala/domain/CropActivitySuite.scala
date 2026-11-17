package rizoma
package domain

import org.scalacheck.Prop.forAll

import java.time.Instant

class CropActivitySuite extends munit.ScalaCheckSuite:
  import DomainGenerators.*

  private val occurredAt = Instant.parse("2026-02-01T14:30:00Z")

  property("activity events trim surrounding whitespace"):
    forAll(names, padding, padding) { (event, before, after) =>
      assertEquals(CropActivity.Event.from(before + event + after).map(_.toString), Right(event))
    }

  property("activity events reject empty and whitespace-only input"):
    forAll(padding) { event =>
      assertEquals(CropActivity.Event.from(event), Left(CropActivity.Event.Error.Empty))
    }

  test("activity events preserve descriptive text, Unicode, and internal whitespace"):
    val event = "Irrigação  no talhão 🌱\n" + "field notes " * 100
    assertEquals(CropActivity.Event.from(s" $event ").map(_.toString), Right(event.trim))

  test("event types recognize common farm operations case-insensitively"):
    val examples = List(
      "soil preparation" -> CropActivity.EventType.SoilPreparation,
      "sowing" -> CropActivity.EventType.Sowing,
      "planting" -> CropActivity.EventType.Planting,
      "transplanting" -> CropActivity.EventType.Transplanting,
      "irrigation" -> CropActivity.EventType.Irrigation,
      "fertilization" -> CropActivity.EventType.Fertilization,
      "weeding" -> CropActivity.EventType.Weeding,
      "pruning" -> CropActivity.EventType.Pruning,
      "pest control" -> CropActivity.EventType.PestControl,
      "harvesting" -> CropActivity.EventType.Harvesting
    )
    examples.foreach { (raw, expected) =>
      assertEquals(CropActivity.EventType.from(raw), expected)
      assertEquals(
        CropActivity.EventType.from(s"  ${raw.toUpperCase(java.util.Locale.ROOT)}  "),
        expected
      )
      assertEquals(CropActivity.Event.from(s" $raw ").map(_.eventType), Right(expected))
    }
    List("SoilPreparation", "soil_preparation", "soil-preparation").foreach { raw =>
      assertEquals(CropActivity.EventType.from(raw), CropActivity.EventType.SoilPreparation)
    }
    List("PestControl", "pest_control", "pest-control").foreach { raw =>
      assertEquals(CropActivity.EventType.from(raw), CropActivity.EventType.PestControl)
    }

  test("unrecognized event types default to Unknown and preserve the raw string"):
    List("", " ", "Custom  task 🌱", "Irrigação", "  New operation\n").foreach { raw =>
      assertEquals(CropActivity.EventType.from(raw), CropActivity.EventType.Unknown(raw))
    }
    val raw = "  Custom  task 🌱  "
    assertEquals(
      CropActivity.Event.from(raw).map(_.eventType),
      Right(CropActivity.EventType.Unknown(raw.trim))
    )

  property("activity commands derive their event type while retaining the validated text"):
    forAll(ids) { id =>
      val result = for
        command <- CropActivity.Register.from(CropCycle.CycleId.from(id), " PLANTING ", occurredAt)
        view = CropActivity.View(
          CropActivity.ActivityId.from(id),
          command.cycleId,
          command.event,
          command.occurredAt
        )
        known <- view.patch("Irrigation", occurredAt)
        unknown <- view.patch("Check  soil moisture", occurredAt)
      yield
        assertEquals(command.event.toString, "PLANTING")
        assertEquals(command.eventType, CropActivity.EventType.Planting)
        assertEquals(view.eventType, CropActivity.EventType.Planting)
        assertEquals(known.eventType, CropActivity.EventType.Irrigation)
        assertEquals(unknown.eventType, CropActivity.EventType.Unknown("Check  soil moisture"))
        assertEquals(unknown.event.toString, "Check  soil moisture")
      assertEquals(result, Right(()))
    }

  property("activity registration validates the event and retains its cycle and timestamp"):
    forAll(ids, names) { (id, event) =>
      val cycleId = CropCycle.CycleId.from(id)
      val result = CropActivity.Register.from(cycleId, s" $event ", occurredAt).map { command =>
        assertEquals(command.cycleId, cycleId)
        assertEquals(command.event.toString, event)
        assertEquals(command.occurredAt, occurredAt)
      }
      assertEquals(result, Right(()))
      assertEquals(
        CropActivity.Register.from(cycleId, " ", occurredAt),
        Left(CropActivity.Constraints.InvalidEvent(CropActivity.Event.Error.Empty))
      )
    }

  property("activity patches retain identities and validate replacement events"):
    forAll(ids, ids, names) { (id, cycle, event) =>
      val result = for
        original <- CropActivity.Event.from("Planting")
        view = CropActivity.View(
          CropActivity.ActivityId.from(id),
          CropCycle.CycleId.from(cycle),
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
          Left(CropActivity.Constraints.InvalidEvent(CropActivity.Event.Error.Empty))
        )
      assertEquals(result, Right(()))
    }

  test("events, activity identities, and commands cannot bypass their boundaries"):
    assert(compileErrors("val event: CropActivity.Event = \"Planting\"").nonEmpty)
    assert(compileErrors("""
      for id <- Id.generate() yield
        val activityId: CropActivity.ActivityId = Harvest.HarvestId.from(id)
        activityId
    """).nonEmpty)
    assert(compileErrors("CropActivity.Register(null, null, null)").nonEmpty)
    assert(compileErrors("CropActivity.Patch(null, null, null, null)").nonEmpty)
