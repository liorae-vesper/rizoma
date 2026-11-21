package rizoma.domain

import rizoma.domain.crop.activity
import rizoma.prelude.Id

class FieldJournalSuite extends BusinessFixture:
  test("structured entries round-trip, preserve details on edits, and hide deleted records"):
    withFixture { f =>
      import f.*
      seed()
      assertEquals(
        activity.get(activity.ActivityId.from(activityId))(using activities).map(_.map(_.journal)),
        Right(Some(activity.Journal.empty))
      )
      val quantity =
        activity.Quantity.from(BigDecimal("2.50"), " kg ").fold(e => fail(e.toString), identity)
      val money =
        activity.Money.from(BigDecimal("13.45"), "brl").fold(e => fail(e.toString), identity)
      val input = activity.InputUse
        .from(
          " Compost ",
          quantity,
          Some(" broadcast "),
          Some(money),
          activity.InputSource.FarmProduced
        )
        .fold(e => fail(e.toString), identity)
      val water = activity.Quantity
        .from(BigDecimal("1000000000000000000000.000000001"), "L")
        .fold(e => fail(e.toString), identity)
      val secondInput =
        activity.InputUse.from("Water", water, None, None).fold(e => fail(e.toString), identity)
      val labor = activity.Labor
        .from(" Worker A ", BigDecimal("1.25"), Some(money))
        .fold(e => fail(e.toString), identity)
      val output = activity.Output
        .from(" Beans ", quantity, activity.Disposition.Stored, Some(money))
        .fold(e => fail(e.toString), identity)
      val sold = activity.Output
        .from("Beans", quantity, activity.Disposition.Sold, None)
        .fold(e => fail(e.toString), identity)
      val journal = activity.Journal
        .from(
          Some(" Ana "),
          Some(" Soil moist "),
          List(input, secondInput),
          List(labor),
          List(output, sold)
        )
        .fold(e => fail(e.toString), identity)
      val command = activity.Registration
        .from(activityInput.cycleId, "Fertilization", at, journal)
        .fold(e => fail(e.toString), identity)
      val id = Id.generate().fold(e => fail(e.toString), identity)
      assertEquals(
        activity.record(command)(using activities, generator(id)),
        Right(activity.ActivityId.from(id))
      )
      val read = activity
        .get(activity.ActivityId.from(id))(using activities)
        .fold(e => fail(e.toString), identity)
        .getOrElse(fail("missing entry"))
      assertEquals(read.journal, journal)
      assertEquals(read.journal.operator, Some("Ana"))
      assertEquals(
        activity.listByCycle(activityInput.cycleId)(using activities).map(_.size),
        Right(2)
      )
      val patch = read.patch("Irrigation", at.plusSeconds(1)).fold(e => fail(e.toString), identity)
      assertEquals(activity.edit(patch)(using activities), Right(()))
      assertEquals(
        activity.edit(patch)(using activities),
        Left(ActionError.Conflict("activity", id.value))
      )
      assertEquals(
        activity.get(activity.ActivityId.from(id))(using activities).map(_.map(_.journal)),
        Right(Some(journal))
      )
      assertEquals(
        activity.delete(activity.Delete(activity.ActivityId.from(id)))(using activities),
        Right(())
      )
      assertEquals(activity.get(activity.ActivityId.from(id))(using activities), Right(None))
      assertEquals(
        activity.listByCycle(activityInput.cycleId)(using activities).map(_.size),
        Right(1)
      )
      assertEquals(
        activity.listByCycle(
          rizoma.domain.crop.cycle.CycleId.from(Id.generate().fold(e => fail(e.toString), identity))
        )(using activities),
        Right(Nil)
      )
    }

  test("journal values reject invalid quantities, money, and blank descriptions"):
    assert(activity.Quantity.from(BigDecimal(0), "kg").isLeft)
    assert(activity.Quantity.from(BigDecimal(1), "  ").isLeft)
    assert(activity.Money.from(BigDecimal(-1), "BRL").isLeft)
    assert(activity.Money.from(BigDecimal(1), "bad code").isLeft)
    assert(activity.Labor.from(" ", BigDecimal(1), None).isLeft)
    assert(activity.Labor.from("Worker", BigDecimal(0), None).isLeft)
    assert(activity.Journal.from(Some(" "), None, Nil, Nil, Nil).isLeft)
    assert(activity.Money.from(BigDecimal(0), "BRL").isRight)
    assert(compileErrors("activity.Quantity(BigDecimal(-1), \"kg\")").nonEmpty)
    assert(compileErrors("activity.Money(BigDecimal(-1), \"BRL\")").nonEmpty)
    assert(compileErrors("activity.Journal(None, None, Nil, Nil, Nil)").nonEmpty)

  test("malformed stored journal data is reported, not silently skipped"):
    withFixture { f =>
      import f.*
      seed()
      val _ = db.execute(
        "UPDATE activities SET journal = ? WHERE id = ?",
        Seq("{\"inputs\": []}", activityId.value)
      )
      assert(
        activity
          .get(activity.ActivityId.from(activityId))(using activities)
          .left
          .exists(_.isInstanceOf[ActionError.PersistenceFailure])
      )
      assert(
        activity
          .listByCycle(activityInput.cycleId)(using activities)
          .left
          .exists(_.isInstanceOf[ActionError.PersistenceFailure])
      )
    }

  test("journal entries are ordered by instant, including fractional seconds"):
    withFixture { f =>
      import f.*
      seed()
      val earlier = activity.Registration
        .from(activityInput.cycleId, "Earlier", at.minusNanos(1))
        .fold(e => fail(e.toString), identity)
      val later = activity.Registration
        .from(activityInput.cycleId, "Later", at.plusNanos(1))
        .fold(e => fail(e.toString), identity)
      val earlierId = Id.generate().fold(e => fail(e.toString), identity)
      val laterId = Id.generate().fold(e => fail(e.toString), identity)
      assert(activity.record(later)(using activities, generator(laterId)).isRight)
      assert(activity.record(earlier)(using activities, generator(earlierId)).isRight)
      assertEquals(
        activity.listByCycle(activityInput.cycleId)(using activities).map(_.map(_.activityId)),
        Right(
          List(
            activity.ActivityId.from(earlierId),
            activity.ActivityId.from(activityId),
            activity.ActivityId.from(laterId)
          )
        )
      )
    }
