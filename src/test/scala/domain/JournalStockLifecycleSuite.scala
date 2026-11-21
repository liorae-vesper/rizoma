package rizoma.domain

import rizoma.domain.crop.activity
import rizoma.persistence.SqliteInventory
import rizoma.prelude.Id

class JournalStockLifecycleSuite extends BusinessFixture:
  test("journal input consumption is atomic and editing or deleting reconciles stock"):
    withFixture { f =>
      import f.*
      seed()
      val store = new SqliteInventory(db)
      val stockId = Id.generate().fold(e => fail(e.toString), identity)
      val ref = inventory.StockId.from(stockId)
      val receipt = inventory.Receipt
        .from(farmId, "Compost", "kg", "Shed", BigDecimal(10), at)
        .fold(e => fail(e.toString), identity)
      assert(inventory.receive(receipt)(using store, generator(stockId)).isRight)
      val link = activity.StockLink.from(ref, "Shed").fold(e => fail(e.toString), identity)
      def journal(amount: BigDecimal): activity.Journal =
        val quantity = activity.Quantity.from(amount, "kg").fold(e => fail(e.toString), identity)
        val input = activity.InputUse
          .from("Compost", quantity, None, None, stock = Some(link))
          .fold(e => fail(e.toString), identity)
        activity.Journal
          .from(None, None, List(input), Nil, Nil)
          .fold(e => fail(e.toString), identity)
      val newActivity = Id.generate().fold(e => fail(e.toString), identity)
      val command = activity.Registration
        .from(activityInput.cycleId, "Fertilization", at, journal(BigDecimal(4)))
        .fold(e => fail(e.toString), identity)
      assert(activity.record(command)(using activities, generator(newActivity)).isRight)
      def balance: BigDecimal = inventory
        .get(ref)(using store)
        .fold(e => fail(e.toString), identity)
        .getOrElse(fail("missing"))
        .balances
        .find(_.location == "Shed")
        .fold(BigDecimal(-1))(_.quantity)
      assertEquals(balance, BigDecimal(6))
      val original = activity
        .get(activity.ActivityId.from(newActivity))(using activities)
        .fold(e => fail(e.toString), identity)
        .getOrElse(fail("missing activity"))
      val annotation =
        original.patch("Irrigation", at, original.journal).fold(e => fail(e.toString), identity)
      assertEquals(activity.edit(annotation)(using activities), Right(()))
      assertEquals(inventory.history(ref)(using store).map(_.size), Right(2))
      val revised = activity
        .get(activity.ActivityId.from(newActivity))(using activities)
        .fold(e => fail(e.toString), identity)
        .getOrElse(fail("missing activity"))
      val tooMuch = revised
        .patch("Fertilization", at, journal(BigDecimal(11)))
        .fold(e => fail(e.toString), identity)
      assert(activity.edit(tooMuch)(using activities).isLeft)
      assertEquals(balance, BigDecimal(6))
      assertEquals(
        activity.get(activity.ActivityId.from(newActivity))(using activities),
        Right(Some(revised))
      )
      val updated = revised
        .patch("Fertilization", at, journal(BigDecimal(7)))
        .fold(e => fail(e.toString), identity)
      assertEquals(activity.edit(updated)(using activities), Right(()))
      assertEquals(balance, BigDecimal(3))
      assertEquals(
        activity.delete(activity.Delete(activity.ActivityId.from(newActivity)))(using activities),
        Right(())
      )
      assertEquals(balance, BigDecimal(10))
      assertEquals(
        inventory.history(ref)(using store).map(_.map(_.kind)),
        Right(
          List(
            inventory.MovementKind.Receipt,
            inventory.MovementKind.JournalUse,
            inventory.MovementKind.JournalReversal,
            inventory.MovementKind.JournalUse,
            inventory.MovementKind.JournalReversal
          )
        )
      )
      assertEquals(
        inventory.history(ref)(using store).map(_.drop(1).map(_.activityId)),
        Right(List.fill(4)(Some(activity.ActivityId.from(newActivity))))
      )
    }

  test("correcting a journal timestamp records a reversal and replacement movement"):
    withFixture { f =>
      import f.*
      seed()
      val store = new SqliteInventory(db)
      val stockId = Id.generate().fold(e => fail(e.toString), identity)
      val ref = inventory.StockId.from(stockId)
      val receipt = inventory.Receipt
        .from(farmId, "Seed", "kg", "Shed", BigDecimal(10), at)
        .fold(e => fail(e.toString), identity)
      assert(inventory.receive(receipt)(using store, generator(stockId)).isRight)
      val link = activity.StockLink.from(ref, "Shed").fold(e => fail(e.toString), identity)
      val quantity =
        activity.Quantity.from(BigDecimal(4), "kg").fold(e => fail(e.toString), identity)
      val input = activity.InputUse
        .from("Seed", quantity, None, None, stock = Some(link))
        .fold(e => fail(e.toString), identity)
      val journal = activity.Journal
        .from(None, None, List(input), Nil, Nil)
        .fold(e => fail(e.toString), identity)
      val newId = Id.generate().fold(e => fail(e.toString), identity)
      val command = activity.Registration
        .from(activityInput.cycleId, "Planting", at, journal)
        .fold(e => fail(e.toString), identity)
      assert(activity.record(command)(using activities, generator(newId)).isRight)
      val view = activity
        .get(activity.ActivityId.from(newId))(using activities)
        .fold(e => fail(e.toString), identity)
        .getOrElse(fail("missing"))
      val updated = view.patch("Planting", at.plusSeconds(2)).fold(e => fail(e.toString), identity)
      assertEquals(activity.edit(updated)(using activities), Right(()))
      val history = inventory.history(ref)(using store).fold(e => fail(e.toString), identity)
      assertEquals(
        history.map(_.kind),
        List(
          inventory.MovementKind.Receipt,
          inventory.MovementKind.JournalUse,
          inventory.MovementKind.JournalReversal,
          inventory.MovementKind.JournalUse
        )
      )
      assertEquals(history.lastOption.map(_.occurredAt), Some(at.plusSeconds(2)))
      assertEquals(
        inventory.get(ref)(using store).map(_.map(_.balances.map(_.quantity))),
        Right(Some(List(BigDecimal(6))))
      )
    }
