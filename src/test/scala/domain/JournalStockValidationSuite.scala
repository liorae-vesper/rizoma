package rizoma.domain

import rizoma.domain.crop.activity
import rizoma.persistence.SqliteInventory
import rizoma.prelude.Id

class JournalStockValidationSuite extends BusinessFixture:
  test("a journal with multiple linked inputs rolls back all stock and the activity if one fails"):
    withFixture { f =>
      import f.*
      seed()
      val store = new SqliteInventory(db)
      val id = Id.generate().fold(e => fail(e.toString), identity)
      val stockId = inventory.StockId.from(id)
      val receipt = inventory.Receipt
        .from(farmId, "Seed", "kg", "Shed", BigDecimal(10), at)
        .fold(e => fail(e.toString), identity)
      assert(inventory.receive(receipt)(using store, generator(id)).isRight)
      val link = activity.StockLink.from(stockId, "Shed").fold(e => fail(e.toString), identity)
      val quantity =
        activity.Quantity.from(BigDecimal(6), "kg").fold(e => fail(e.toString), identity)
      val input = activity.InputUse
        .from("Seed", quantity, None, None, stock = Some(link))
        .fold(e => fail(e.toString), identity)
      val journal = activity.Journal
        .from(None, None, List(input, input), Nil, Nil)
        .fold(e => fail(e.toString), identity)
      val command = activity.Registration
        .from(activityInput.cycleId, "Planting", at, journal)
        .fold(e => fail(e.toString), identity)
      val newId = Id.generate().fold(e => fail(e.toString), identity)
      assert(activity.record(command)(using activities, generator(newId)).isLeft)
      assertEquals(activity.get(activity.ActivityId.from(newId))(using activities), Right(None))
      assertEquals(inventory.history(stockId)(using store).map(_.size), Right(1))
      assertEquals(
        inventory.get(stockId)(using store).map(_.map(_.balances.map(_.quantity))),
        Right(Some(List(BigDecimal(10))))
      )
    }

  test("linked inputs reject a different farm, item name or unit"):
    withFixture { f =>
      import f.*
      seed()
      val store = new SqliteInventory(db)
      val otherFarm = Id.generate().fold(e => fail(e.toString), identity)
      val otherStock = Id.generate().fold(e => fail(e.toString), identity)
      val ownStock = Id.generate().fold(e => fail(e.toString), identity)
      val _ = db.execute(
        "INSERT INTO farms VALUES (?, ?, ?, ?, ?)",
        Seq(otherFarm.value, "Other", "[0,0]", 1.0, "acres")
      )
      val foreign = inventory.Receipt
        .from(otherFarm, "Seed", "kg", "Shed", BigDecimal(10), at)
        .fold(e => fail(e.toString), identity)
      val own = inventory.Receipt
        .from(farmId, "Seed", "kg", "Shed", BigDecimal(10), at)
        .fold(e => fail(e.toString), identity)
      assert(inventory.receive(foreign)(using store, generator(otherStock)).isRight)
      assert(inventory.receive(own)(using store, generator(ownStock)).isRight)
      def journal(id: Id, name: String, unit: String): activity.Journal =
        val link = activity.StockLink
          .from(inventory.StockId.from(id), "Shed")
          .fold(e => fail(e.toString), identity)
        val quantity =
          activity.Quantity.from(BigDecimal(1), unit).fold(e => fail(e.toString), identity)
        val input = activity.InputUse
          .from(name, quantity, None, None, stock = Some(link))
          .fold(e => fail(e.toString), identity)
        activity.Journal
          .from(None, None, List(input), Nil, Nil)
          .fold(e => fail(e.toString), identity)
      List(
        journal(otherStock, "Seed", "kg"),
        journal(ownStock, "Fertilizer", "kg"),
        journal(ownStock, "Seed", "lb")
      ).foreach { details =>
        val id = Id.generate().fold(e => fail(e.toString), identity)
        val command = activity.Registration
          .from(activityInput.cycleId, "Planting", at, details)
          .fold(e => fail(e.toString), identity)
        assertEquals(
          activity.record(command)(using activities, generator(id)),
          Left(ActionError.InvalidReference("journal input stock"))
        )
        assertEquals(activity.get(activity.ActivityId.from(id))(using activities), Right(None))
      }
      assertEquals(
        inventory
          .get(inventory.StockId.from(ownStock))(using store)
          .map(_.map(_.balances.map(_.quantity))),
        Right(Some(List(BigDecimal(10))))
      )
    }

  test("journal entries written before stock links can still be read and edited"):
    withFixture { f =>
      import f.*
      seed()
      val legacy =
        """{"operator":null,"notes":null,"inputs":[{"name":"Seed","quantity":{"amount":2,"unit":"kg"},"method":null,"cost":null,"source":"Purchased"}],"labor":[],"outputs":[]} """.trim
      val _ =
        db.execute("UPDATE activities SET journal = ? WHERE id = ?", Seq(legacy, activityId.value))
      val id = activity.ActivityId.from(activityId)
      val original = activity
        .get(id)(using activities)
        .fold(e => fail(e.toString), identity)
        .getOrElse(fail("missing legacy journal"))
      assertEquals(original.journal.inputs.map(_.stock), List(None))
      val patch = original.patch("Updated", at).fold(e => fail(e.toString), identity)
      assertEquals(activity.edit(patch)(using activities), Right(()))
      assertEquals(
        activity.get(id)(using activities).map(_.map(_.journal)),
        Right(Some(original.journal))
      )
    }
