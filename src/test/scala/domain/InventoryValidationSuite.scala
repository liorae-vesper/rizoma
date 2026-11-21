package rizoma.domain

import rizoma.persistence.SqliteInventory
import rizoma.prelude.Id

class InventoryValidationSuite extends BusinessFixture:
  test("invalid quantities, locations and references are rejected"):
    withFixture { f =>
      import f.*
      seed()
      val id = Id.generate().fold(e => fail(e.toString), identity)
      val ref = inventory.StockId.from(id)
      assert(inventory.Receipt.from(farmId, " ", "kg", "Store", BigDecimal(1), at).isLeft)
      assert(inventory.Receipt.from(farmId, "Seed", " ", "Store", BigDecimal(1), at).isLeft)
      assert(inventory.Receipt.from(farmId, "Seed", "kg", " ", BigDecimal(1), at).isLeft)
      assert(inventory.Receipt.from(farmId, "Seed", "kg", "Store", BigDecimal(0), at).isLeft)
      assert(inventory.Use.from(ref, "Store", BigDecimal(-1), at).isLeft)
      assert(inventory.Transfer.from(ref, "Store", " Store ", BigDecimal(1), at).isLeft)
      assert(inventory.Adjustment.from(ref, "Store", BigDecimal(-1), at).isLeft)
      val store = new SqliteInventory(db)
      val missingFarm = Id.generate().fold(e => fail(e.toString), identity)
      val receipt = inventory.Receipt
        .from(missingFarm, "Seed", "kg", "Store", BigDecimal(1), at)
        .fold(e => fail(e.toString), identity)
      assertEquals(
        inventory.receive(receipt)(using store, generator(id)),
        Left(ActionError.InvalidReference("farm"))
      )
      assertEquals(inventory.get(ref)(using store), Right(None))
      assertEquals(
        inventory.history(ref)(using store),
        Left(ActionError.NotFound("stock", id.value))
      )
    }

  test("invalid stored balances are reported without changing stock"):
    withFixture { f =>
      import f.*
      seed()
      val store = new SqliteInventory(db)
      val id = Id.generate().fold(e => fail(e.toString), identity)
      val ref = inventory.StockId.from(id)
      val receipt = inventory.Receipt
        .from(farmId, "Seed", "kg", "Shed", BigDecimal(5), at)
        .fold(e => fail(e.toString), identity)
      assert(inventory.receive(receipt)(using store, generator(id)).isRight)
      val _ =
        db.execute("UPDATE stock_balances SET quantity = ? WHERE stock_id = ?", Seq("-1", id.value))
      assert(
        inventory.get(ref)(using store).left.exists(_.isInstanceOf[ActionError.PersistenceFailure])
      )
      val adjustment = inventory.Adjustment
        .from(ref, "Shed", BigDecimal(5), at)
        .fold(e => fail(e.toString), identity)
      assert(
        inventory
          .adjust(adjustment)(using store)
          .left
          .exists(_.isInstanceOf[ActionError.PersistenceFailure])
      )
      assertEquals(
        db.queryOne("SELECT quantity FROM stock_balances WHERE stock_id = ?", Seq(id.value))(
          _.getString(1)
        ),
        Some("-1")
      )
    }
