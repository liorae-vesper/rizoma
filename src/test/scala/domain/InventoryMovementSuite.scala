package rizoma.domain

import rizoma.persistence.SqliteInventory
import rizoma.prelude.Id

class InventoryMovementSuite extends BusinessFixture:
  test("receipts, transfers, adjustments and use retain exact quantities and movement history"):
    withFixture { f =>
      import f.*
      seed()
      val store = new SqliteInventory(db)
      val stockId = Id.generate().fold(e => fail(e.toString), identity)
      val input = inventory.Receipt
        .from(farmId, "Compost", "kg", "Shed", BigDecimal("10.125"), at)
        .fold(e => fail(e.toString), identity)
      assertEquals(
        inventory.receive(input)(using store, generator(stockId)),
        Right(inventory.StockId.from(stockId))
      )
      val ref = inventory.StockId.from(stockId)
      val transfer = inventory.Transfer
        .from(ref, "Shed", "Barn", BigDecimal("2.025"), at)
        .fold(e => fail(e.toString), identity)
      assertEquals(inventory.transfer(transfer)(using store), Right(()))
      val use = inventory.Use
        .from(ref, "Barn", BigDecimal("0.025"), at)
        .fold(e => fail(e.toString), identity)
      assertEquals(inventory.use(use)(using store), Right(()))
      val adjustment = inventory.Adjustment
        .from(ref, "Shed", BigDecimal("7.9"), at)
        .fold(e => fail(e.toString), identity)
      assertEquals(inventory.adjust(adjustment)(using store), Right(()))
      val view = inventory
        .get(ref)(using store)
        .fold(e => fail(e.toString), identity)
        .getOrElse(fail("missing stock"))
      assertEquals(
        view.balances.map(b => (b.location, b.quantity)),
        List(("Barn", BigDecimal("2")), ("Shed", BigDecimal("7.9")))
      )
      assertEquals(inventory.history(ref)(using store).map(_.size), Right(4))
      val zeroCount = inventory.Adjustment
        .from(ref, "Empty location", BigDecimal(0), at)
        .fold(e => fail(e.toString), identity)
      assertEquals(inventory.adjust(zeroCount)(using store), Right(()))
      assertEquals(
        inventory.get(ref)(using store).map(_.map(_.balances)),
        Right(Some(view.balances))
      )
      assertEquals(inventory.history(ref)(using store).map(_.size), Right(4))
      assertEquals(inventory.listByFarm(farmId)(using store).map(_.size), Right(1))
      val tooMuch =
        inventory.Use.from(ref, "Barn", BigDecimal("3"), at).fold(e => fail(e.toString), identity)
      assert(inventory.use(tooMuch)(using store).isLeft)
      assertEquals(
        inventory.get(ref)(using store).map(_.map(_.balances)),
        Right(Some(view.balances))
      )
    }

  test("stock calculations retain arbitrary decimal precision"):
    withFixture { f =>
      import f.*
      seed()
      val store = new SqliteInventory(db)
      val id = Id.generate().fold(e => fail(e.toString), identity)
      val amount = BigDecimal("10000000000000000000000000000000000000000.0000000000000000001")
      val receipt = inventory.Receipt
        .from(farmId, "Seed", "kg", "Shed", amount, at)
        .fold(e => fail(e.toString), identity)
      assert(inventory.receive(receipt)(using store, generator(id)).isRight)
      assertEquals(
        inventory
          .get(inventory.StockId.from(id))(using store)
          .map(_.map(_.balances.map(_.quantity))),
        Right(Some(List(amount)))
      )
      val use = inventory.Use
        .from(inventory.StockId.from(id), "Shed", BigDecimal("0.0000000000000000001"), at)
        .fold(e => fail(e.toString), identity)
      assertEquals(inventory.use(use)(using store), Right(()))
      val result = inventory
        .get(inventory.StockId.from(id))(using store)
        .fold(e => fail(e.toString), identity)
        .getOrElse(fail("missing"))
      assertEquals(
        result.balances.map(_.quantity),
        List(BigDecimal("10000000000000000000000000000000000000000"))
      )
    }
