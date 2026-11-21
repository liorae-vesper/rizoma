package rizoma.domain

import rizoma.domain.harvest.lot
import rizoma.persistence.{SqliteHarvestLots, SqliteInventory}
import rizoma.prelude.Id

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

class InventoryConcurrencySuite extends BusinessFixture:
  test("concurrent uses and lot allocations cannot exceed available quantities"):
    withFixture { f =>
      import f.*
      seed()
      val executor = Executors.newFixedThreadPool(2)
      given ExecutionContext = ExecutionContext.fromExecutor(executor)
      try
        def race[A](action: => A): List[A] =
          val ready = new CountDownLatch(2)
          val start = new CountDownLatch(1)
          val results = List.fill(2)(Future { ready.countDown(); start.await(); action })
          assert(ready.await(5, TimeUnit.SECONDS))
          start.countDown()
          Await.result(Future.sequence(results), 10.seconds)
        val store = new SqliteInventory(db)
        val id = Id.generate().fold(e => fail(e.toString), identity)
        val ref = inventory.StockId.from(id)
        val receipt = inventory.Receipt
          .from(farmId, "Seed", "kg", "Shed", BigDecimal(10), at)
          .fold(e => fail(e.toString), identity)
        assert(inventory.receive(receipt)(using store, generator(id)).isRight)
        val use =
          inventory.Use.from(ref, "Shed", BigDecimal(7), at).fold(e => fail(e.toString), identity)
        val uses = race(inventory.use(use)(using store))
        assertEquals(uses.count(_.isRight), 1)
        assertEquals(
          inventory.get(ref)(using store).map(_.map(_.balances.map(_.quantity))),
          Right(Some(List(BigDecimal(3))))
        )
        val lots = new SqliteHarvestLots(db)
        val harvestRef = harvest.HarvestId.from(harvestId)
        val create = lot.Create
          .from(harvestRef, "Lot", "Store", BigDecimal(3), at)
          .fold(e => fail(e.toString), identity)
        val allocations = race {
          val id = Id.generate().fold(e => fail(e.toString), identity)
          lot.create(create)(using lots, generator(id))
        }
        assertEquals(allocations.count(_.isRight), 1)
        assertEquals(lot.listByHarvest(harvestRef)(using lots).map(_.size), Right(1))
      finally
        val _ = executor.shutdownNow()
    }
