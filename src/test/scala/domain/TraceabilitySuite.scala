package rizoma.domain

import rizoma.domain.crop.activity
import rizoma.domain.harvest.lot
import rizoma.persistence.{SqliteHarvestLots, SqliteInventory, SqliteTraceability}
import rizoma.prelude.Id

class TraceabilitySuite extends BusinessFixture:
  test("receipt origin and journal inputs trace both directions up to harvest"):
    withFixture { f =>
      import f.*
      seed()
      val stocks = new SqliteInventory(db)
      val lots = new SqliteHarvestLots(db)
      val traces = new SqliteTraceability(db)
      val stockId = Id.generate().fold(e => fail(e.toString), identity)
      val stockRef = inventory.StockId.from(stockId)
      val receipt = inventory.Receipt
        .from(
          farmId,
          "Compost",
          "kg",
          "Shed",
          BigDecimal(10),
          at,
          Some("Supplier"),
          Some("Batch-42")
        )
        .fold(e => fail(e.toString), identity)
      assert(inventory.receive(receipt)(using stocks, generator(stockId)).isRight)
      val link = activity.StockLink.from(stockRef, "Shed").fold(e => fail(e.toString), identity)
      val quantity =
        activity.Quantity.from(BigDecimal(2), "kg").fold(e => fail(e.toString), identity)
      val linked = activity.InputUse
        .from("Compost", quantity, None, None, stock = Some(link))
        .fold(e => fail(e.toString), identity)
      val unlinked =
        activity.InputUse.from("Water", quantity, None, None).fold(e => fail(e.toString), identity)
      val journal = activity.Journal
        .from(None, None, List(linked, unlinked), Nil, Nil)
        .fold(e => fail(e.toString), identity)
      val eventId = Id.generate().fold(e => fail(e.toString), identity)
      val event = activity.Registration
        .from(activityInput.cycleId, "Fertilization", at, journal)
        .fold(e => fail(e.toString), identity)
      assert(activity.record(event)(using activities, generator(eventId)).isRight)
      val lotId = Id.generate().fold(e => fail(e.toString), identity)
      val create = lot.Create
        .from(harvest.HarvestId.from(harvestId), "Lot", "Store", BigDecimal(2), at)
        .fold(e => fail(e.toString), identity)
      assert(lot.create(create)(using lots, generator(lotId)).isRight)
      val report = traceability
        .lot(lot.LotId.from(lotId))(using traces)
        .fold(e => fail(e.toString), identity)
        .getOrElse(fail("missing lot"))
      assertEquals(report.lot.plotId, plot.PlotId.from(plotId))
      assertEquals(report.inputs.size, 2)
      assertEquals(
        report.inputs.find(_.input.name == "Compost").flatMap(_.stock).flatMap(_.supplier),
        Some("Supplier")
      )
      assertEquals(
        report.inputs.find(_.input.name == "Compost").flatMap(_.stock).flatMap(_.supplierBatch),
        Some("Batch-42")
      )
      assertEquals(report.inputs.find(_.input.name == "Water").flatMap(_.stock), None)
      assert(report.hasUnlinkedInputs)
      assert(!report.hasUnknownInputOrigins)
      assertEquals(
        traceability.input(stockRef)(using traces).map(_.map(_.lots.map(_.lot.id))),
        Right(Some(List(lot.LotId.from(lotId))))
      )
      val futureId = Id.generate().fold(e => fail(e.toString), identity)
      val future = activity.Registration
        .from(activityInput.cycleId, "Fertilization", at.plusSeconds(1), journal)
        .fold(e => fail(e.toString), identity)
      assert(activity.record(future)(using activities, generator(futureId)).isRight)
      assertEquals(
        traceability.lot(lot.LotId.from(lotId))(using traces).map(_.map(_.inputs.size)),
        Right(Some(2))
      )
      assertEquals(
        traceability.input(stockRef)(using traces).map(_.map(_.cycles.map(_.activities.size))),
        Right(Some(List(2)))
      )
      val _ = db.execute("DELETE FROM stock_receipt_origins WHERE stock_id = ?", Seq(stockId.value))
      assertEquals(inventory.get(stockRef)(using stocks).map(_.flatMap(_.supplier)), Right(None))
      assertEquals(
        traceability.lot(lot.LotId.from(lotId))(using traces).map(_.map(_.hasUnknownInputOrigins)),
        Right(Some(true))
      )
      val origin = inventory.Origin
        .from(stockRef, Some("New supplier"), Some("Legacy batch"))
        .fold(e => fail(e.toString), identity)
      assertEquals(inventory.recordOrigin(origin)(using stocks), Right(()))
      assertEquals(
        traceability.lot(lot.LotId.from(lotId))(using traces).map(_.map(_.hasUnknownInputOrigins)),
        Right(Some(false))
      )
      val replacement =
        inventory.Origin.from(stockRef, Some("Other"), None).fold(e => fail(e.toString), identity)
      assert(inventory.recordOrigin(replacement)(using stocks).isLeft)
      assert(
        inventory
          .recordOrigin(
            inventory.Origin
              .from(lot.LotId.toStockId(lot.LotId.from(lotId)), Some("Supplier"), None)
              .fold(e => fail(e.toString), identity)
          )(using stocks)
          .isLeft
      )
      assertEquals(
        traceability.input(lot.LotId.toStockId(lot.LotId.from(lotId)))(using traces),
        Right(None)
      )
      assertEquals(
        activity.delete(activity.Delete(activity.ActivityId.from(eventId)))(using activities),
        Right(())
      )
      assertEquals(
        traceability.lot(lot.LotId.from(lotId))(using traces).map(_.map(_.inputs.size)),
        Right(Some(0))
      )
      assertEquals(
        traceability.input(stockRef)(using traces).map(_.map(_.lots.size)),
        Right(Some(0))
      )
      assertEquals(
        inventory.Receipt
          .from(farmId, "x", "kg", "Shed", BigDecimal(1), at, Some(" "), None)
          .isLeft,
        true
      )
      assertEquals(
        traceability.lot(lot.LotId.from(Id.generate().fold(e => fail(e.toString), identity)))(using
          traces
        ),
        Right(None)
      )
      assertEquals(
        traceability.input(
          inventory.StockId.from(Id.generate().fold(e => fail(e.toString), identity))
        )(using traces),
        Right(None)
      )
    }
