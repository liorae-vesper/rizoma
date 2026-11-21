package rizoma.domain

import rizoma.domain.harvest.lot
import rizoma.persistence.{SqliteHarvestLots, SqliteInventory}
import rizoma.prelude.Id

class HarvestLotSuite extends BusinessFixture:
  test("harvest lots retain provenance and cannot exceed the recorded harvest"):
    withFixture { f =>
      import f.*
      seed()
      val lots = new SqliteHarvestLots(db)
      val stock = new SqliteInventory(db)
      val lotId = Id.generate().fold(e => fail(e.toString), identity)
      val harvestRef = harvest.HarvestId.from(harvestId)
      val command = lot.Create
        .from(harvestRef, "Bin A", "Store", BigDecimal("3"), at)
        .fold(e => fail(e.toString), identity)
      assertEquals(lot.create(command)(using lots, generator(lotId)), Right(lot.LotId.from(lotId)))
      val view = lot
        .get(lot.LotId.from(lotId))(using lots)
        .fold(e => fail(e.toString), identity)
        .getOrElse(fail("missing lot"))
      assertEquals(view.harvestId, harvestRef)
      assertEquals(view.cycleId, rizoma.domain.crop.cycle.CycleId.from(cycleId))
      assertEquals(view.plotId, plot.PlotId.from(plotId))
      assertEquals(view.speciesId, rizoma.domain.crop.species.SpeciesId.from(speciesId))
      assertEquals(view.stock.farmId, farmId)
      assertEquals(view.stock.harvestId, Some(harvestRef))
      assertEquals(lot.LotId.fromStockId(view.stock.id), lot.LotId.from(lotId))
      assertEquals(view.stock.unit, "Pounds")
      assertEquals(view.stock.balances.map(_.quantity), List(BigDecimal(3)))
      assertEquals(lot.listByHarvest(harvestRef)(using lots).map(_.size), Right(1))
      val tooMuch = lot.Create
        .from(harvestRef, "Bin B", "Store", BigDecimal("2.1"), at)
        .fold(e => fail(e.toString), identity)
      assert(
        lot
          .create(tooMuch)(using
            lots,
            generator(Id.generate().fold(e => fail(e.toString), identity))
          )
          .isLeft
      )
      assertEquals(lot.listByHarvest(harvestRef)(using lots).map(_.size), Right(1))
      val secondId = Id.generate().fold(e => fail(e.toString), identity)
      val remaining = lot.Create
        .from(harvestRef, "Bin B", "Store", BigDecimal(2), at)
        .fold(e => fail(e.toString), identity)
      assertEquals(
        lot.create(remaining)(using lots, generator(secondId)),
        Right(lot.LotId.from(secondId))
      )
      assertEquals(lot.listByHarvest(harvestRef)(using lots).map(_.size), Right(2))
      val move = inventory.Transfer
        .from(lot.LotId.toStockId(lot.LotId.from(lotId)), "Store", "Cold room", BigDecimal(1), at)
        .fold(e => fail(e.toString), identity)
      assertEquals(inventory.transfer(move)(using stock), Right(()))
      val excess = inventory.Adjustment
        .from(lot.LotId.toStockId(lot.LotId.from(lotId)), "Store", BigDecimal(3), at)
        .fold(e => fail(e.toString), identity)
      assert(inventory.adjust(excess)(using stock).isLeft)
      assert(harvest.delete(harvest.Delete(harvestRef))(using harvests).isLeft)
      val patch = harvest
        .View(harvestRef, harvestInput.cycleId, harvestInput.yieldAmount, at)
        .patch(4, at)
        .fold(e => fail(e.toString), identity)
      assert(harvest.edit(patch)(using harvests).isLeft)
      val replacementSpecies = Id.generate().fold(e => fail(e.toString), identity)
      val speciesInput2 = rizoma.domain.crop.species.Registration
        .from("Bean", "Phaseolus vulgaris")
        .fold(e => fail(e.toString), identity)
      assert(
        rizoma.domain.crop.species
          .register(speciesInput2)(using speciesStore, generator(replacementSpecies))
          .isRight
      )
      val cycleView = rizoma.domain.crop.cycle.View(
        rizoma.domain.crop.cycle.CycleId.from(cycleId),
        rizoma.domain.crop.species.SpeciesId.from(speciesId),
        plot.PlotId.from(plotId),
        polygon,
        cycleInput.period
      )
      val cyclePatch = cycleView
        .patch(rizoma.domain.crop.species.SpeciesId.from(replacementSpecies), polygon)
        .fold(e => fail(e.toString), identity)
      assertEquals(rizoma.domain.crop.cycle.edit(cyclePatch)(using cycles), Right(()))
      rizoma.persistence.SqliteDatabase.initialize(db)
      assertEquals(
        lot.get(lot.LotId.from(lotId))(using lots).map(_.map(_.speciesId)),
        Right(Some(rizoma.domain.crop.species.SpeciesId.from(speciesId)))
      )
      assert(lot.Create.from(harvestRef, " ", "Store", BigDecimal(1), at).isLeft)
      assert(lot.Create.from(harvestRef, "Lot", "Store", BigDecimal(0), at).isLeft)
      val missing = harvest.HarvestId.from(Id.generate().fold(e => fail(e.toString), identity))
      val missingCommand = lot.Create
        .from(missing, "Lot", "Store", BigDecimal(1), at)
        .fold(e => fail(e.toString), identity)
      assertEquals(
        lot.create(missingCommand)(using
          lots,
          generator(Id.generate().fold(e => fail(e.toString), identity))
        ),
        Left(ActionError.InvalidReference("harvest"))
      )
      assertEquals(
        lot.get(lot.LotId.from(Id.generate().fold(e => fail(e.toString), identity)))(using lots),
        Right(None)
      )
      val _ =
        db.execute("UPDATE stock_items SET capacity = ? WHERE id = ?", Seq("-1", secondId.value))
      val secondStock = lot.LotId.toStockId(lot.LotId.from(secondId))
      assert(
        inventory
          .get(secondStock)(using stock)
          .left
          .exists(_.isInstanceOf[ActionError.PersistenceFailure])
      )
      val badAdjustment = inventory.Adjustment
        .from(secondStock, "Store", BigDecimal(1), at)
        .fold(e => fail(e.toString), identity)
      assert(
        inventory
          .adjust(badAdjustment)(using stock)
          .left
          .exists(_.isInstanceOf[ActionError.PersistenceFailure])
      )
    }
