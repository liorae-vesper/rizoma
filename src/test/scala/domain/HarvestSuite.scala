package rizoma
package domain

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import java.time.Instant

class HarvestSuite extends munit.ScalaCheckSuite:
  import DomainGenerators.*
  import Harvest.UnitOfYield

  private val amounts = Gen.choose(0.000001, 1_000_000_000.0)
  private val harvestedAt = Instant.parse("2026-03-01T12:00:00Z")

  property("harvest yield accepts positive finite amounts without changing their value"):
    forAll(amounts) { amount =>
      assertEquals(Harvest.Yield.from(amount).map(_.toDouble), Right(amount))
    }

  test("harvest quantities default to kilograms and retain explicit units without conversion"):
    assertEquals(Harvest.Yield.from(12).map(_.unitOfYield), Right(UnitOfYield.Kilograms))
    UnitOfYield.values.foreach { unit =>
      val result = Harvest.Yield.from(12, unit).map { quantity =>
        assertEquals(quantity.toDouble, 12.0)
        assertEquals(quantity.unitOfYield, unit)
      }
      assertEquals(result, Right(()))
    }
    assertNotEquals(
      Harvest.Yield.from(12, UnitOfYield.Kilograms),
      Harvest.Yield.from(12, UnitOfYield.Liters)
    )

  property("harvest registration and patches retain units or explicitly replace them"):
    forAll(ids, amounts) { (id, amount) =>
      UnitOfYield.values.foreach { unit =>
        val result = for
          command <- Harvest.Registration.from(
            CropCycle.CycleId.from(id),
            amount,
            harvestedAt,
            unit
          )
          view = Harvest.View(
            Harvest.HarvestId.from(id),
            command.cycleId,
            command.yieldAmount,
            command.harvestedAt
          )
          unchanged <- view.patch(amount + 1, harvestedAt.plusNanos(1))
          changed <- view.patch(amount + 2, harvestedAt, UnitOfYield.Liters)
        yield
          assertEquals(command.yieldAmount.unitOfYield, unit)
          assertEquals(unchanged.yieldAmount.unitOfYield, unit)
          assertEquals(unchanged.yieldAmount.toDouble, amount + 1)
          assertEquals(changed.yieldAmount.unitOfYield, UnitOfYield.Liters)
          assertEquals(changed.yieldAmount.toDouble, amount + 2)
          assertEquals(changed.harvestId, view.harvestId)
          assertEquals(changed.cycleId, view.cycleId)
        assertEquals(result, Right(()))
      }
    }

  test("every harvest unit uses the same positive finite amount validation"):
    UnitOfYield.values.foreach { unit =>
      assertEquals(Harvest.Yield.from(0, unit), Left(Harvest.Yield.Error.NotGreaterThanZero))
      assertEquals(Harvest.Yield.from(Double.NaN, unit), Left(Harvest.Yield.Error.NotFinite))
    }

  test("harvest yield rejects zero, negative, and non-finite amounts"):
    List(0.0, -0.0, -1.0, -Double.MaxValue).foreach { amount =>
      assertEquals(Harvest.Yield.from(amount), Left(Harvest.Yield.Error.NotGreaterThanZero))
    }
    List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach { amount =>
      assertEquals(Harvest.Yield.from(amount), Left(Harvest.Yield.Error.NotFinite))
    }
    List(java.lang.Double.MIN_VALUE, Double.MaxValue).foreach { amount =>
      assertEquals(Harvest.Yield.from(amount).map(_.toDouble), Right(amount))
    }

  property("harvest registration validates yield and retains the cycle and timestamp"):
    forAll(ids, amounts) { (id, amount) =>
      val cycleId = CropCycle.CycleId.from(id)
      val result = Harvest.Registration.from(cycleId, amount, harvestedAt).map { command =>
        assertEquals(command.cycleId, cycleId)
        assertEquals(command.yieldAmount.toDouble, amount)
        assertEquals(command.harvestedAt, harvestedAt)
      }
      assertEquals(result, Right(()))
      assertEquals(
        Harvest.Registration.from(cycleId, 0, harvestedAt),
        Left(Harvest.Constraints.InvalidYield(Harvest.Yield.Error.NotGreaterThanZero))
      )
      assertEquals(
        Harvest.Registration.from(cycleId, Double.NaN, harvestedAt),
        Left(Harvest.Constraints.InvalidYield(Harvest.Yield.Error.NotFinite))
      )
    }

  property("harvest patches retain harvest and cycle identities and validate replacement yield"):
    forAll(ids, ids, amounts, amounts) { (id, cycle, originalAmount, amount) =>
      val result = for
        original <- Harvest.Yield.from(originalAmount)
        view = Harvest.View(
          Harvest.HarvestId.from(id),
          CropCycle.CycleId.from(cycle),
          original,
          harvestedAt
        )
        patch <- view.patch(amount, harvestedAt.plusNanos(1))
      yield
        assertEquals(patch.harvestId, view.harvestId)
        assertEquals(patch.cycleId, view.cycleId)
        assertEquals(patch.yieldAmount.toDouble, amount)
        assertEquals(patch.harvestedAt, harvestedAt.plusNanos(1))
        assertEquals(
          view.patch(-1, harvestedAt),
          Left(Harvest.Constraints.InvalidYield(Harvest.Yield.Error.NotGreaterThanZero))
        )
        assertEquals(
          view.patch(Double.PositiveInfinity, harvestedAt),
          Left(Harvest.Constraints.InvalidYield(Harvest.Yield.Error.NotFinite))
        )
      assertEquals(result, Right(()))
    }

  test("yield values, harvest identities, and commands cannot bypass their boundaries"):
    assert(compileErrors("val amount: Harvest.Yield = -1.0").nonEmpty)
    assert(compileErrors("Harvest.Yield(-1, UnitOfYield.Kilograms)").nonEmpty)
    assert(compileErrors("Harvest.Yield.from(1).map(_.copy(amount = -1))").nonEmpty)
    assert(compileErrors("""
      for id <- Id.generate() yield
        val harvestId: Harvest.HarvestId = CropCycle.CycleId.from(id)
        harvestId
    """).nonEmpty)
    assert(compileErrors("""
      for
        id <- Id.generate()
        amount <- Harvest.Yield.from(1)
      yield Harvest.Registration(CropCycle.CycleId.from(id), amount, java.time.Instant.EPOCH)
    """).nonEmpty)
    assert(compileErrors("""
      for
        id <- Id.generate()
        amount <- Harvest.Yield.from(1)
      yield Harvest.Patch(
        amount, java.time.Instant.EPOCH, CropCycle.CycleId.from(id), Harvest.HarvestId.from(id)
      )
    """).nonEmpty)
