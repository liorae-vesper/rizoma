package rizoma

import domain.*

object FarmOps:
  trait FarmWriter:
    def registerFarm(input: Farm.Register): Either[Farm.Constraints, Unit]

    def patchFarm(input: Farm.Patch): Either[Farm.Constraints, Unit]

  trait FarmReader:
    def findFarm(id: Id): Option[Farm.View]
