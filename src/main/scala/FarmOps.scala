package rizoma

import domain.*

object FarmOps:
  trait FarmWriter:
    def registerFarm(input: RegisterFarm): Either[RegisterFarmError, Unit]

  trait FarmReader:
    def findFarm(id: Id): Option[Farm]
