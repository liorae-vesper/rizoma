package rizoma
package domain

private[domain] trait GeometryFixture extends munit.FunSuite:
  protected val fixture: Option[(Position, List[Position])] = for
    position <- Position.from(longitude = 10, latitude = 20)
    second <- Position.from(longitude = 11, latitude = 21)
    third <- Position.from(longitude = 12, latitude = 20)
  yield (position, List(position, second, third, position))
