package rizoma
package domain

import org.scalacheck.Gen
import rizoma.prelude.Id

private[domain] object DomainGenerators:
  val names: Gen[String] =
    for
      length <- Gen.choose(1, 100)
      characters <- Gen.listOfN(length, Gen.alphaNumChar)
    yield characters.mkString

  val padding: Gen[String] =
    for
      length <- Gen.choose(0, 120)
      characters <- Gen.listOfN(length, Gen.oneOf(' ', '\t', '\n', '\r', '\u0000'))
    yield characters.mkString

  val longNames: Gen[String] =
    for
      length <- Gen.choose(101, 300)
      characters <- Gen.listOfN(length, Gen.alphaNumChar)
    yield characters.mkString

  val invalidNames: Gen[String] = Gen.oneOf(padding, longNames)

  private def valid[A](value: Option[A]): Gen[A] =
    value match
      case Some(result) => Gen.const(result)
      case None         => Gen.fail[A]

  private def valid[E, A](value: Either[E, A]): Gen[A] =
    value match
      case Right(result) => Gen.const(result)
      case Left(_)       => Gen.fail[A]

  val positions: Gen[Position] =
    for
      latitude <- Gen.choose(-90.0, 90.0)
      longitude <- Gen.choose(-180.0, 180.0)
      altitude <- Gen.option(Gen.choose(-1_000.0, 10_000.0))
      position <- valid(
        Position.from(longitude = longitude, latitude = latitude, altitude = altitude)
      )
    yield position

  val areas: Gen[Area] =
    for
      magnitude <- Gen.choose(0.000001, 1_000_000_000.0)
      unit <- Gen.oneOf(UnitOfArea.values.toSeq)
      area <- valid(Area.from(magnitude, unit))
    yield area

  val ids: Gen[Id] =
    for
      timestamp <- Gen.choose(0L, 0xffffffffL)
      id <- valid(Id.parse(f"$timestamp%08x-0000-7000-8000-000000000000"))
    yield id

  val polygons: Gen[Polygon] =
    for
      latitude <- Gen.choose(-89.0, 89.0)
      longitude <- Gen.choose(-179.0, 179.0)
      first <- valid(Position.from(longitude = longitude, latitude = latitude))
      second <- valid(Position.from(longitude = longitude, latitude = latitude + 0.5))
      third <- valid(Position.from(longitude = longitude + 0.5, latitude = latitude))
      result <- valid(Polygon.from(List(first, second, third, first)))
    yield result

  val geometries: Gen[MultiPolygon] =
    for
      count <- Gen.choose(0, 3)
      members <- Gen.listOfN(count, polygons)
      geometry <- valid(MultiPolygon.from(members))
    yield geometry
