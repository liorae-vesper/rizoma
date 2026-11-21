package rizoma
package domain

import io.circe.parser.decode

private[domain] trait GeometryRelationsFixture extends munit.FunSuite:
  protected def geometry(kind: String, coordinates: String): Geometry =
    decode[Geometry](s"""{"type":"$kind","coordinates":$coordinates}""")
      .fold(error => fail(error.getMessage), identity)

  protected def point(x: Double, y: Double): Geometry = geometry("Point", s"[$x,$y]")

  protected def collection(members: Geometry*): Geometry =
    GeometryCollection.from(members.toList).getOrElse(fail("Invalid geometry collection"))

  protected val square: Geometry = geometry("Polygon", "[[[0,0],[10,0],[10,10],[0,10],[0,0]]]")
  protected val inside: Geometry = geometry("Polygon", "[[[2,2],[4,2],[4,4],[2,4],[2,2]]]")
  protected val outside: Geometry =
    geometry("Polygon", "[[[20,20],[30,20],[30,30],[20,30],[20,20]]]")
  protected val withHole: Geometry = geometry(
    "Polygon",
    "[[[0,0],[10,0],[10,10],[0,10],[0,0]],[[3,3],[3,7],[7,7],[7,3],[3,3]]]"
  )
