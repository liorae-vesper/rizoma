package rizoma.domain.plot

import io.circe.syntax.*
import org.sqlite.SQLiteDataSource
import rizoma.domain.{Area, MultiPolygon, Polygon, Position, UnitOfArea}
import rizoma.persistence.{SqliteDatabase, SqlitePlots}
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

class PlotCommandsSuite extends munit.FunSuite:
  test("mark, demark and remeasure modify only their target fields"):
    val url = s"jdbc:sqlite:file:${java.util.UUID.randomUUID()}?mode=memory&cache=shared"
    val ds = new SQLiteDataSource()
    ds.setUrl(url)
    val anchor = ds.getConnection()
    try
      val db = SqliteDatabase.open(url)
      SqliteDatabase.initialize(db)
      val farmId = Id.generate().fold(error => fail(error.toString), identity)
      val plotId = Id.generate().fold(error => fail(error.toString), identity)
      val otherId = Id.generate().fold(error => fail(error.toString), identity)
      val _ = db.execute(
        "INSERT INTO farms VALUES (?, ?, ?, ?, ?)",
        Seq(farmId.value, "Farm", "[1,2]", 10.0, "hectares")
      )
      val empty = MultiPolygon.from(Nil).getOrElse(fail("empty geometry"))
      def position(x: Double, y: Double): Position =
        Position.from(x, y).getOrElse(fail("invalid position"))
      val ring = List(position(0, 0), position(1, 0), position(0, 1), position(0, 0))
      val polygon = Polygon.from(ring).getOrElse(fail("invalid polygon"))
      val boundary = MultiPolygon.from(List(polygon)).getOrElse(fail("invalid geometry"))
      val area = Area.from(10, UnitOfArea.Hectares).fold(e => fail(e.toString), identity)
      val newArea = Area.from(22, UnitOfArea.Acres).fold(e => fail(e.toString), identity)
      val name = Name.from("Field").fold(e => fail(e.toString), identity)
      val store = new SqlitePlots(db)
      def ids(id: Id): IdGenerator = new IdGenerator:
        def next(): Either[rizoma.prelude.InvalidGeneration, Id] = Right(id)
      val registration = Registration(empty, area, name, farmId, Status.Resting)
      assertEquals(register(registration)(using store, ids(plotId)), Right(PlotId.from(plotId)))
      assertEquals(register(registration)(using store, ids(otherId)), Right(PlotId.from(otherId)))
      def row(id: Id): Option[(String, Double, String, String, String, String)] =
        db.queryOne(
          "SELECT geometry, area, unit_of_area, name, status, farm_id FROM plots WHERE id = ?",
          Seq(id.value)
        ) { r =>
          (
            r.getString(1),
            r.getDouble(2),
            r.getString(3),
            r.getString(4),
            r.getString(5),
            r.getString(6)
          )
        }
      val originalOther = row(otherId)
      assertEquals(mark(Mark(PlotId.from(plotId), boundary))(using store), Right(()))
      assertEquals(
        row(plotId),
        Some((boundary.asJson.noSpaces, 10.0, "hectares", "Field", "Resting", farmId.value))
      )
      assertEquals(remeasure(Remeasure(PlotId.from(plotId), newArea))(using store), Right(()))
      assertEquals(
        row(plotId),
        Some((boundary.asJson.noSpaces, 22.0, "acres", "Field", "Resting", farmId.value))
      )
      assertEquals(demark(Demark(PlotId.from(plotId)))(using store), Right(()))
      assertEquals(
        row(plotId),
        Some((empty.asJson.noSpaces, 22.0, "acres", "Field", "Resting", farmId.value))
      )
      assertEquals(demark(Demark(PlotId.from(plotId)))(using store), Right(()))
      assertEquals(mark(Mark(PlotId.from(plotId), boundary))(using store), Right(()))
      assertEquals(
        row(plotId),
        Some((boundary.asJson.noSpaces, 22.0, "acres", "Field", "Resting", farmId.value))
      )
      assertEquals(row(otherId), originalOther)
      val missing = PlotId.from(Id.generate().fold(error => fail(error.toString), identity))
      assert(mark(Mark(missing, boundary))(using store).isLeft)
      assert(demark(Demark(missing))(using store).isLeft)
      assert(remeasure(Remeasure(missing, newArea))(using store).isLeft)
    finally anchor.close()
