package rizoma.domain

import org.sqlite.SQLiteDataSource
import rizoma.persistence.{SqliteDatabase, SqlitePlots}
import rizoma.prelude.Id
import rizoma.prelude.Prelude.IdGenerator

class PlotStatusActionsSuite extends munit.FunSuite:
  test("plot status commands persist changes and reject missing plots"):
    val url = s"jdbc:sqlite:file:${java.util.UUID.randomUUID()}?mode=memory&cache=shared"
    val ds = new SQLiteDataSource()
    ds.setUrl(url)
    val anchor = ds.getConnection()
    try
      val db = SqliteDatabase.open(url)
      SqliteDatabase.initialize(db)
      val plots = new SqlitePlots(db)
      def ids(id: Id): IdGenerator = new IdGenerator:
        def next(): Either[rizoma.prelude.InvalidGeneration, Id] = Right(id)
      val farmId = Id.generate().fold(error => fail(error.toString), identity)
      val plotId = Id.generate().fold(error => fail(error.toString), identity)
      val _ = db.execute(
        "INSERT INTO farms VALUES (?, ?, ?, ?, ?)",
        Seq(farmId.value, "Farm", "[0,0]", 1.0, "acres")
      )
      val geometry = MultiPolygon.from(Nil).getOrElse(fail("geometry"))
      val area = Area.from(1).fold(e => fail(e.toString), identity)
      val name = plot.Name.from("North").fold(e => fail(e.toString), identity)
      assertEquals(
        plot.register(plot.Registration(geometry, area, name, farmId))(using plots, ids(plotId)),
        Right(plot.PlotId.from(plotId))
      )
      assertEquals(plot.resting(plot.Resting(plot.PlotId.from(plotId)))(using plots), Right(()))
      assertEquals(db.queryOne("SELECT status FROM plots")(_.getString(1)), Some("Resting"))
      assertEquals(plot.activate(plot.Activate(plot.PlotId.from(plotId)))(using plots), Right(()))
      assertEquals(db.queryOne("SELECT status FROM plots")(_.getString(1)), Some("Active"))
      assert(
        plot
          .activate(
            plot.Activate(
              plot.PlotId.from(Id.generate().fold(error => fail(error.toString), identity))
            )
          )(using plots)
          .isLeft
      )
    finally anchor.close()
