package rizoma.persistence

import rizoma.domain.ActionError
import rizoma.domain.crop.species
import rizoma.prelude.Id
import rizoma.prelude.jdbc.Database

import java.sql.ResultSet
import scala.util.{Failure, Success, Try}

final class SqliteSpecies(db: Database)
    extends SqliteWrites(db, "species", "species")
    with species.Store:
  def initialize(): Unit = db.transaction(session => initialize(session))
  private[persistence] def initialize(session: rizoma.prelude.jdbc.SqlSession): Unit = schema(
    session,
    """CREATE TABLE IF NOT EXISTS species (
    id TEXT PRIMARY KEY, common_name TEXT NOT NULL, scientific_name TEXT NOT NULL
  )"""
  )
  def insert(id: Id, input: species.Registration): Try[Unit] = write(
    "INSERT INTO species (id, common_name, scientific_name) VALUES (?, ?, ?)",
    Seq(id.value, input.commonName.toString, input.scientificName.toString)
  )
  def update(input: species.Patch): Try[Unit] = writeForId(
    "UPDATE species SET common_name = ?, scientific_name = ? WHERE id = ? AND deleted = 0",
    Seq(input.commonName.toString, input.scientificName.toString, input.speciesId.toString),
    input.speciesId.toString
  )
  def delete(input: species.Delete): Try[Unit] =
    writeExisting(
      "UPDATE species SET deleted = 1 WHERE id = ? AND deleted = 0 AND NOT EXISTS (SELECT 1 FROM cycles WHERE cycles.species_id = species.id AND cycles.deleted = 0)",
      Seq(input.speciesId.toString),
      input.speciesId.toString,
      _ => ActionError.HasDependents("species", input.speciesId.toString)
    )

  private def decode(row: ResultSet): Either[String, species.View] =
    for
      id <- Id.parse(row.getString("id")).left.map(_.toString)
      common <- species.CommonName.from(row.getString("common_name")).left.map(_.toString)
      scientific <- species.ScientificName
        .from(row.getString("scientific_name"))
        .left
        .map(_.toString)
    yield species.View(species.SpeciesId.from(id), common, scientific)

  private def validated(value: Either[String, species.View]): Try[species.View] =
    value.fold(
      error => Failure(new IllegalStateException(s"Invalid stored species: $error")),
      Success(_)
    )

  def get(id: species.SpeciesId): Try[Option[species.View]] =
    Try(
      db.queryOne(
        "SELECT id, common_name, scientific_name FROM species WHERE id = ? AND deleted = 0",
        Seq(id.toString)
      )(decode)
    )
      .flatMap {
        case Some(value) => validated(value).map(Some(_))
        case None        => Success(None)
      }

  def list(): Try[List[species.View]] =
    Try(
      db.query(
        "SELECT id, common_name, scientific_name FROM species WHERE deleted = 0 ORDER BY common_name, id"
      )(
        decode
      )
    )
      .flatMap(_.foldLeft[Try[List[species.View]]](Success(Nil)) { (result, value) =>
        for rows <- result; row <- validated(value) yield row :: rows
      }.map(_.reverse))
