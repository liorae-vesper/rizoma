# Internal code patterns

This guide describes the patterns present in the code today. The application entry point currently
only prints a welcome message; the domain types and JDBC wrapper are building blocks, not a wired
application or persistence implementation.

## Code map

- [`src/main/scala/domain`](../../src/main/scala/domain) contains shared measurement and geometry
  types, with farm, plot, crop species, crop cycle, activity, and harvest in subpackages.
- [`src/main/scala/prelude/Prelude.scala`](../../src/main/scala/prelude/Prelude.scala) defines IDs,
  UUIDv7, and clock and ID-generator interfaces.
- [`src/main/scala/prelude/jdbc/Jdbc.scala`](../../src/main/scala/prelude/jdbc/Jdbc.scala) provides
  synchronous JDBC operations and transactions.
- [`src/test/scala`](../../src/test/scala) mirrors the domain and prelude areas, with shared domain
  generators in [`DomainGenerators.scala`](../../src/test/scala/domain/DomainGenerators.scala).
  Domain suites live in matching directories but use the `rizoma.domain` package, so their
  constructor-access checks run outside the entity subpackages.

The project uses Scala 3 indentation syntax, immutable values, enums, opaque types, companion
objects, and `given` instances. Dependencies and compiler settings are in
[`build.sbt`](../../build.sbt).

## Domain values and commands

Validation lives at construction boundaries. Opaque types distinguish values that have the same
underlying representation, and companion `from` methods normalize or validate raw input. For
example, [`farm.Name.from`](../../src/main/scala/domain/farm/Farm.scala) trims a name and returns an
`Either` with a specific error if it is empty or longer than 100 UTF-16 code units. Plot and crop
species names follow the same pattern. [`Area.from`](../../src/main/scala/domain/Area.scala) and
`harvest.Yield.from` reject non-finite and non-positive quantities, retaining their units without
conversion. Geometry factories use `Option` for invalid values instead of a detailed error enum.

Domain packages commonly group these types. Qualify them with their package, for example
`domain.farm.Registration` or `domain.plot.View`:

- `Registration` represents values needed to create an entity. Factories validate raw fields and
  map value errors into the package's `Constraints` enum. An exception is `plot.Registration`, a
  public case class assembled from already validated values.
- `View` is a snapshot from which `patch` constructs replacement values. Patches keep the entity ID
  and, where applicable, the parent ID from the view. Their constructors are restricted to the
  enclosing domain package. Some fields have defaults from the current view, such as plot status,
  cycle dates, and harvest unit.
- `Destroy` is an ID-bearing removal command where defined. Farm currently has no `Destroy` type.

See [`CropSpecies.scala`](../../src/main/scala/domain/crop/species/CropSpecies.scala) for a compact example of
validation and error mapping, and [`Plot.scala`](../../src/main/scala/domain/plot/Plot.scala) for a
registration built from validated values. These are command data structures: constructing an ID or
command does not check whether a referenced entity exists. Plot registration also does not check
the relationship among farm, geometry, and area; cycle registration does not check containment
within its plot.

[`crop.cycle.Period`](../../src/main/scala/domain/crop/cycle/CropCycle.scala) illustrates state derived from
validated data: no dates means planned, a start without an end means active, and both dates mean
completed. An end requires a start and may equal, but not precede, it. A patch cannot clear an
existing start or end, though dates can be corrected while retaining chronological order.
[`crop.activity`](../../src/main/scala/domain/crop/activity/CropActivity.scala) retains validated event text and
derives a recognized `EventType`; unknown text remains available in `Unknown`.

## Identity and effects

`Id` is an opaque wrapper around the project's UUIDv7 implementation. Entity-specific IDs such as
`PlotId` and `CycleId` are distinct opaque wrappers constructed from an existing `Id`; they express
the kind of reference, not existence in storage. Farm references currently use the general `Id`.
UUID generation and parsing return `Either` errors. `Prelude.Clock` and
`Prelude.IdGenerator[E]` are small interfaces for supplying time and IDs, with `SystemClock` and
`DefaultIdGenerator` as implementations. The default generator obtains its own current time.

## Geometry and serialization

[`Geo.scala`](../../src/main/scala/domain/Geo.scala) models the seven GeoJSON geometry types.
Positions take longitude, latitude, and optional altitude in that order; coordinates and altitude
must be finite, with longitude and latitude in their respective geographic ranges. Private
constructors and companion factories keep invalid values out. Lines require at least two positions;
polygon rings require at least four positions and matching first and last positions, including
altitude. Empty polygons and collections are allowed under their factory contracts. Ring orientation,
self-intersection, and hole placement are not validated.

Circe encoders and decoders live in the corresponding companions. Decoders pass payloads through
the same factories, while `Geometry` dispatches using the GeoJSON `type` field. Collections use
`geometries`; the other geometry types use `coordinates`.

`contains`, `intersects`, and `isNeighbor` delegate to the private JTS adapter in
[`GeometryTopology.scala`](../../src/main/scala/domain/GeometryTopology.scala). They treat longitude
and latitude as planar coordinates, ignore altitude, and do not wrap the antimeridian. Predicate
results are guaranteed only for topologically valid input. Boundary contact counts as intersection
and can count as neighboring; it does not by itself imply containment. The adapter includes a
JTS-specific collection-containment workaround; preserve its rationale and regression coverage when
changing it.

## Database boundary

`Database` obtains a `DataSource` connection per independent operation. `query` decodes all rows,
`queryOne` decodes only the first without enforcing uniqueness or adding a limit, and `execute`
returns the affected row count. Parameters are positional `SqlParam` values; convert domain values
to supported primitive types before binding. Statements, result sets, and owned connections are
closed using `Using.resource`.

Use `transaction` for operations sharing a `SqlSession`: a normal return commits, even if the
returned value is `Left`. Use `transactionEither` when `Left` must roll back and `Right` must commit.
Exceptions propagate and trigger a rollback attempt; JDBC exceptions are not converted into domain
errors. A session must stay within its callback and must not be shared across threads. Nested
transactions on the same session are unsupported. The wrapper restores the prior auto-commit state;
cleanup failures may be suppressed on an original exception, and a failed commit can leave its
outcome uncertain. See [`DatabaseSuite.scala`](../../src/test/scala/prelude/jdbc/DatabaseSuite.scala)
for resource and failure-path examples.

## Tests and checks

Tests use MUnit, with ScalaCheck properties for generated domain values and ordinary examples for
boundaries and regressions. Suites also use `compileErrors` to check that private constructors,
copies, and distinct opaque IDs cannot bypass intended boundaries. JDBC tests use proxy-based test
doubles to observe bindings, resource ownership, and failures; they do not require a database.

Run the test suite with `sbt test`. [`mise.toml`](../../mise.toml) defines `mise run format-check`
and `mise run lint`; the latter compiles main and test sources and checks Scalafix. The pre-commit
hook runs both for relevant Scala and configuration changes. Compiler warnings are treated as errors.
