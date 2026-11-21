# Internal code patterns

This guide describes the patterns present in the code today. The application entry point currently
only prints a welcome message; the business actions and SQLite persistence are not wired into it.

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
- `Delete` is an ID-bearing removal command where defined. Farm currently has no `Delete` type.

See [`CropSpecies.scala`](../../src/main/scala/domain/crop/species/CropSpecies.scala) for a compact example of
validation and error mapping, and [`Plot.scala`](../../src/main/scala/domain/plot/Plot.scala) for a
registration built from validated values. These are command data structures: constructing an ID or
command does not check whether a referenced entity exists. Plot registration does not check
the relationship among farm, geometry, and area; the SQLite stores enforce the cross-entity
lifecycle rules described below.

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
UUID generation returns `Either[InvalidGeneration, UUIDv7]`; parsing returns an `Either` error.
The ID generator returns the same generation error, which registration actions propagate without
calling persistence when generation fails. `Prelude.Clock` and
`Prelude.IdGenerator` are small interfaces for supplying time and IDs, with `SystemClock` and
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

### Migrations, recovery, audit, and access

`SqliteDatabase.initialize` applies numbered schema migrations in a transaction and rejects newer or
inconsistent histories. Version 1 upgrades legacy unversioned databases; subsequent migrations
add access, audit history, and tasks. Migrations do not invent audit history for earlier changes.
`SqliteBackup.backup` creates a consistent SQLite snapshot in a new file, including committed WAL
data. `restore` checks the backup's integrity and schema version and creates a new database file;
neither operation overwrites an existing destination. A failed write can leave the reserved empty
destination for an operator to inspect or remove. Stop application traffic before switching to a
restored database.

SQLite triggers record before/after row values, operation, time, and connection actor for business
tables, memberships, inventory and tasks; audit updates and deletions are blocked by triggers.
`SqliteDatabase.open` uses the `system` actor, and `asUser` assigns a user ID to a connection.
This ID is supplied by trusted application code, not authenticated by SQLite. Database owners can
modify triggers or forge data, so this is an operational history, not tamper-proof storage. Historic
changes predating audit installation are unknown.

The access store supports users and owner/editor/viewer farm membership, last-owner protection,
and permission checks. Only the system actor may register users or claim an unowned farm. The
`withPermission` callback checks the actor's membership in the same transaction, but the callback
itself must scope queries to the authorized farm. Existing stores and the database handle are trusted
low-level interfaces and do **not** automatically authorize reads or writes. They must not be
exposed to a shared service until an authenticated service layer applies these checks to every
operation, scopes every query, and protects system/database access.

### Planned tasks

Tasks have a farm, title, optional due time, assignee, plot and cycle. Assignments require a current
farm member and plot/cycle references must belong to the farm and agree with one another. Tasks
progress from planned or in-progress to completed or cancelled. Completion may atomically record
a cycle activity, including any linked journal stock consumption; a task can also complete without
an activity. Active assignments prevent membership removal, and planned/in-progress task references
prevent deleting the referenced plot or cycle. A completed task keeps its linked activity until the
task itself is deleted.

Business action functions accept validated commands and injected stores. Registration uses an injected
ID generator and returns the new entity ID; edits retain the command's identity, and removal is
available where a domain defines `Delete`. Actions return `ActionError` values for generation,
missing records, dependent records, stale updates, invalid references or state, and storage failures.
SQLite adapters in
`src/main/scala/persistence` use the JDBC `Database` abstraction. Create a database with
`SqliteDatabase.open("jdbc:sqlite:path/to/file.db")`, then call `SqliteDatabase.initialize(db)`
before using the adapters. The factory enables foreign keys on each connection. A caller supplying
its own `Database` must also enable foreign keys for every connection. Deletion marks rows as deleted;
initialization adds the flag to existing tables while preserving their data. Deleted rows are excluded
from species reads and cannot be edited or used as active parents. Parent deletion is rejected
while active dependent rows exist; editing or deleting a missing or deleted row returns an error. Geometry is stored
as GeoJSON and timestamps as ISO-8601 strings, preserving their precision. Because each database
operation opens a connection, a plain `:memory:` URL will not persist across operations; use a
file-backed database, or keep a shared in-memory database connection open for its lifetime.

Plots also support `Mark` to replace a boundary, `Demark` to clear it, and `Remeasure` to replace
only the area and its unit. An empty GeoJSON multipolygon represents a cleared boundary; it is
indistinguishable from an explicitly supplied empty multipolygon. These operations update only the
fields they name and return an error when the plot does not exist.

Plot and crop-cycle patches retain the snapshot used to prepare them. SQLite applies a patch only
while the editable stored values still match that snapshot, otherwise returning a conflict. This
prevents a stale plot edit from undoing a focused change and a stale cycle edit from reopening a
finished cycle. Snapshot matching uses the stored canonical geometry and timestamp representations.

Plot `activate` and `resting` update only status. Crop-cycle `plant` creates a started cycle;
`harvest` records a validated yield using the harvest store without ending the cycle. `finish`
ends an active cycle at or after its start and its recorded harvests, and rejects missing, planned,
or already finished cycles. A new harvest requires an active cycle and cannot predate planting.
Existing harvests may be corrected after completion only within the cycle period. Cycle date
corrections cannot put existing harvests outside that period.

When both plot boundary and occupied land are known, planned and active cycle land must fit inside
the plot, including when creating or editing a cycle and when marking or editing a plot. Empty
geometry represents an unknown boundary or occupied area, so containment cannot be checked until
both are supplied; demarking is allowed. Completed cycles retain historical land and do not prevent
later boundary changes. SQLite checks and writes these relationships in the same transaction.
Topology must be valid; spatial checks are planar and do not wrap the antimeridian. Existing data
is not migrated or retroactively validated.

Species `get` and `list` return validated views and report malformed stored rows; `remove`
uses the same referential-integrity rules as `delete`. Activity `record` registers an event.

Crop activities also hold an optional structured field journal, informed by the FAO
[farm business analysis field guide](https://www.fao.org/uploads/media/4-BenchmarkingInternLores.pdf)
(record-keeping section). A journal captures an operator and notes, applied inputs (quantity, unit,
method, source, and optional total cost), labor (worker, hours, and optional total cost), and outputs
(quantity, unit, disposition, and optional total value). Amounts use `BigDecimal`; units are retained
as entered without conversion, and money uses a three-letter currency code. Journal outputs are
records in their own right and do not automatically create harvests, harvest lots, or sales.

`activity.get` and `activity.listByCycle` read validated journal views; lists are ordered by occurrence
time and ID, and exclude soft-deleted activities. Existing rows without journal data read as an empty
journal. Initialization adds the nullable journal column to older SQLite databases without rewriting
their activities. Activity patches preserve the view's journal unless a replacement is provided, and
reject a stale snapshot rather than overwriting another edit.

## Inventory and harvest lots

An `inventory.Receipt` creates a farm-owned input stock batch, identified by its own `StockId`,
with a name, exact unit, initial location, quantity and occurrence time. `inventory.use`, `transfer`,
and `adjust` update location balances transactionally and append movements; an adjustment supplies
the observed balance and its movement records the signed difference. `get`, `listByFarm`, and `history`
return validated stock views and sequence-ordered movements. Quantities are stored as decimal text and
calculated without rounding. Units and locations are case-sensitive strings, and no conversion or
automatic matching between separately received batches is performed.

An activity input can optionally carry an `activity.StockLink` to an input batch and location. Its
recorded name and unit must match the stock exactly, and the stock must belong to the activity's farm.
Recording the activity and consuming all linked inputs is atomic. Editing or deleting the activity
reconciles linked consumption in the same transaction, retaining reversal and use movements.
Unlinked journal inputs remain descriptive and do not affect balances.

`harvest.lot.create` allocates part of an existing harvest to a named lot and initial location. The
allocation cannot exceed the harvest quantity, including earlier allocations. A lot exposes its
harvest, cycle, plot, farm, and species provenance; cycle, plot and species are snapshotted when the
lot is created. Its `StockId` supports the same use, transfer, adjustment and history operations as
other stock, with the total balance capped by the allocated quantity. Once lots exist, the source
harvest cannot be edited or deleted. SQLite initialization creates the inventory tables and backfills
origin snapshots for any existing harvest stock. Concurrent write attempts are protected against
overconsumption and over-allocation; a SQLite lock failure is returned to the caller.

## Input and harvest traceability

Input receipts can include a supplier and supplier batch identifier. Older receipts remain readable
with unknown origin. `inventory.recordOrigin` fills missing details later; it does not replace an
existing value. `traceability.lot` reads a lot, its harvest date, and the active journal activities
in its cycle through that date, including linked input batches and their receipt origin. Its
`hasUnlinkedInputs` and `hasUnknownInputOrigins` flags identify gaps in the recorded inputs.
`traceability.input` finds active activities using a batch, grouped by cycle, and the lots harvested
after those activities. It includes affected cycles even if no lots have been recorded.

These links identify potentially affected lots in a cycle, not proof that a particular application
reached every lot. Both reports reflect the current active journal: edits and soft deletions can
change the result, although the stock ledger retains consumption and reversal movements. Unrecorded
activities cannot be detected, and unknown flags cover only inputs that were recorded. The reports do
not track customers, sales, processing, or recalls. This approach is informed by the FAO's
[field traceability guidance](https://www.fao.org/4/i2385e/i2385e02.pdf), not a claim of regulatory
or certification compliance.

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
and [`DatabaseTransactionSuite.scala`](../../src/test/scala/prelude/jdbc/DatabaseTransactionSuite.scala)
for resource and failure-path examples.

## Tests and checks

Tests use MUnit, with ScalaCheck properties for generated domain values and ordinary examples for
boundaries and regressions. Suites also use `compileErrors` to check that private constructors,
copies, and distinct opaque IDs cannot bypass intended boundaries. JDBC tests use proxy-based test
doubles to observe bindings, resource ownership, and failures; they do not require a database.

Run the test suite with `sbt test`. [`mise.toml`](../../mise.toml) defines `mise run format-check`
and `mise run lint`; the latter compiles main and test sources and checks Scalafix. The pre-commit
hook runs both for relevant Scala and configuration changes. Compiler warnings are treated as errors.
