package rizoma
package prelude

import java.time.Instant

import prelude.{DefaultIdGenerator, Id, UUIDv7}

class UUIDv7Suite extends munit.FunSuite:
  test("generation stores the timestamp, version, and RFC variant"):
    val timestamp = 1_700_000_000_123L
    val result = for uuid <- UUIDv7.generate(Instant.ofEpochMilli(timestamp)) yield
      assertEquals(uuid.timestampMillis, timestamp)
      assertEquals(uuid.version, 7)
      assertEquals(uuid.variant, 2)

    assertEquals(result, Right(()))

  test("generation accepts the largest 48-bit timestamp"):
    val timestamp = 0x0000ffffffffffffL

    assertEquals(
      UUIDv7.generate(Instant.ofEpochMilli(timestamp)).map(_.timestampMillis),
      Right(timestamp)
    )

  test("generation accepts the Unix epoch and fractional milliseconds at the upper boundary"):
    val maximum = 0x0000ffffffffffffL
    assertEquals(UUIDv7.generate(Instant.EPOCH).map(_.timestampMillis), Right(0L))
    assertEquals(
      UUIDv7.generate(Instant.ofEpochMilli(maximum).plusNanos(999_999)).map(_.timestampMillis),
      Right(maximum)
    )

  test("generation rejects timestamps outside the UUIDv7 range"):
    List(
      Instant.ofEpochMilli(-1),
      Instant.EPOCH.minusNanos(1),
      Instant.ofEpochMilli(0x0001000000000000L),
      Instant.MIN,
      Instant.MAX
    ).foreach { timestamp =>
      assertEquals(UUIDv7.generate(timestamp), Left(UUIDv7.GenerationError.TimestampOutOfRange))
    }

  test("UUIDv7 ordering follows timestamps across the signed Long boundary"):
    val result = for
      beforeBoundary <- UUIDv7.generate(Instant.ofEpochMilli(0x00007fffffffffffL))
      afterBoundary <- UUIDv7.generate(Instant.ofEpochMilli(0x0000800000000000L))
    yield assertEquals(
      List(afterBoundary, beforeBoundary).sorted,
      List(beforeBoundary, afterBoundary)
    )

    assertEquals(result, Right(()))

  test("a generated UUIDv7 round-trips through its canonical string"):
    val result = for uuid <- UUIDv7.generate(Instant.ofEpochMilli(1_700_000_000_123L)) yield
      assertEquals(uuid.toString.length, 36)
      assertEquals(UUIDv7.parse(uuid.toString), Right(uuid))

    assertEquals(result, Right(()))

  test("equal UUIDv7 values have matching hashes and deduplicate in hashed collections"):
    val result = for
      uuid <- UUIDv7.generate(Instant.ofEpochMilli(1_700_000_000_123L))
      parsed <- UUIDv7.parse(uuid.toString)
    yield
      assertEquals(parsed, uuid)
      assertEquals(parsed.hashCode, uuid.hashCode)
      assertEquals(Set(uuid, parsed).size, 1)

    assertEquals(result, Right(()))

  test("parsing accepts uppercase hexadecimal digits"):
    val result = for
      expected <- UUIDv7.parse("00000000-0000-7000-8000-000000000000")
      parsed <- UUIDv7.parse(expected.toString.toUpperCase)
    yield assertEquals(parsed, expected)

    assertEquals(result, Right(()))

  test("parsing rejects malformed strings"):
    assertEquals(UUIDv7.parse("not-a-uuid"), Left(UUIDv7.ParseError.InvalidFormat))

  test("parsing rejects UUIDs with a different version"):
    assertEquals(
      UUIDv7.parse("00000000-0000-4000-8000-000000000000"),
      Left(UUIDv7.ParseError.InvalidVersion)
    )

  test("parsing rejects UUIDs with a different variant"):
    assertEquals(
      UUIDv7.parse("00000000-0000-7000-0000-000000000000"),
      Left(UUIDv7.ParseError.InvalidVariant)
    )

  test("Id generation and parsing preserve the UUIDv7 value"):
    val result = for id <- Id.generate() yield
      assertEquals(id.toUUIDv7.version, 7)
      assertEquals(Id.parse(id.toString), Right(id))

    assertEquals(result, Right(()))

  test("the default ID generator returns an Id backed by UUIDv7"):
    assertEquals(DefaultIdGenerator.next().map(_.toUUIDv7.version), Right(7))
