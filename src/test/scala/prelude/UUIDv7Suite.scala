package rizoma
package prelude

import java.security.SecureRandom
import java.time.Instant

import prelude.{DefaultIdGenerator, Id, UUIDv7}

@SuppressWarnings(Array("scalafix:DisableSyntax"))
class UUIDv7Suite extends munit.FunSuite:
  private def generated(timestamp: Instant): UUIDv7 =
    UUIDv7.generate(timestamp).fold(error => fail(error.toString), identity)

  test("generation stores the timestamp, version, and RFC variant"):
    val timestamp = 1_700_000_000_123L
    val uuid = generated(Instant.ofEpochMilli(timestamp))
    assertEquals(uuid.timestampMillis, timestamp)
    assertEquals(uuid.version, 7)
    assertEquals(uuid.variant, 2)

  test("generation accepts the largest 48-bit timestamp"):
    val timestamp = 0x0000ffffffffffffL

    assertEquals(
      generated(Instant.ofEpochMilli(timestamp)).timestampMillis,
      timestamp
    )

  test(
    "generation accepts timestamps just after the Unix epoch and fractional milliseconds at the upper boundary"
  ):
    val maximum = 0x0000ffffffffffffL
    assertEquals(generated(Instant.EPOCH.plusNanos(1)).timestampMillis, 0L)
    assertEquals(
      generated(Instant.ofEpochMilli(maximum).plusNanos(999_999)).timestampMillis,
      maximum
    )

  test("generation rejects timestamps outside the UUIDv7 range"):
    List(
      Instant.ofEpochMilli(-1),
      Instant.EPOCH.minusNanos(1),
      Instant.EPOCH,
      Instant.ofEpochMilli(0x0001000000000000L),
      Instant.MIN,
      Instant.MAX
    ).foreach { timestamp =>
      assertEquals(UUIDv7.generate(timestamp), Left(InvalidGeneration(None)))
    }

  test("generation retains failures from its random source"):
    val failure = new IllegalStateException("random source unavailable")
    val random = new SecureRandom():
      override def nextInt(bound: Int): Int = throw failure
    val result = UUIDv7.generate(Instant.ofEpochMilli(1), random)
    assertEquals(result, Left(InvalidGeneration(Some(failure))))

  test("UUIDv7 ordering follows timestamps across the signed Long boundary"):
    val beforeBoundary = generated(Instant.ofEpochMilli(0x00007fffffffffffL))
    val afterBoundary = generated(Instant.ofEpochMilli(0x0000800000000000L))
    assertEquals(
      List(afterBoundary, beforeBoundary).sorted,
      List(beforeBoundary, afterBoundary)
    )

  test("a generated UUIDv7 round-trips through its canonical string"):
    val uuid = generated(Instant.ofEpochMilli(1_700_000_000_123L))
    assertEquals(uuid.toString.length, 36)
    assertEquals(UUIDv7.parse(uuid.toString), Right(uuid))

  test("equal UUIDv7 values have matching hashes and deduplicate in hashed collections"):
    val uuid = generated(Instant.ofEpochMilli(1_700_000_000_123L))
    val result = UUIDv7.parse(uuid.toString).map { parsed =>
      assertEquals(parsed, uuid)
      assertEquals(parsed.hashCode, uuid.hashCode)
      assertEquals(Set(uuid, parsed).size, 1)
    }

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
    val id = Id.generate().fold(error => fail(error.toString), identity)
    assertEquals(id.toUUIDv7.version, 7)
    assertEquals(Id.parse(id.toString), Right(id))

  test("the default ID generator returns an Id backed by UUIDv7"):
    assertEquals(DefaultIdGenerator.next().map(_.toUUIDv7.version), Right(7))
