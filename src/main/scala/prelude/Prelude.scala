package rizoma
package prelude

import java.security.SecureRandom
import java.time.Instant
import scala.util.Try

/** UUID generation failed, optionally retaining the underlying exception. */
final case class InvalidGeneration(throwable: Option[Throwable])
    extends RuntimeException("Unable to generate a UUIDv7", throwable.orNull)

object Prelude:
  trait IdGenerator:
    def next(): Either[InvalidGeneration, Id]

  trait Clock:
    def now(): Instant

object SystemClock extends Prelude.Clock:
  override def now(): Instant = Instant.now()

object DefaultIdGenerator extends Prelude.IdGenerator:
  override def next(): Either[InvalidGeneration, Id] = Id.generate()

/** A UUID using the time-ordered UUID version 7 layout. */
final class UUIDv7 private (
    val mostSignificantBits: Long,
    val leastSignificantBits: Long
):
  /** The Unix timestamp, in milliseconds, stored in this UUID. */
  def timestampMillis: Long = (mostSignificantBits >>> 16) & UUIDv7.TimestampMask

  /** The UUID version number, always 7. */
  def version: Int = ((mostSignificantBits >>> 12) & 0xfL).toInt

  /** The RFC 4122/9562 variant value, always 2. */
  def variant: Int = (leastSignificantBits >>> 62).toInt

  override def equals(other: Any): Boolean = other match
    case that: UUIDv7 =>
      mostSignificantBits == that.mostSignificantBits && leastSignificantBits == that.leastSignificantBits
    case _ => false

  override def hashCode(): Int =
    31 * java.lang.Long.hashCode(mostSignificantBits) + java.lang.Long.hashCode(
      leastSignificantBits
    )

  override def toString: String =
    s"${UUIDv7.hex(mostSignificantBits >>> 32, 8)}-" +
      s"${UUIDv7.hex((mostSignificantBits >>> 16) & 0xffffL, 4)}-" +
      s"${UUIDv7.hex(mostSignificantBits & 0xffffL, 4)}-" +
      s"${UUIDv7.hex(leastSignificantBits >>> 48, 4)}-" +
      s"${UUIDv7.hex(leastSignificantBits & 0xffffffffffffL, 12)}"

object UUIDv7:

  /** Errors returned when parsing a UUIDv7 string. */
  enum ParseError:
    case InvalidFormat
    case InvalidVersion
    case InvalidVariant

  private val TimestampMask = 0x0000ffffffffffffL
  private val TimestampUpperBound = Instant.ofEpochMilli(TimestampMask + 1)
  private val RandomBMask = 0x3fffffffffffffffL
  private lazy val random = new SecureRandom()

  given Ordering[UUIDv7] with
    override def compare(left: UUIDv7, right: UUIDv7): Int =
      val mostSignificantComparison = java.lang.Long.compareUnsigned(
        left.mostSignificantBits,
        right.mostSignificantBits
      )
      if mostSignificantComparison != 0 then mostSignificantComparison
      else java.lang.Long.compareUnsigned(left.leastSignificantBits, right.leastSignificantBits)

  /** Creates a UUIDv7 using the current Unix time, returning an error outside the 48-bit range. */
  def generate(): Either[InvalidGeneration, UUIDv7] =
    Try(Instant.now()).toEither.left.map(error => InvalidGeneration(Some(error))).flatMap(generate)

  /** Creates a UUIDv7 from a timestamp, rejecting values outside the 48-bit millisecond range.
    *
    * The range is checked before conversion to milliseconds so even `Instant.MIN` and `Instant.MAX`
    * return `InvalidGeneration` without overflowing. Failures from the random source retain their
    * cause.
    */
  def generate(timestamp: Instant): Either[InvalidGeneration, UUIDv7] =
    generate(timestamp, random)

  private[prelude] def generate(
      timestamp: Instant,
      randomSource: => SecureRandom
  ): Either[InvalidGeneration, UUIDv7] =
    if !timestamp.isAfter(Instant.EPOCH) || !timestamp.isBefore(TimestampUpperBound) then
      Left(InvalidGeneration(None))
    else
      Try {
        val millis = timestamp.toEpochMilli
        val source = randomSource
        val randomA = source.nextInt(1 << 12).toLong
        val randomB = source.nextLong() & RandomBMask
        val mostSignificantBits = (millis << 16) | 0x7000L | randomA
        val leastSignificantBits = 0x8000000000000000L | randomB
        new UUIDv7(mostSignificantBits, leastSignificantBits)
      }.toEither.left.map(error => InvalidGeneration(Some(error)))

  /** Parses a canonical UUIDv7 string. Hex digits may be upper or lower case. */
  def parse(value: String): Either[ParseError, UUIDv7] =
    if !hasCanonicalFormat(value) then Left(ParseError.InvalidFormat)
    else
      val hex = value.filterNot(_ == '-')
      val mostSignificantBits = java.lang.Long.parseUnsignedLong(hex.substring(0, 16), 16)
      val leastSignificantBits = java.lang.Long.parseUnsignedLong(hex.substring(16), 16)
      val version = ((mostSignificantBits >>> 12) & 0xfL).toInt
      val variant = (leastSignificantBits >>> 62).toInt

      if version != 7 then Left(ParseError.InvalidVersion)
      else if variant != 2 then Left(ParseError.InvalidVariant)
      else Right(new UUIDv7(mostSignificantBits, leastSignificantBits))

  private def hasCanonicalFormat(value: String): Boolean =
    value.length == 36 &&
      value.charAt(8) == '-' && value.charAt(13) == '-' &&
      value.charAt(18) == '-' && value.charAt(23) == '-' &&
      value.indices.forall { index =>
        index == 8 || index == 13 || index == 18 || index == 23 ||
        Character.digit(value.charAt(index), 16) >= 0
      }

  private def hex(value: Long, width: Int): String =
    val digits = java.lang.Long.toHexString(value)
    ("0" * (width - digits.length)) + digits

/** An application identifier backed by UUIDv7, without exposing a Java UUID. */
opaque type Id = UUIDv7
object Id:
  /** Generates an identifier, returning an error if the current timestamp is out of range. */
  def generate(): Either[InvalidGeneration, Id] = UUIDv7.generate()

  /** Parses an application identifier from its canonical UUIDv7 string form. */
  def parse(value: String): Either[UUIDv7.ParseError, Id] = UUIDv7.parse(value)

  /** Wraps an already validated UUIDv7 value as an application identifier. */
  def fromUUIDv7(value: UUIDv7): Id = value

  extension (id: Id)
    /** Exposes the UUIDv7 value for UUID-specific operations. */
    def toUUIDv7: UUIDv7 = id
    def value: String = toUUIDv7.toString
