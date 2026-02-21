// Range.scala
package net.egp.cf.core

/**
 * Range over Rational, including infinity.
 *
 * Encoding invariant:
 *
 *   min == max  → exact
 *   min < max   → inside interval
 *   min > max   → outside interval
 *
 * Outside intervals represent complement intervals.
 */
final case class Range(
  min: Rational,
  max: Rational
) extends Ordered[Range]:

  require(min != null, "min cannot be null")
  require(max != null, "max cannot be null")

  private def assertEncoding(): Unit =
    require(
      (min == max) || (min < max) || (min > max),
      s"Range encoding invariant violated: min=$min max=$max"
    )

  assertEncoding()

  ////////////////////////////////////////////////////////////
  // classification
  ////////////////////////////////////////////////////////////

  def isExact: Boolean = min == max
  def isInside: Boolean = min < max
  def isOutside: Boolean = min > max


  ////////////////////////////////////////////////////////////
  // width
  ////////////////////////////////////////////////////////////

  def width: Rational =
    val w =
      if isExact then
        Rational(0, 1)
      else if min.isInfinite || max.isInfinite then
        PosInf
      else
        if max >= min then max - min else min - max

    require(w.isPosInf || w >= Rational(0, 1), s"width must be non-negative, found $w")
    w


  ////////////////////////////////////////////////////////////
  // reciprocal
  ////////////////////////////////////////////////////////////

  def reciprocal: Range =
    val r = Range(max.reciprocal, min.reciprocal)
    r.assertEncoding()
    r


  ////////////////////////////////////////////////////////////
  // reciprocalSubtract: 1 / (range - digit)
  ////////////////////////////////////////////////////////////

  def reciprocalSubtract(digit: BigInt): Range =
    val d = Rational(digit, 1)
    val shifted = Range(min - d, max - d)
    val result = shifted.reciprocal
    result.assertEncoding()
    result


  ////////////////////////////////////////////////////////////
  // containment
  ////////////////////////////////////////////////////////////

  def contains(x: Rational): Boolean =
    require(x != null)

    if isExact then
      x == min
    else if isInside then
      x >= min && x <= max
    else
      x <= max || x >= min


  ////////////////////////////////////////////////////////////
  // uncertainty ordering
  ////////////////////////////////////////////////////////////

  override def compare(that: Range): Int =
    require(that != null)

    if this == that then 0
    else if this.isExact then -1
    else if that.isExact then 1
    else if this.isInside && that.isOutside then -1
    else if this.isOutside && that.isInside then 1
    else if this.isInside && that.isInside then
      this.width.compare(that.width)
    else
      // outside vs outside: wider is less uncertain
      that.width.compare(this.width)


////////////////////////////////////////////////////////////
// companion
////////////////////////////////////////////////////////////

object Range:

  def exact(x: Rational): Range =
    require(x != null)
    val r = Range(x, x)
    require(r.isExact)
    r

  def inside(min: Rational, max: Rational): Range =
    require(min != null)
    require(max != null)
    val r = Range(min, max)
    require(r.isInside || r.isExact)
    r

  def outside(min: Rational, max: Rational): Range =
    require(min != null)
    require(max != null)
    val r = Range(min, max)
    require(r.isOutside || r.isExact)
    r

  /**
   * If CF terminates, represent it exactly; else bound via convergents.
   */
  def fromCF(cf: ContinuedFraction): Range =
    require(cf != null)

    cf.toRationalOption() match
      case Some(r) =>
        val ex = exact(r)
        require(ex.isExact)
        ex

      case None =>
        val conv = cf.convergents.take(10).toList

        conv match
          case Nil =>
            exact(Rational(0, 1))

          case x :: Nil =>
            exact(x)

          case xs =>
            inside(xs.min, xs.max)


// EOF: Range.scala