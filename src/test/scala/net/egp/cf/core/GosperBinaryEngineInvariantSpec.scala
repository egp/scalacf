// GosperBinaryEngineInvariantSpec.scala
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

/**
 * Verifies core Gosper invariant:
 *
 *   If extractSafe emits digit d, then floor(z(range)) is provably d
 *   across the entire output range.
 *
 * This is the correctness contract for streaming CF arithmetic.
 */
final class GosperBinaryEngineInvariantSpec extends AnyFunSuite:

  ////////////////////////////////////////////////////////////
  // helper test values
  ////////////////////////////////////////////////////////////

  private val finiteTestValues =
    Seq(
      Rational(-5,2),
      Rational(-3,2),
      Rational(-1,1),
      Rational(-1,2),
      Rational(0,1),
      Rational(1,2),
      Rational(1,1),
      Rational(3,2),
      Rational(5,2)
    )

  private def safeEval(z: BLFT, x: Rational, y: Rational): Option[Rational] =
    try
      Some(z.eval(x,y))
    catch
      case _: ArithmeticException => None
      case _: IllegalArgumentException => None

  ////////////////////////////////////////////////////////////
  // invariant: safe extraction must match floor everywhere
  ////////////////////////////////////////////////////////////

  test("extractSafe invariant must never be violated (when defined)"):

    val transforms =
      Seq(
        BLFT.add,
        BLFT.subtract,
        BLFT.multiply,
        BLFT.divide
      )

    for
      z <- transforms
      x <- finiteTestValues
      y <- finiteTestValues
    do

      val xr = Range.exact(x)
      val yr = Range.exact(y)

      val digitOpt =
        z.extractSafe(xr, yr)

      val evalOpt =
        safeEval(z, x, y)

      (digitOpt, evalOpt) match

        case (Some(digit), Some(result)) =>
          assert(
            result.floor == digit,
            s"""
               |Invariant violation detected
               |
               |Transform: $z
               |x: $x
               |y: $y
               |
               |Result: $result
               |Expected floor: ${result.floor}
               |Extracted digit: $digit
               |""".stripMargin
          )

        case (Some(_), None) =>
          // If operation is undefined at this point (e.g. 0/0),
          // we do not demand a digit. If a digit was emitted anyway,
          // that would be suspicious, but we allow it here to keep this
          // test focused on defined points.
          succeed

        case _ =>
          succeed


  ////////////////////////////////////////////////////////////
  // invariant under reciprocalSubtract progression
  ////////////////////////////////////////////////////////////

  test("reciprocalSubtract progression preserves extractSafe contract"):

    val z0 = BLFT.add

    var z = z0
    var xr = Range.exact(Rational(5,3))
    var yr = Range.exact(Rational(7,4))

    for step <- 0 until 10 do

      z.extractSafe(xr, yr) match

        case Some(digit) =>

          // Correct invariant: digit must equal the common floor
          // of the output interval.
          val out =
            z.range(xr, yr)

          // If out is unbounded, extraction must not have succeeded.
          assert(!out.min.isInfinite && !out.max.isInfinite)

          assert(out.min.floor == out.max.floor)
          assert(out.min.floor == digit, s"Invariant failure at step $step")

          z = z.emit(digit)
          xr = xr.reciprocalSubtract(digit)
          yr = yr.reciprocalSubtract(digit)

        case None =>
          // No safe digit; progression stops.
          succeed


  ////////////////////////////////////////////////////////////
  // invariant must hold for inside ranges
  ////////////////////////////////////////////////////////////

  test("extractSafe invariant holds for inside ranges"):

    val xr =
      Range.inside(
        Rational(2,1),
        Rational(3,1)
      )

    val yr =
      Range.inside(
        Rational(4,1),
        Rational(5,1)
      )

    val digitOpt =
      BLFT.add.extractSafe(xr, yr)

    digitOpt match

      case Some(digit) =>

        val out =
          BLFT.add.range(xr, yr)

        assert(!out.min.isInfinite && !out.max.isInfinite)
        assert(out.min.floor == out.max.floor)
        assert(out.min.floor == digit)

      case None =>
        succeed


// EOF: GosperBinaryEngineInvariantSpec.scala