////////////////////////////////////////////////////////////
// BEGIN FILE: GosperBinaryEngineBranchCoverageSpec.scala
////////////////////////////////////////////////////////////

package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent.TimeLimits.*
import org.scalatest.time.Span
import org.scalatest.time.Seconds

/**
 * GosperBinaryEngineBranchCoverageSpec
 *
 * Goal:
 *   Drive GosperBinaryEngine through a wide variety of control-flow paths
 *   while remaining strictly bounded and fast.
 *
 * Constraints:
 *   - Hard cap on emitted digits (LazyList.take)
 *   - Hard cap on wall-clock time per test (failAfter)
 */
final class GosperBinaryEngineBranchCoverageSpec extends AnyFunSuite:

  ////////////////////////////////////////////////////////////
  // configuration
  ////////////////////////////////////////////////////////////

  private val PerTestTimeout: Span =
    Span(20, Seconds)

  private val DigitCap: Int =
    20

  ////////////////////////////////////////////////////////////
  // helpers
  ////////////////////////////////////////////////////////////

  private def cf(r: Rational): ContinuedFraction =
    r.toContinuedFraction

  /**
   * Run the engine but only force a bounded prefix of the output CF.
   * This is the critical termination/latency guard.
   */
  private def runPrefix(
    op: BLFT,
    x: Rational,
    y: Rational,
    digits: Int = DigitCap
  ): List[BigInt] =
    failAfter(PerTestTimeout) {
      val out =
        GosperBinaryEngine.run(
          op,
          cf(x),
          cf(y)
        )

      // Force only a bounded prefix.
      out.terms.take(digits).toList
    }

  private def assertNonEmptyPrefix(prefix: List[BigInt]): Unit =
    assert(prefix.nonEmpty, "Expected at least one emitted CF digit")

  ////////////////////////////////////////////////////////////
  // tests
  ////////////////////////////////////////////////////////////

  test("fast bounded prefixes for all ops on small positive rationals"):

    val pairs =
      Seq(
        (Rational(1,2), Rational(1,3)),
        (Rational(2,3), Rational(3,5)),
        (Rational(3,2), Rational(5,3)),
        (Rational(7,4), Rational(9,11))
      )

    val ops =
      Seq(BLFT.add, BLFT.subtract, BLFT.multiply, BLFT.divide)

    for
      (x,y) <- pairs
      op    <- ops
    do
      val prefix = runPrefix(op, x, y)
      assertNonEmptyPrefix(prefix)


  test("fast bounded prefixes with negatives (branch stress: signs/floor paths)"):

    val pairs =
      Seq(
        (Rational(-1,2), Rational(1,3)),
        (Rational(5,3), Rational(-7,4)),
        (Rational(-13,8), Rational(-21,13)),
        (Rational(-355,113), Rational(22,7))
      )

    val ops =
      Seq(BLFT.add, BLFT.subtract, BLFT.multiply, BLFT.divide)

    for
      (x,y) <- pairs
      op    <- ops
    do
      val prefix = runPrefix(op, x, y)
      assertNonEmptyPrefix(prefix)


  test("fast bounded prefixes with zeros (branch stress: exact/degenerate cases)"):

    val pairs =
      Seq(
        (Rational(0,1), Rational(1,2)),
        (Rational(1,2), Rational(0,1)),
        (Rational(0,1), Rational(-3,2)),
        (Rational(-3,2), Rational(0,1))
      )

    // Avoid division by zero as an input rational (y=0) for division op.
    val opsSafeForAll =
      Seq(BLFT.add, BLFT.subtract, BLFT.multiply)

    for
      (x,y) <- pairs
      op    <- opsSafeForAll
    do
      val prefix = runPrefix(op, x, y)
      assertNonEmptyPrefix(prefix)

    // Division cases with y != 0
    val divPairs =
      Seq(
        (Rational(1,2), Rational(2,1)),
        (Rational(-3,2), Rational(5,7)),
        (Rational(0,1), Rational(5,7)) // 0 / y is fine
      )

    for (x,y) <- divPairs do
      val prefix = runPrefix(BLFT.divide, x, y)
      assertNonEmptyPrefix(prefix)


  test("fast bounded prefixes for longer-but-terminating CFs (stress ingestion/emit loop)"):

    // These have longer CF expansions but are still finite rationals.
    val pairs =
      Seq(
        (Rational(355,113), Rational(22,7)),
        (Rational(415,93), Rational(13,5)),
        (Rational(103993,33102), Rational(355,113))
      )

    val ops =
      Seq(BLFT.add, BLFT.multiply, BLFT.divide, BLFT.subtract)

    for
      (x,y) <- pairs
      op    <- ops
    do
      val prefix = runPrefix(op, x, y, digits = 25)
      assertNonEmptyPrefix(prefix)


  test("swapped operands also terminate fast (branch stress: symmetry/priority decisions)"):

    val x = Rational(415,93)
    val y = Rational(355,113)

    val ops =
      Seq(BLFT.add, BLFT.subtract, BLFT.multiply, BLFT.divide)

    for op <- ops do
      val p1 = runPrefix(op, x, y)
      val p2 = runPrefix(op, y, x)

      assertNonEmptyPrefix(p1)
      assertNonEmptyPrefix(p2)

      // Not asserting equality (not generally true for non-commutative ops),
      // just ensuring both paths execute quickly.
      succeed


////////////////////////////////////////////////////////////
// END FILE: GosperBinaryEngineBranchCoverageSpec.scala
////////////////////////////////////////////////////////////
//EOF GosperBinaryEngineBranchCoverageSpec.scala