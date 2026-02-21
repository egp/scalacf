////////////////////////////////////////////////////////////
// BEGIN FILE: GosperBinaryEngineTerminationSpec.scala
////////////////////////////////////////////////////////////

package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

/**
 * GosperBinaryEngineTerminationSpec
 *
 * Purpose:
 *
 *   Verify that the GosperBinaryEngine always makes progress and terminates
 *   for finite rational inputs.
 *
 * This Spec detects:
 *
 *   • infinite ingestion loops
 *   • infinite emission loops
 *   • heap exhaustion from non-termination
 *   • failure to emit digits
 *
 * These are correctness properties, not performance properties.
 */
final class GosperBinaryEngineTerminationSpec extends AnyFunSuite:

  ////////////////////////////////////////////////////////////
  // helpers
  ////////////////////////////////////////////////////////////

  private def cf(n: Int, d: Int): ContinuedFraction =
    Rational(n, d).toContinuedFraction


  ////////////////////////////////////////////////////////////
  // tests
  ////////////////////////////////////////////////////////////

  test("engine produces first digit in finite time"):

    val x = cf(355, 113)   // π approximation
    val y = cf(22, 7)

    val z =
      GosperBinaryEngine.run(
        BLFT.add,
        x,
        y
      )

    val first =
      z.terms.head

    assert(first.isInstanceOf[BigInt])


  test("engine produces finite CF for finite inputs"):

    val x = cf(355, 113)
    val y = cf(22, 7)

    val z =
      GosperBinaryEngine.run(
        BLFT.multiply,
        x,
        y
      )

    val terms =
      z.terms.take(50).toList

    assert(terms.nonEmpty)

    // finite rational must terminate
    assert(terms.length < 50)


  test("engine does not enter infinite loop on addition"):

    val x = cf(13, 8)
    val y = cf(21, 13)

    val z =
      GosperBinaryEngine.run(
        BLFT.add,
        x,
        y
      )

    val terms =
      z.terms.take(20).toList

    assert(terms.nonEmpty)


  test("engine does not enter infinite loop on multiplication"):

    val x = cf(13, 8)
    val y = cf(21, 13)

    val z =
      GosperBinaryEngine.run(
        BLFT.multiply,
        x,
        y
      )

    val terms =
      z.terms.take(20).toList

    assert(terms.nonEmpty)


////////////////////////////////////////////////////////////
// END FILE: GosperBinaryEngineTerminationSpec.scala
////////////////////////////////////////////////////////////