// BLFTRobustnessSpec.scala
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

/**
 * BLFTRobustnessSpec
 *
 * Validates that BLFT range propagation is conservative and total:
 * it must not throw even when some corner evaluations are undefined
 * (e.g. 0*∞, ∞-∞). In such cases, the result must be widened enough
 * to remain safe, and extractSafe must return None.
 */
final class BLFTRobustnessSpec extends AnyFunSuite:

  private val finiteZero = Range.exact(Rational(0,1))
  private val posInf     = Range.exact(PosInf)
  private val negInf     = Range.exact(NegInf)

  test("BLFT.multiply range does not throw on 0 * ∞ corner"):

    // multiply: axy with a=1. Corner eval includes 0 * ∞ which is undefined.
    // Range must be conservative and total (no throw).
    val r =
      BLFT.multiply.range(
        finiteZero,
        posInf
      )

    assert(r != null)
    assert(r.min != null)
    assert(r.max != null)

    // Must not claim a safe digit
    assert(BLFT.multiply.extractSafe(finiteZero, posInf).isEmpty)


  test("BLFT.add range does not throw on (+∞) + (-∞) corner"):

    val r =
      BLFT.add.range(
        posInf,
        negInf
      )

    assert(r != null)
    assert(r.min != null)
    assert(r.max != null)

    assert(BLFT.add.extractSafe(posInf, negInf).isEmpty)


  test("BLFT.subtract range does not throw on (+∞) - (+∞) corner"):

    val r =
      BLFT.subtract.range(
        posInf,
        posInf
      )

    assert(r != null)
    assert(r.min != null)
    assert(r.max != null)

    assert(BLFT.subtract.extractSafe(posInf, posInf).isEmpty)


// EOF: BLFTRobustnessSpec.scala