package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MatrixRangeDenominatorCrossSpec extends AnyFunSuite:

  test("Matrix.range detects denominator sign change and returns infinite range") {

    /*
     Transform:
       f(x,y) = 1 / (x - y)

     denominator = x - y

     Over rectangle:
       x ∈ [1,3]
       y ∈ [1,3]

     denominator crosses zero at x=y.

     True range = (-∞, +∞)
     */

    val m =
      Matrix(
        0, 0, 0, 1,   // numerator = 1
        0, 1, -1, 0   // denominator = x - y
      )

    val xr =
      CFRange.inside(
        Rational(1,1),
        Rational(3,1)
      )

    val yr =
      CFRange.inside(
        Rational(1,1),
        Rational(3,1)
      )

    val r = m.range(xr, yr)

    assert(r.lo.isInfinite, s"Expected infinite lo, got ${r.lo}")
    assert(r.hi.isInfinite, s"Expected infinite hi, got ${r.hi}")

    assert(
      r.lo == Rational.negativeInfinity ||
      r.lo == Rational.positiveInfinity
    )

    assert(
      r.hi == Rational.positiveInfinity ||
      r.hi == Rational.negativeInfinity
    )
  }
