package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MatrixRangeSpec extends AnyFunSuite:

  test("multiplyIdentity produces correct range") {

    val m = Matrix.multiplyIdentity

    val xr = CFRange.inside(
      Rational(2,1),
      Rational(2,1)
    )

    val yr = CFRange.inside(
      Rational(3,1),
      Rational(3,1)
    )

    val r = m.range(xr, yr)

    assert(r.lo == Rational(6,1))
    assert(r.hi == Rational(6,1))
  }

  test("reduce removes integer digit correctly") {

    val m = Matrix.multiplyIdentity

    val reduced = m.reduce(6)

    val result =
      reduced.apply(
        Rational(2,1),
        Rational(3,1)
      )

    // after emitting 6, remainder should be 0

    assert(result.contains(Rational.zero))
  }
