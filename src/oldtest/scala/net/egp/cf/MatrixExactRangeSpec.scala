package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MatrixExactRangeSpec extends AnyFunSuite:

  test("range contains exact transform result") {

    val xr = CFRange.inside(Rational(2,1), Rational(2,1))
    val yr = CFRange.inside(Rational(3,1), Rational(3,1))

    val r = Matrix.multiplyIdentity.range(xr, yr)

    assert(r.lo == Rational(6,1))
    assert(r.hi == Rational(6,1))
  }

  test("range bounds match corner extrema") {

    val xr = CFRange.inside(Rational(2,1), Rational(4,1))
    val yr = CFRange.inside(Rational(3,1), Rational(5,1))

    val r = Matrix.multiplyIdentity.range(xr, yr)

    assert(r.lo == Rational(6,1))
    assert(r.hi == Rational(20,1))
  }
