package net.egp.cf
import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MatrixReduceDigitEmissionSpec extends AnyFunSuite:

  test("reduce enables next digit emission") {

    val m = Matrix.multiplyIdentity

    val xr = CFRange.inside(Rational(2,1), Rational(2,1))
    val yr = CFRange.inside(Rational(3,1), Rational(3,1))

    val digit = 6

    val reduced = m.reduce(digit)

    val r = reduced.range(xr, yr)

    assert(r.floorOption.nonEmpty)
  }
