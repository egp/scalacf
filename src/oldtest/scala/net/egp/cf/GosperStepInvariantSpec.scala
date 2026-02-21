package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class GosperStepInvariantSpec extends AnyFunSuite:

  test("multiply identity emits correct first digit") {

    val xr = CFRange.inside(Rational(2,1), Rational(2,1))
    val yr = CFRange.inside(Rational(3,1), Rational(3,1))

    val r = Matrix.multiplyIdentity.range(xr, yr)

    assert(r.floorOption.contains(6))
  }
