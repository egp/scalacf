package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MultiplicationCoreSpec extends AnyFunSuite:

  test("single step multiply identity emits correct digit") {

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

    assert(r.floorOption.contains(6))
  }
