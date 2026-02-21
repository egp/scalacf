package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class CFRangeReciprocalPropertiesSpec extends AnyFunSuite:

  test("reciprocal reverses bounds") {

    val r =
      CFRange.inside(
        Rational(2,1),
        Rational(4,1)
      )

    val inv = r.reciprocal

    assert(inv.lo == Rational(1,4))
    assert(inv.hi == Rational(1,2))
  }

  test("double reciprocal restores original") {

    val r =
      CFRange.inside(
        Rational(2,1),
        Rational(4,1)
      )

    val rr =
      r.reciprocal.reciprocal

    assert(rr.lo == r.lo)
    assert(rr.hi == r.hi)
  }
