package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class CFRangeInvariantSpec extends AnyFunSuite:

  test("shift without reciprocal") {

    val r =
      CFRange.inside(
        Rational(2,1),
        Rational(3,1)
      )

    val shifted =
      CFRange(
        r.lo - Rational(2,1),
        r.hi - Rational(2,1),
        inside = r.inside
      )

    assert(shifted.lo == Rational(0,1))
    assert(shifted.hi == Rational(1,1))
  }

  test("reciprocal valid only when strictly positive") {

    val r =
      CFRange.inside(
        Rational(1,1),
        Rational(2,1)
      )

    val inv = r.reciprocal

    assert(inv.lo == Rational(1,2))
    assert(inv.hi == Rational(1,1))
  }

  test("reciprocal invalid when containing zero") {

    val r =
      CFRange.outside(
        Rational(-1,1),
        Rational(1,1)
      )

    assertThrows[IllegalArgumentException] {
      r.reciprocal
    }
  }

  test("ingest invariant example") {

    // x in [3,4], ingest 3

    val r =
      CFRange.inside(
        Rational(3,1),
        Rational(4,1)
      )

    val ing = r.ingest(3)

    assert(ing.lo == Rational(1,1))
    assert(ing.hi.isInfinite)
  }
