package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class CFRangeExactReciprocalSpec extends AnyFunSuite:

  test("reciprocal of singleton inside range") {

    val r = CFRange.inside(Rational(2,1), Rational(2,1))

    val rec = r.reciprocal

    assert(rec.inside)
    assert(rec.lo == Rational(1,2))
    assert(rec.hi == Rational(1,2))
  }

  test("double reciprocal restores original inside range") {

    val r = CFRange.inside(Rational(3,2), Rational(7,4))

    val rr = r.reciprocal.reciprocal

    assert(rr == r)
  }

  test("double reciprocal restores original outside range") {

    val r = CFRange.outside(Rational(2,1), Rational(5,1))

    val rr = r.reciprocal.reciprocal

    assert(rr.lo == r.lo)
    assert(rr.hi == r.hi)
    assert(!rr.inside)
  }

  test("outside reciprocal preserves exclusion of zero") {

    val r = CFRange.outside(Rational(-2,1), Rational(3,1))

    val rec = r.reciprocal

    assert(!rec.inside)
    assert(!rec.containsZero)
  }
