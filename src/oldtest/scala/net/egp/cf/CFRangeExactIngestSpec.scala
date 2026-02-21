package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class CFRangeExactIngestSpec extends AnyFunSuite:

  test("ingest exact singleton") {

    val r = CFRange.inside(Rational(5,1), Rational(5,1))

    val ing = r.ingest(2)

    assert(ing.inside)
    assert(ing.lo == Rational(1,3))
    assert(ing.hi == Rational(1,3))
  }

  test("ingest preserves exact transform bounds") {

    val r = CFRange.inside(Rational(3,1), Rational(4,1))

    val ing = r.ingest(2)

    assert(ing.lo == Rational(1,2))
    assert(ing.hi == Rational(1,1))
  }

  test("double ingest inverse restores original") {

    val r = CFRange.inside(Rational(7,1), Rational(9,1))

    val t = BigInt(5)

    val ing = r.ingest(t)

    val back =
      CFRange(
        Rational.one / ing.hi + Rational(t,1),
        Rational.one / ing.lo + Rational(t,1),
        inside = true
      )

    assert(back.lo == r.lo)
    assert(back.hi == r.hi)
  }
