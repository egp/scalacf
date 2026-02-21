package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class CFRangeSpec extends AnyFunSuite:

  test("CFRange.reciprocal basic") {

    val r = CFRange.inside(
      Rational(2,1),
      Rational(3,1)
    )

    val inv = r.reciprocal

    assert(inv.lo == Rational(1,3))
    assert(inv.hi == Rational(1,2))
  }

  test("CFRange.ingest produces correct reciprocal shift") {

    // x ∈ [2,3]
    val r = CFRange.inside(
      Rational(2,1),
      Rational(3,1)
    )

    val ing = r.ingest(2)

    // expected: 1/(x-2), x∈[2,3] → [1,∞)

    assert(ing.lo == Rational(1,1))
    assert(ing.hi.isInfinite)
  }

  test("CFRange.floorOption detects stable digit") {

    val r = CFRange.inside(
      Rational(6,1),
      Rational(6,1)
    )

    assert(r.floorOption.contains(6))
  }

  test("CFRange.floorOption detects unstable digit") {

    val r = CFRange.inside(
      Rational(5,1),
      Rational(6,1)
    )

    assert(r.floorOption.isEmpty)
  }
