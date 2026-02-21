package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class CFRangeIngestPropertiesSpec extends AnyFunSuite:

  test("ingest reduces magnitude correctly") {

    val r =
      CFRange.inside(
        Rational(5,1),
        Rational(6,1)
      )

    val ing = r.ingest(5)

    assert(ing.lo == Rational(1,1))
  }

  test("ingest produces infinite upper bound when appropriate") {

    val r =
      CFRange.inside(
        Rational(5,1),
        Rational(6,1)
      )

    val ing = r.ingest(5)

    assert(ing.hi.isInfinite)
  }
