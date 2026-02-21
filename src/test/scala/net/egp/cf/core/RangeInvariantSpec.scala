////////////////////////////////////////////////////////////
// BEGIN FILE: RangeInvariantSpec.scala
////////////////////////////////////////////////////////////

package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

/**
 * RangeInvariantSpec
 *
 * Validates the mathematical invariants of Gosper Range:
 *
 *   1. width is always non-negative
 *   2. exact ranges have zero width
 *   3. inside and outside ranges can have identical width
 *   4. topology ordering dominates width ordering
 *   5. Gosper uncertainty ordering is correct
 *
 * This spec protects against incorrect compare() or width logic.
 */
final class RangeInvariantSpec extends AnyFunSuite:


  ////////////////////////////////////////////////////////////
  // helpers
  ////////////////////////////////////////////////////////////

  private def r(n: Int, d: Int) =
    Rational(n, d)


  ////////////////////////////////////////////////////////////
  // width invariants
  ////////////////////////////////////////////////////////////

  test("width is always non-negative"):

    val ranges =
      List(
        Range.exact(r(5,1)),
        Range.inside(r(1,1), r(2,1)),
        Range.outside(r(2,1), r(1,1)),
        Range.inside(r(-3,1), r(7,1)),
        Range.outside(r(7,1), r(-3,1))
      )

    ranges.foreach { range =>
      assert(
        range.width >= Rational(0,1),
        s"Width negative for range $range"
      )
    }


  test("exact range has zero width"):

    val x = Range.exact(r(42,7))

    assert(x.width == Rational(0,1))


  ////////////////////////////////////////////////////////////
  // topology vs width invariants
  ////////////////////////////////////////////////////////////

  test("inside and outside ranges may have identical width"):

    val inside  = Range.inside(r(4,1), r(5,1))
    val outside = Range.outside(r(5,1), r(4,1))

    assert(
      inside.width == outside.width,
      s"Widths differ: inside=${inside.width}, outside=${outside.width}"
    )


  test("inside range is always less uncertain than outside range of same width"):

    val inside  = Range.inside(r(4,1), r(5,1))
    val outside = Range.outside(r(5,1), r(4,1))

    assert(
      inside < outside,
      s"Inside range must be less uncertain than outside range"
    )


  ////////////////////////////////////////////////////////////
  // exact ordering invariants
  ////////////////////////////////////////////////////////////

  test("exact range is always least uncertain"):

    val exact  = Range.exact(r(3,1))
    val inside = Range.inside(r(3,1), r(4,1))
    val outside= Range.outside(r(4,1), r(3,1))

    assert(exact < inside)
    assert(exact < outside)


  ////////////////////////////////////////////////////////////
  // width ordering within same topology
  ////////////////////////////////////////////////////////////

  test("inside narrow is less uncertain than inside wide"):

    val narrow = Range.inside(r(4,1), r(5,1))
    val wide   = Range.inside(r(4,1), r(10,1))

    assert(
      narrow < wide,
      s"Narrow inside must be less uncertain than wide inside"
    )


  test("outside wide is less uncertain than outside narrow"):

    val wide   = Range.outside(r(10,1), r(4,1))
    val narrow = Range.outside(r(5,1), r(4,1))

    assert(
      wide < narrow,
      s"Wide outside must be less uncertain than narrow outside"
    )


  ////////////////////////////////////////////////////////////
  // equality invariants
  ////////////////////////////////////////////////////////////

  test("identical ranges compare equal"):

    val a = Range.inside(r(2,1), r(7,1))
    val b = Range.inside(r(2,1), r(7,1))

    assert(a.compare(b) == 0)


////////////////////////////////////////////////////////////
// END FILE: RangeInvariantSpec.scala
////////////////////////////////////////////////////////////