// RangeSpec.scala
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

final class RangeSpec extends AnyFunSuite:


  ////////////////////////////////////////////////////////////
  // classification
  ////////////////////////////////////////////////////////////

  test("exact classification"):

    val r = Range.exact(Rational(3,4))

    assert(r.isExact)
    assert(!r.isInside)
    assert(!r.isOutside)


  test("inside classification"):

    val r = Range.inside(
      Rational(1,2),
      Rational(3,4)
    )

    assert(r.isInside)
    assert(!r.isExact)
    assert(!r.isOutside)


  test("outside classification"):

    val r = Range.outside(
      Rational(3,4),
      Rational(1,2)
    )

    assert(r.isOutside)
    assert(!r.isInside)
    assert(!r.isExact)


  ////////////////////////////////////////////////////////////
  // width
  ////////////////////////////////////////////////////////////

  test("exact width zero"):

    val r = Range.exact(Rational(5,7))

    assert(r.width == Rational(0,1))


  test("inside width positive"):

    val r = Range.inside(
      Rational(1,2),
      Rational(3,2)
    )

    assert(r.width > Rational(0,1))


  ////////////////////////////////////////////////////////////
  // reciprocal
  ////////////////////////////////////////////////////////////

  test("reciprocal swaps endpoints"):

    val r = Range.inside(
      Rational(1,2),
      Rational(2,1)
    )

    val rec = r.reciprocal

    assert(rec.min == Rational(1,2))
    assert(rec.max == Rational(2,1))


  ////////////////////////////////////////////////////////////
  // reciprocalSubtract invariant
  ////////////////////////////////////////////////////////////

  test("reciprocalSubtract invariant never throws"):

    val r = Range.inside(
      Rational(3,2),
      Rational(5,2)
    )

    val result = r.reciprocalSubtract(1)

    assert(result != null)


  ////////////////////////////////////////////////////////////
  // containment
  ////////////////////////////////////////////////////////////

  test("inside containment"):

    val r = Range.inside(
      Rational(1,2),
      Rational(3,2)
    )

    assert(r.contains(Rational(1,1)))
    assert(!r.contains(Rational(3,1)))


  test("outside containment"):

    val r = Range.outside(
      Rational(3,2),
      Rational(1,2)
    )

    assert(r.contains(Rational(2,1)))
    assert(r.contains(Rational(0,1)))
    assert(!r.contains(Rational(1,1)))


  ////////////////////////////////////////////////////////////
  // 5.2.2: Range.fromCF behavior
  ////////////////////////////////////////////////////////////

  test("Range.fromCF is exact for terminating continued fraction"):

    val x = Rational(415,93)
    val cf = x.toContinuedFraction

    val r = Range.fromCF(cf)

    assert(r.isExact)
    assert(r.min == x)
    assert(r.max == x)


  test("Range.fromCF is inside (not exact) for non-terminating continued fraction"):

    // Golden ratio φ = [1; 1,1,1,...] (non-terminating)
    val phi =
      ContinuedFraction.fromTerms(
        LazyList(1) #::: LazyList.continually(BigInt(1))
      )

    val r = Range.fromCF(phi)

    assert(!r.isExact)
    assert(r.isInside)

    // should contain early convergents
    val conv = phi.convergents.take(8).toList
    conv.foreach { c =>
      assert(r.contains(c), s"range did not contain convergent $c")
    }


// EOF: RangeSpec.scala