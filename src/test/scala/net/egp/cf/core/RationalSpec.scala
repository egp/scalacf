////////////////////////////////////////////////////////////
// BEGIN FILE: RationalSpec.scala
////////////////////////////////////////////////////////////

package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

final class RationalSpec extends AnyFunSuite:


  ////////////////////////////////////////////////////////////
  // construction and normalization
  ////////////////////////////////////////////////////////////

  test("normalization reduces fraction"):

    val r = Rational(6,8)

    assert(r.isFinite)
    assert(r.numerator == 3)
    assert(r.denominator == 4)


  test("negative denominator normalized"):

    val r = Rational(1,-2)

    assert(r.isFinite)
    assert(r.numerator == -1)
    assert(r.denominator == 2)


  ////////////////////////////////////////////////////////////
  // infinity construction
  ////////////////////////////////////////////////////////////

  test("positive infinity construction"):

    val r = Rational(5,0)

    assert(r.isPosInf)
    assert(r.isInfinite)


  test("negative infinity construction"):

    val r = Rational(-5,0)

    assert(r.isNegInf)
    assert(r.isInfinite)


  ////////////////////////////////////////////////////////////
  // arithmetic finite
  ////////////////////////////////////////////////////////////

  test("addition finite"):

    val a = Rational(1,3)
    val b = Rational(1,6)

    val c = a + b

    assert(c == Rational(1,2))


  test("subtraction finite"):

    val a = Rational(3,4)
    val b = Rational(1,4)

    assert(a - b == Rational(1,2))


  test("multiplication finite"):

    val a = Rational(2,3)
    val b = Rational(3,5)

    assert(a * b == Rational(2,5))


  test("division finite"):

    val a = Rational(2,3)
    val b = Rational(4,5)

    assert(a / b == Rational(5,6))


  ////////////////////////////////////////////////////////////
  // infinity arithmetic
  ////////////////////////////////////////////////////////////

  test("finite plus infinity"):

    val r = Rational(3,4) + PosInf

    assert(r.isPosInf)


  test("finite times infinity sign"):

    val r = Rational(-3,4) * PosInf

    assert(r.isNegInf)


  ////////////////////////////////////////////////////////////
  // reciprocal
  ////////////////////////////////////////////////////////////

  test("reciprocal finite"):

    val r = Rational(3,4)

    assert(r.reciprocal == Rational(4,3))


  test("reciprocal infinity is zero"):

    assert(PosInf.reciprocal == Rational(0,1))
    assert(NegInf.reciprocal == Rational(0,1))


  ////////////////////////////////////////////////////////////
  // ordering
  ////////////////////////////////////////////////////////////

  test("finite ordering"):

    assert(Rational(1,2) < Rational(2,3))
    assert(Rational(7,5) > Rational(6,5))


  test("infinity ordering"):

    assert(PosInf > Rational(999,1))
    assert(NegInf < Rational(-999,1))


  ////////////////////////////////////////////////////////////
  // floor
  ////////////////////////////////////////////////////////////

  test("floor positive"):

    assert(Rational(7,3).floor == 2)


  test("floor negative"):

    assert(Rational(-7,3).floor == -3)


////////////////////////////////////////////////////////////
// END FILE: RationalSpec.scala
////////////////////////////////////////////////////////////