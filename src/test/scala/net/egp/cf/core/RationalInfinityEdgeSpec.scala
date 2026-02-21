// RationalInfinityEdgeSpec.scala
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

final class RationalInfinityEdgeSpec extends AnyFunSuite:

  test("infinity addition undefined forms throw"):

    assertThrows[ArithmeticException] {
      PosInf + NegInf
    }

    assertThrows[ArithmeticException] {
      NegInf + PosInf
    }


  test("infinity multiplication by zero is undefined and throws"):

    assertThrows[ArithmeticException] {
      PosInf * Rational(0,1)
    }

    assertThrows[ArithmeticException] {
      NegInf * Rational(0,1)
    }

    assertThrows[ArithmeticException] {
      Rational(0,1) * PosInf
    }

    assertThrows[ArithmeticException] {
      Rational(0,1) * NegInf
    }


  test("infinity division by infinity is undefined and throws"):

    assertThrows[ArithmeticException] {
      PosInf / PosInf
    }

    assertThrows[ArithmeticException] {
      PosInf / NegInf
    }

    assertThrows[ArithmeticException] {
      NegInf / PosInf
    }

    assertThrows[ArithmeticException] {
      NegInf / NegInf
    }


  test("finite division by zero throws requirement"):

    assertThrows[IllegalArgumentException] {
      Rational(1,2) / Rational(0,1)
    }

    assertThrows[IllegalArgumentException] {
      Rational(-3,5) / Rational(0,1)
    }


  test("infinity divided by finite has correct sign"):

    assert(PosInf / Rational(2,1) == PosInf)
    assert(PosInf / Rational(-2,1) == NegInf)

    assert(NegInf / Rational(2,1) == NegInf)
    assert(NegInf / Rational(-2,1) == PosInf)


  test("compare handles infinity extremes"):

    assert(PosInf > Rational(0,1))
    assert(NegInf < Rational(0,1))
    assert(PosInf > NegInf)
    assert(PosInf.compare(PosInf) == 0)
    assert(NegInf.compare(NegInf) == 0)


// EOF: RationalInfinityEdgeSpec.scala