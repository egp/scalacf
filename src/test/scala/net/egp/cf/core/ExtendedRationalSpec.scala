// ExtendedRationalSpec.scala
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

/**
 * Specification for Rational (finite + infinities).
 *
 * Validates:
 *  - ordering
 *  - equality/normalization
 *  - reciprocal correctness
 *  - infinity behavior
 *  - division behavior
 *
 * This is foundational for Range, BLFT, and GosperBinaryEngine correctness.
 */
final class ExtendedRationalSpec extends AnyFunSuite:

  ////////////////////////////////////////////////////////////
  // helpers
  ////////////////////////////////////////////////////////////

  private def R(n: Int, d: Int): Rational =
    Rational(BigInt(n), BigInt(d))


  ////////////////////////////////////////////////////////////
  // equality / normalization
  ////////////////////////////////////////////////////////////

  test("finite equality works (normalized)"):

    assert(R(1,2) == R(1,2))
    assert(R(2,4) == R(1,2))
    assert(R(-3,5) == R(-3,5))


  test("infinity equality works"):

    assert(PosInf == PosInf)
    assert(NegInf == NegInf)
    assert(PosInf != NegInf)


  ////////////////////////////////////////////////////////////
  // ordering
  ////////////////////////////////////////////////////////////

  test("finite ordering works"):

    assert(R(1,2) < R(3,4))
    assert(R(-1,2) < R(0,1))
    assert(R(5,1) > R(4,1))


  test("infinity ordering works"):

    assert(NegInf < R(-1000,1))
    assert(NegInf < R(0,1))
    assert(NegInf < PosInf)

    assert(PosInf > R(1000,1))
    assert(PosInf > R(0,1))


  ////////////////////////////////////////////////////////////
  // reciprocal correctness
  ////////////////////////////////////////////////////////////

  test("finite reciprocal works"):

    assert(R(2,1).reciprocal == R(1,2))
    assert(R(3,4).reciprocal == R(4,3))


  test("reciprocal of negative finite works"):

    assert(R(-2,1).reciprocal == R(-1,2))


  test("reciprocal of positive infinity is zero"):

    assert(PosInf.reciprocal == R(0,1))


  test("reciprocal of negative infinity is zero"):

    assert(NegInf.reciprocal == R(0,1))


  ////////////////////////////////////////////////////////////
  // division correctness
  ////////////////////////////////////////////////////////////

  test("finite division works"):

    val a = R(6,1)
    val b = R(3,1)

    val result = a / b
    assert(result == R(2,1))


  test("division by infinity yields zero"):

    val result = R(5,1) / PosInf
    assert(result == R(0,1))


  test("infinity divided by finite yields infinity"):

    val result = PosInf / R(2,1)
    assert(result == PosInf)


  ////////////////////////////////////////////////////////////
  // monotonicity
  ////////////////////////////////////////////////////////////

  test("ordering is transitive"):

    val a = NegInf
    val b = R(0,1)
    val c = PosInf

    assert(a < b)
    assert(b < c)
    assert(a < c)


// EOF: ExtendedRationalSpec.scala