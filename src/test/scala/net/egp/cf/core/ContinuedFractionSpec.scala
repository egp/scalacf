// ContinuedFractionSpec.scala
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

class ContinuedFractionSpec extends AnyFunSuite:

  private def r(n: Int, d: Int): Rational =
    Rational(BigInt(n), BigInt(d))

  private def assertLegalTerms(cf: ContinuedFraction, maxCheck: Int = 50): Unit =
    val ts = cf.terms.take(maxCheck).toList
    assert(ts.nonEmpty)
    // a0 can be any integer
    // a_i >= 1 for i>=1
    ts.drop(1).foreach { t =>
      assert(t >= 1, s"illegal CF term found: $t in $ts")
    }


  test("Rational → CF → Rational round-trip exact"):

    val values =
      List(
        r(0,1),
        r(1,1),
        r(3,2),
        r(13,5),
        r(415,93),
        r(355,113),
        r(-1,1),
        r(-3,2),
        r(-7,3),
        r(-415,93)
      )

    for v <- values do
      val cf = v.toContinuedFraction
      assertLegalTerms(cf)

      val back = cf.toRational
      assert(back == v)


  test("terms correct for 415/93"):

    val cf =
      Rational(415,93).toContinuedFraction

    val expected =
      List(BigInt(4), BigInt(2), BigInt(6), BigInt(7))

    assert(cf.terms.take(4).toList == expected)
    assertLegalTerms(cf)


  test("convergents exact for 415/93"):

    val cf =
      Rational(415,93).toContinuedFraction

    val expected =
      List(
        Rational(4,1),
        Rational(9,2),
        Rational(58,13),
        Rational(415,93)
      )

    assert(cf.convergents.take(4).toList == expected)


  test("single term convergent"):

    val cf =
      Rational(5,1).toContinuedFraction

    assert(cf.convergents.head == Rational(5,1))


  test("two-term convergent correct"):

    val cf =
      Rational(7,3).toContinuedFraction

    assert(cf.convergents.last == Rational(7,3))


  test("negative rational produces legal continued fraction terms"):

    val values =
      List(
        Rational(-1,2),
        Rational(-3,2),
        Rational(-7,3),
        Rational(-22,7),
        Rational(-355,113)
      )

    values.foreach { v =>
      val cf = v.toContinuedFraction
      assertLegalTerms(cf)
      assert(cf.toRational == v)
    }


  test("zero produces single-term continued fraction [0]"):

    val cf = Rational(0,1).toContinuedFraction
    assert(cf.terms.take(5).toList == List(BigInt(0)))
    assert(cf.toRational == Rational(0,1))


// EOF: ContinuedFractionSpec.scala