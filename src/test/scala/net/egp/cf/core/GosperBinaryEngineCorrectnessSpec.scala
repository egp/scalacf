// GosperBinaryEngineCorrectnessSpec.scala
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

/**
 * End-to-end correctness verification of GosperBinaryEngine.
 *
 * Verifies that Gosper streaming arithmetic produces identical
 * continued fractions as exact Rational arithmetic.
 */
final class GosperBinaryEngineCorrectnessSpec extends AnyFunSuite:

  ////////////////////////////////////////////////////////////
  // helper
  ////////////////////////////////////////////////////////////

  private def firstTerms(
    cf: ContinuedFraction,
    n: Int = 20
  ): List[BigInt] =
    cf.terms.take(n).toList


  private def verifyOperation(
    x: Rational,
    y: Rational,
    op: BLFT,
    name: String
  ): Unit =

    val expected =
      firstTerms(
        op.eval(x,y).toContinuedFraction
      )

    val actual =
      firstTerms(
        GosperBinaryEngine.run(
          op,
          ContinuedFraction.fromRational(x),
          ContinuedFraction.fromRational(y)
        )
      )

    assert(
      actual == expected,
      s"""
         |$name mismatch
         |
         |x: $x
         |y: $y
         |
         |expected: $expected
         |actual:   $actual
         |""".stripMargin
    )


  ////////////////////////////////////////////////////////////
  // addition
  ////////////////////////////////////////////////////////////

  test("addition matches exact rational reference"):

    val testPairs =
      Seq(
        (Rational(1,2), Rational(1,3)),
        (Rational(3,4), Rational(5,7)),
        (Rational(22,7), Rational(355,113)),
        (Rational(-5,3), Rational(7,4))
      )

    for (x,y) <- testPairs do
      verifyOperation(x,y, BLFT.add, "addition")


  ////////////////////////////////////////////////////////////
  // subtraction
  ////////////////////////////////////////////////////////////

  test("subtraction matches exact rational reference"):

    val testPairs =
      Seq(
        (Rational(5,2), Rational(3,2)),
        (Rational(7,4), Rational(5,6)),
        (Rational(22,7), Rational(355,113)),
        (Rational(-5,3), Rational(7,4))
      )

    for (x,y) <- testPairs do
      verifyOperation(x,y, BLFT.subtract, "subtraction")


  ////////////////////////////////////////////////////////////
  // multiplication
  ////////////////////////////////////////////////////////////

  test("multiplication matches exact rational reference"):

    val testPairs =
      Seq(
        (Rational(2,3), Rational(3,5)),
        (Rational(7,4), Rational(9,11)),
        (Rational(22,7), Rational(355,113)),
        (Rational(-5,3), Rational(7,4))
      )

    for (x,y) <- testPairs do
      verifyOperation(x,y, BLFT.multiply, "multiplication")


  ////////////////////////////////////////////////////////////
  // division
  ////////////////////////////////////////////////////////////

  test("division matches exact rational reference"):

    val testPairs =
      Seq(
        (Rational(3,5), Rational(7,4)),
        (Rational(22,7), Rational(355,113)),
        (Rational(-5,3), Rational(7,4))
      )

    for (x,y) <- testPairs do
      verifyOperation(x,y, BLFT.divide, "division")


  ////////////////////////////////////////////////////////////
  // deep CF chains
  ////////////////////////////////////////////////////////////

  test("deep CF chain correctness"):

    val x =
      Rational(355,113)

    val y =
      Rational(103993,33102)

    verifyOperation(x, y, BLFT.add, "deep addition")
    verifyOperation(x, y, BLFT.multiply, "deep multiplication")


// EOF: GosperBinaryEngineCorrectnessSpec.scala