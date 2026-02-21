package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MatrixReduceExactPointSpec extends AnyFunSuite:

  test("reduce preserves exact transform for singleton inputs") {

    val m = Matrix.multiplyIdentity

    val x = Rational(2,1)
    val y = Rational(3,1)

    val original = m(x,y).get

    val digit = original.floor

    val reduced = m.reduce(digit)

    val expected =
      Rational.one / (original - Rational(digit,1))

    val actual =
      reduced(x,y).get

    assert(actual == expected)
  }
