package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MatrixExactReduceSpec extends AnyFunSuite:

  test("reduce preserves transform exactly") {

    val m = Matrix.multiplyIdentity

    val x = Rational(2,1)
    val y = Rational(3,1)

    val original = m(x,y).get

    val digit = original.floor

    val reduced = m.reduce(digit)

    val after = reduced(x,y).get

    assert(after == Rational.zero)
  }

  test("reduce maintains algebraic identity") {

    val m = Matrix.multiplyIdentity

    val reduced = m.reduce(6)

    val result = reduced(Rational(2,1), Rational(3,1)).get

    assert(result == Rational.zero)
  }
