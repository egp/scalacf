package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MatrixReduceInvariantSpec extends AnyFunSuite:

  test("reduce identity example") {

    val m =
      Matrix.multiplyIdentity

    val reduced =
      m.reduce(6)

    val result =
      reduced.apply(
        Rational(2,1),
        Rational(3,1)
      )

    assert(result.contains(Rational.zero))
  }

  test("reduce preserves algebraic correctness") {

    val m =
      Matrix.multiplyIdentity

    val r =
      m.apply(
        Rational(5,1),
        Rational(7,1)
      ).get

    val digit =
      r.floor

    val reduced =
      m.reduce(digit)

    val remainder =
      reduced.apply(
        Rational(5,1),
        Rational(7,1)
      ).get

    assert(remainder.floor == 0)
  }
