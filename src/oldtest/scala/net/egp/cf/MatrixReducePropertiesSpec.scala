package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MatrixReducePropertiesSpec extends AnyFunSuite:

  test("reduce produces zero remainder when exact") {

    val m =
      Matrix.multiplyIdentity

    val result =
      m.apply(Rational(2,1), Rational(3,1)).get

    val digit =
      result.floor

    val reduced =
      m.reduce(digit)

    val rem =
      reduced.apply(Rational(2,1), Rational(3,1))

    assert(rem.contains(Rational.zero))
  }
