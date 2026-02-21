// BEGIN FILE: GosperBinaryEngineSpec.scala v3
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

class GosperBinaryEngineSpec extends AnyFunSuite:

  private def cf(r: Rational): ContinuedFraction =
    r.toContinuedFraction

  private def rat(cf: ContinuedFraction): Rational =
    cf.toRational


  test("addition exact"):

    val x = Rational(3,2)
    val y = Rational(5,3)

    val result =
      GosperBinaryEngine.run(
        BLFT.add,
        cf(x),
        cf(y)
      )

    assert(rat(result) == x + y)


  test("multiplication exact"):

    val x = Rational(4,3)
    val y = Rational(5,7)

    val result =
      GosperBinaryEngine.run(
        BLFT.multiply,
        cf(x),
        cf(y)
      )

    assert(rat(result) == x * y)


  test("multi-step emission works"):

    val x = Rational(355,113)
    val y = Rational(22,7)

    val result =
      GosperBinaryEngine.run(
        BLFT.add,
        cf(x),
        cf(y)
      )

    assert(rat(result) == x + y)


// END FILE: GosperBinaryEngineSpec.scala v3
