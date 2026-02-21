// BEGIN FILE: GosperBinaryEngineStateSpec.scala v1
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

class GosperBinaryEngineStateSpec extends AnyFunSuite:

  test("engine produces identical result as rational reference"):

    val x = Rational(415,93)
    val y = Rational(13,5)

    val result =
      GosperBinaryEngine.run(
        BLFT.add,
        x.toContinuedFraction,
        y.toContinuedFraction
      )

    val expected =
      (x + y).toContinuedFraction

    assert(result.terms.toList == expected.terms.toList)


// END FILE: GosperBinaryEngineStateSpec.scala v1
