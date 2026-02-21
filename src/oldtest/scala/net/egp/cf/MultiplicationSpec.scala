// MultiplicationSpec.scala
package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MultiplicationSpec extends AnyFunSuite:

  test("2 * 3 = 6"):

    val a =
      ContinuedFraction.fromTerms(2)

    val b =
      ContinuedFraction.fromTerms(3)

    val product =
      a * b

    assert(
      product.take(1) ==
      Vector(6)
    )

  test("pi * 1 = pi"):

    val pi =
      PiCF.generator

    val one =
      ContinuedFraction.fromTerms(1)

    val product =
      pi * one

    assert(
      product.take(10) ==
      pi.take(10)
    )

  test("division via reciprocal"):

    val six =
      ContinuedFraction.fromTerms(6)

    val two =
      ContinuedFraction.fromTerms(2)

    val result =
      six / two

    assert(
      result.take(1) ==
      Vector(3)
    )
// EOF
