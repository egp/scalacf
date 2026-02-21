// AlgebraicIdentitiesSpec.scala v1
package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class AlgebraicIdentitiesSpec extends AnyFunSuite:

  test("additive identity: x + 0 = x"):

    val x =
      PiCF.generator

    val zero =
      ContinuedFraction.fromTerms(0)

    assert(
      (x + zero).take(10) ==
      x.take(10)
    )


  test("multiplicative identity: x * 1 = x"):

    val x =
      PiCF.generator

    val one =
      ContinuedFraction.fromTerms(1)

    assert(
      (x * one).take(10) ==
      x.take(10)
    )


  test("multiplicative annihilator: x * 0 = 0"):

    val x =
      PiCF.generator

    val zero =
      ContinuedFraction.fromTerms(0)

    assert(
      (x * zero).take(5) ==
      Vector(0)
    )


  test("reciprocal identity: x * (1/x) = 1"):

    val x =
      ContinuedFraction.fromTerms(3,7,15,1)

    val one =
      ContinuedFraction.fromTerms(1)

    assert(
      (x * x.reciprocal).take(1) ==
      one.take(1)
    )


  test("division identity: x / 1 = x"):

    val x =
      PiCF.generator

    val one =
      ContinuedFraction.fromTerms(1)

    assert(
      (x / one).take(10) ==
      x.take(10)
    )


  test("double reciprocal: 1/(1/x) = x"):

    val x =
      PiCF.generator

    assert(
      x.reciprocal.reciprocal.take(10) ==
      x.take(10)
    )

// EOF
