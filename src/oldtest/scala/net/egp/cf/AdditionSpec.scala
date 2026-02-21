// AdditionSpec.scala
package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class AdditionSpec extends AnyFunSuite:

  test("1 + 1 = 2"):

    val a =
      ContinuedFraction.fromTerms(1)

    val b =
      ContinuedFraction.fromTerms(1)

    val sum =
      a + b

    assert(
      sum.take(1) ==
      Vector(2)
    )

  test("pi + 1 begins with 4"):

    val sum =
      PiCF.generator +
      ContinuedFraction.fromTerms(1)

    assert(
      sum.take(1) ==
      Vector(4)
    )
// EOF
