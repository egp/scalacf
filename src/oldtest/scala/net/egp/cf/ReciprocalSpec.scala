// ReciprocalSpec.scala
package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class ReciprocalSpec extends AnyFunSuite:

  test("reciprocal of integer"):

    val cf =
      ContinuedFraction.fromTerms(3)

    val rec =
      cf.reciprocal

    assert(
      rec.take(3) ==
      Vector(0,3)
    )

  test("reciprocal of pi begins with 0,3"):

    val rec =
      PiCF.generator.reciprocal

    assert(
      rec.take(2) ==
      Vector(0,3)
    )
// EOF
