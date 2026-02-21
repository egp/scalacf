// DiagnosticSpec.scala

package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class DiagnosticSpec extends AnyFunSuite:

  test("trace multiplication 2 * 3"):

    // DIRECT MATRIX TEST (add this)
    println(
      "Matrix test: " +
      Matrix.multiplyIdentity.apply(
        Rational(2,1),
        Rational(3,1)
      )
    )

    val two =
      ContinuedFraction.fromTerms(2)

    val three =
      ContinuedFraction.fromTerms(3)

    val prod =
      two * three

    println(
      "2*3 terms = " +
      prod.terms.take(10).toList
    )
