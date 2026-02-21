package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MatrixIngestSpec extends AnyFunSuite:

  test("ingestX transforms correctly") {

    val m = Matrix.multiplyIdentity

    val m2 = m.ingestX(2)

    val r =
      m2.apply(
        Rational(1,1),
        Rational(3,1)
      )

    assert(r.nonEmpty)
  }

  test("ingestY transforms correctly") {

    val m = Matrix.multiplyIdentity

    val m2 = m.ingestY(3)

    val r =
      m2.apply(
        Rational(2,1),
        Rational(1,1)
      )

    assert(r.nonEmpty)
  }
