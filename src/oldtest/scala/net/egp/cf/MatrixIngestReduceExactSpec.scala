package net.egp.cf
import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MatrixIngestReduceExactSpec extends AnyFunSuite:

  test("ingest then reduce matches algebra") {

    val m = Matrix.multiplyIdentity

    val m2 = m.ingestX(2)

    val value =
      m2(Rational(3,1), Rational(4,1)).get

    val digit = value.floor

    val reduced = m2.reduce(digit)

    val expected =
      Rational.one / (value - Rational(digit,1))

    val actual =
      reduced(Rational(3,1), Rational(4,1)).get

    assert(actual == expected)
  }
