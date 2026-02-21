package net.egp.cf
import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class MultiplicationExactIntegerSpec extends AnyFunSuite:

  test("2 * 3 emits single digit 6") {

    val a = ContinuedFraction.fromTerms(2)
    val b = ContinuedFraction.fromTerms(3)

    val result = (a * b).take(5)

    assert(result == Vector(6))
  }
