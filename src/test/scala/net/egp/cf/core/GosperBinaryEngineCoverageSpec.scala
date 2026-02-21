// GosperBinaryEngineCoverageSpec.scala
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

/**
 * GosperBinaryEngineCoverageSpec
 *
 * Goal:
 *   Drive additional (previously uncovered) paths in GosperBinaryEngine
 *   without introducing slow or non-terminating tests.
 *
 * Philosophy:
 *   - Bounded: always take a finite prefix of any LazyList.
 *   - Black-box: verify observable results (Rational equality, term prefix).
 *   - Deterministic: fixed operands, fixed operation set.
 */
final class GosperBinaryEngineCoverageSpec extends AnyFunSuite:

  ////////////////////////////////////////////////////////////
  // helpers
  ////////////////////////////////////////////////////////////

  private def cf(r: Rational): ContinuedFraction =
    r.toContinuedFraction

  private def rat(cf: ContinuedFraction): Rational =
    cf.toRationalOption() match
      case Some(r) => r
      case None =>
        fail(s"Expected finite continued fraction to convert to Rational, but got: $cf")

  private def verifyExact(
    name: String,
    op: BLFT,
    x: Rational,
    y: Rational
  ): Unit =

    val expected: Rational =
      op.eval(x, y)

    val out: ContinuedFraction =
      GosperBinaryEngine.run(op, cf(x), cf(y))

    val actual: Rational =
      rat(out)

    assert(
      actual == expected,
      s"""
         |$name mismatch
         |x: $x
         |y: $y
         |expected: $expected
         |actual:   $actual
         |""".stripMargin
    )

  ////////////////////////////////////////////////////////////
  // tests
  ////////////////////////////////////////////////////////////

  test("coverage: addition across signs"):

    val cases =
      Seq(
        (Rational(1, 2), Rational(1, 3)),
        (Rational(-5, 3), Rational(7, 4)),
        (Rational(0, 1), Rational(9, 7)),
        (Rational(-1, 2), Rational(-2, 3))
      )

    cases.foreach { (x, y) =>
      verifyExact("add", BLFT.add, x, y)
    }

  test("coverage: subtraction across signs"):

    val cases =
      Seq(
        (Rational(5, 2), Rational(3, 2)),
        (Rational(7, 4), Rational(5, 6)),
        (Rational(0, 1), Rational(1, 7)),
        (Rational(-5, 3), Rational(7, 4))
      )

    cases.foreach { (x, y) =>
      verifyExact("subtract", BLFT.subtract, x, y)
    }

  test("coverage: multiplication including zeros"):

    val cases =
      Seq(
        (Rational(2, 3), Rational(3, 5)),
        (Rational(0, 1), Rational(7, 4)),
        (Rational(-5, 3), Rational(7, 4)),
        (Rational(-2, 1), Rational(-3, 7))
      )

    cases.foreach { (x, y) =>
      verifyExact("multiply", BLFT.multiply, x, y)
    }

  test("coverage: division including negative results"):

    val cases =
      Seq(
        (Rational(3, 5), Rational(7, 4)),
        (Rational(-5, 3), Rational(7, 4)),
        (Rational(22, 7), Rational(355, 113)),
        (Rational(5, 2), Rational(-3, 2))
      )

    cases.foreach { (x, y) =>
      verifyExact("divide", BLFT.divide, x, y)
    }

  test("coverage: bounded term prefix is produced"):

    val x = Rational(355, 113)
    val y = Rational(22, 7)

    val z =
      GosperBinaryEngine.run(
        BLFT.add,
        cf(x),
        cf(y)
      )

    // Bounded: do not force the whole stream.
    val prefix =
      z.terms.take(25).toList

    assert(prefix.nonEmpty)
    assert(prefix.forall(_.isInstanceOf[BigInt]))

//EOF GosperBinaryEngineCoverageSpec.scala