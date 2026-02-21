// BEGIN FILE: UnaryDecisionSpec.scala
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

final class UnaryDecisionSpec extends AnyFunSuite:

  /**
   * A tiny deterministic unary transform used only for testing UnaryDecision.
   */
  final case class FakeUnary(
    safeDigit: Option[BigInt]
  ) extends UnaryTransform[FakeUnary]:

    def extractSafe(xRange: Range): Option[BigInt] =
      safeDigit

    def emit(d: BigInt): FakeUnary =
      this.copy(safeDigit = None)

    def ingestX(t: BigInt): FakeUnary =
      this.copy(safeDigit = None)

    def range(xRange: Range): Range =
      xRange


  test("nextAction emits when extractSafe is defined (regardless of term availability)"):

    val z = FakeUnary(Some(BigInt(7)))
    val xRange = Range.exact(Rational(3, 2))

    val a1 =
      UnaryDecision.nextAction(
        z = z,
        xTerm = None,
        xRange = xRange
      )
    assert(a1 == UnaryDecision.Emit(7))

    val a2 =
      UnaryDecision.nextAction(
        z = z,
        xTerm = Some(BigInt(123)),
        xRange = xRange
      )
    assert(a2 == UnaryDecision.Emit(7))


  test("nextAction ingests when no safe digit and a term is available"):

    val z = FakeUnary(None)
    val xRange = Range.inside(Rational(1, 1), Rational(2, 1))

    val a =
      UnaryDecision.nextAction(
        z = z,
        xTerm = Some(BigInt(4)),
        xRange = xRange
      )

    assert(a == UnaryDecision.IngestX)


  test("nextAction terminates when no safe digit and no term is available"):

    val z = FakeUnary(None)
    val xRange = Range.inside(Rational(1, 1), Rational(2, 1))

    val a =
      UnaryDecision.nextAction(
        z = z,
        xTerm = None,
        xRange = xRange
      )

    assert(a == UnaryDecision.Terminate)

// END FILE: UnaryDecisionSpec.scala