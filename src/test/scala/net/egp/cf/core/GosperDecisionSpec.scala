////////////////////////////////////////////////////////////
// BEGIN FILE: GosperDecisionSpec.scala
////////////////////////////////////////////////////////////

package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

final class GosperDecisionSpec extends AnyFunSuite:

  ////////////////////////////////////////////////////////////
  // helpers
  ////////////////////////////////////////////////////////////

  private def cf(r: Rational): ContinuedFraction =
    r.toContinuedFraction

  private def exact(r: Rational): Range =
    Range.exact(r)

  ////////////////////////////////////////////////////////////
  // tests
  ////////////////////////////////////////////////////////////

  test("nextAction emits when extractSafe is defined, regardless of available terms"):

    val z  = BLFT.add
    val xr = exact(Rational(3, 1))
    val yr = exact(Rational(4, 1))

    // floor(3+4) = 7, and output is exact => safe
    assert(z.extractSafe(xr, yr).contains(7))

    val action =
      GosperDecision.nextAction(
        z = z,
        xTerm = None,
        yTerm = None,
        xRange = xr,
        yRange = yr
      )

    assert(action == GosperDecision.Emit(7))


  test("nextAction terminates when extractSafe is empty and no terms available"):

    val z  = BLFT.divide
    val xr = Range.inside(Rational(1, 2), Rational(2, 1))
    val yr = Range.inside(Rational(1, 2), Rational(2, 1))

    // Not safe in general; and no terms to ingest
    assert(z.extractSafe(xr, yr).isEmpty)

    val action =
      GosperDecision.nextAction(
        z = z,
        xTerm = None,
        yTerm = None,
        xRange = xr,
        yRange = yr
      )

    assert(action == GosperDecision.Terminate)


  test("nextAction chooses ingest side that yields less-uncertain output range (only when no safe digit)"):

    val z0 = BLFT.add

    // Choose ranges that make extractSafe empty (floors can differ)
    val xr = Range.inside(Rational(0, 1), Rational(1, 1))
    val yr = Range.inside(Rational(0, 1), Rational(1, 1))

    // Guardrail: this test is ONLY meaningful when emission is not possible.
    val safeOpt = z0.extractSafe(xr, yr)
    assert(
      safeOpt.isEmpty,
      s"Test fixture invalid: extractSafe unexpectedly defined as $safeOpt; " +
        s"in that case Emit must win."
    )

    // Provide both terms so a real ingest choice exists
    val xTerm0 = cf(Rational(355, 113)).terms.head
    val yTerm0 = cf(Rational(22, 7)).terms.head

    val action =
      GosperDecision.nextAction(
        z = z0,
        xTerm = Some(xTerm0),
        yTerm = Some(yTerm0),
        xRange = xr,
        yRange = yr
      )

    // Now the choice must be an ingest (never Emit, never Terminate)
    action match
      case GosperDecision.IngestX | GosperDecision.IngestY =>
        succeed
      case other =>
        fail(s"Expected ingest action when extractSafe is empty; got $other")


  test("nextAction tie-breaks to IngestX when resulting ranges compare equal"):

    val z0 = BLFT.add

    // Symmetric situation where ingestX/ingestY tend to be comparable
    val xr = Range.inside(Rational(1, 1), Rational(2, 1))
    val yr = Range.inside(Rational(1, 1), Rational(2, 1))

    val safeOpt = z0.extractSafe(xr, yr)
    assert(safeOpt.isEmpty)

    val term = BigInt(1)

    val action =
      GosperDecision.nextAction(
        z = z0,
        xTerm = Some(term),
        yTerm = Some(term),
        xRange = xr,
        yRange = yr
      )

    // If equal, policy is IngestX
    assert(action == GosperDecision.IngestX)


  test("nextAction returns IngestX when only xTerm is available and no safe digit"):

    val z0 = BLFT.multiply

    val xr = Range.inside(Rational(1, 2), Rational(3, 2))
    val yr = Range.inside(Rational(1, 2), Rational(3, 2))

    assert(z0.extractSafe(xr, yr).isEmpty)

    val action =
      GosperDecision.nextAction(
        z = z0,
        xTerm = Some(BigInt(1)),
        yTerm = None,
        xRange = xr,
        yRange = yr
      )

    assert(action == GosperDecision.IngestX)


  test("nextAction returns IngestY when only yTerm is available and no safe digit"):

    val z0 = BLFT.subtract

    val xr = Range.inside(Rational(1, 2), Rational(3, 2))
    val yr = Range.inside(Rational(1, 2), Rational(3, 2))

    assert(z0.extractSafe(xr, yr).isEmpty)

    val action =
      GosperDecision.nextAction(
        z = z0,
        xTerm = None,
        yTerm = Some(BigInt(1)),
        xRange = xr,
        yRange = yr
      )

    assert(action == GosperDecision.IngestY)


////////////////////////////////////////////////////////////
// END FILE: GosperDecisionSpec.scala
////////////////////////////////////////////////////////////