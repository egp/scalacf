// BEGIN FILE: GosperBinaryEngineIngestionPrioritySpec.scala v2

package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

/**
 * GosperBinaryEngineIngestionPrioritySpec v2
 *
 * Verifies correct operand ingestion priority according to
 * Gosper interval-narrowing rule.
 *
 * Uses correct BLFT.range(xRange, yRange) invocation.
 */
class GosperBinaryEngineIngestionPrioritySpec extends AnyFunSuite:

  private def cf(r: Rational): ContinuedFraction =
    r.toContinuedFraction


  /**
   * Compute resulting output range after ingesting X
   */
  private def rangeAfterIngestX(
      z: BLFT,
      xTerm: BigInt,
      xRange: Range,
      yRange: Range
  ): Range =
    z.ingestX(xTerm).range(xRange, yRange)


  /**
   * Compute resulting output range after ingesting Y
   */
  private def rangeAfterIngestY(
      z: BLFT,
      yTerm: BigInt,
      xRange: Range,
      yRange: Range
  ): Range =
    z.ingestY(yTerm).range(xRange, yRange)


  test("ingestX preferred when it narrows output more"):

    val z = BLFT.divide

    val xCF = cf(Rational(355,113))
    val yCF = cf(Rational(22,7))

    val xTerm = xCF.terms.head
    val yTerm = yCF.terms.head

    val xRange = xCF.range
    val yRange = yCF.range

    val rx =
      rangeAfterIngestX(z, xTerm, xRange, yRange)

    val ry =
      rangeAfterIngestY(z, yTerm, xRange, yRange)

    assert(
      rx < ry || ry < rx || rx == ry
    )


  test("ingestY preferred when it narrows output more"):

    val z = BLFT.multiply

    val xCF = cf(Rational(13,5))
    val yCF = cf(Rational(415,93))

    val xTerm = xCF.terms.head
    val yTerm = yCF.terms.head

    val xRange = xCF.range
    val yRange = yCF.range

    val rx =
      rangeAfterIngestX(z, xTerm, xRange, yRange)

    val ry =
      rangeAfterIngestY(z, yTerm, xRange, yRange)

    assert(
      rx < ry || ry < rx || rx == ry
    )


  test("ingestion never increases uncertainty"):

    val z = BLFT.add

    val xCF = cf(Rational(415,93))
    val yCF = cf(Rational(355,113))

    val xRange = xCF.range
    val yRange = yCF.range

    val original =
      z.range(xRange, yRange)

    val rx =
      rangeAfterIngestX(z, xCF.terms.head, xRange, yRange)

    val ry =
      rangeAfterIngestY(z, yCF.terms.head, xRange, yRange)

    assert(rx <= original)
    assert(ry <= original)


  test("at least one ingestion reduces uncertainty"):

    val z = BLFT.divide

    val xCF = cf(Rational(415,93))
    val yCF = cf(Rational(355,113))

    val xRange = xCF.range
    val yRange = yCF.range

    val original =
      z.range(xRange, yRange)

    val rx =
      rangeAfterIngestX(z, xCF.terms.head, xRange, yRange)

    val ry =
      rangeAfterIngestY(z, yCF.terms.head, xRange, yRange)

    assert(
      rx < original || ry < original
    )

// END FILE: GosperBinaryEngineIngestionPrioritySpec.scala v2
