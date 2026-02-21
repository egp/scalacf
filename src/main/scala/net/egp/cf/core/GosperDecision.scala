// GosperDecision.scala
package net.egp.cf.core

/**
 * GosperDecision
 *
 * Centralizes the "emit vs ingest" decision used by GosperBinaryEngine, in a way
 * that is:
 *   - explicit (good for reasoning, testing, and coverage)
 *   - deterministic
 *   - conservative (prefers correctness over cleverness)
 *
 * Design intent (Gosper-smile):
 *   1) If extractSafe is defined, we MUST emit that digit (correctness invariant).
 *   2) Otherwise, ingest the operand that reduces the resulting output Range
 *      uncertainty the most (based on Range ordering).
 *   3) If we cannot ingest (no next terms), terminate.
 */
object GosperDecision:

  ////////////////////////////////////////////////////////////
  // Action ADT
  ////////////////////////////////////////////////////////////

  sealed trait Action

  /** Emit a digit that is proven safe (i.e. extractSafe returned it). */
  final case class Emit(digit: BigInt) extends Action

  /** Ingest the next term from X. */
  case object IngestX extends Action

  /** Ingest the next term from Y. */
  case object IngestY extends Action

  /** No safe digit and no available ingestion terms. */
  case object Terminate extends Action


  ////////////////////////////////////////////////////////////
  // Decision function
  ////////////////////////////////////////////////////////////

  /**
   * Decide the next engine action from the current transform and operand ranges.
   *
   * @param z     current BLFT transform
   * @param xTerm next available term from X (if any)
   * @param yTerm next available term from Y (if any)
   * @param xRange current range approximation for X
   * @param yRange current range approximation for Y
   */
  def nextAction(
    z: BLFT,
    xTerm: Option[BigInt],
    yTerm: Option[BigInt],
    xRange: Range,
    yRange: Range
  ): Action =
    require(z != null, "z must not be null")
    require(xRange != null, "xRange must not be null")
    require(yRange != null, "yRange must not be null")

    // Rule (1): if safe digit exists, emit it.
    z.extractSafe(xRange, yRange) match
      case Some(digit) =>
        Emit(digit)

      case None =>
        // Rule (2): no safe digit -> choose ingestion that reduces uncertainty most.
        (xTerm, yTerm) match
          case (Some(xt), Some(yt)) =>
            // Compare the resulting output ranges after ingesting X vs Y.
            // Prefer the "less uncertain" Range (as per Range.compare / ordering).
            val rx = z.ingestX(xt).range(xRange, yRange)
            val ry = z.ingestY(yt).range(xRange, yRange)

            // Deterministic tie-break: prefer X if equal.
            if rx <= ry then IngestX else IngestY

          case (Some(_), None) =>
            IngestX

          case (None, Some(_)) =>
            IngestY

          case (None, None) =>
            // Rule (3): cannot emit and cannot ingest => terminate.
            Terminate

//EOF GosperDecision.scala