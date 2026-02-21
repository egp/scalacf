// BEGIN FILE: UnaryDecision.scala
package net.egp.cf.core

/**
 * UnaryDecision
 *
 * Unary analogue of GosperDecision, for transforms over a single operand.
 *
 * Design intent (Gosper-smile):
 *   1) If extractSafe is defined, we MUST emit that digit (correctness invariant).
 *   2) Otherwise, ingest X if a term is available.
 *   3) If we cannot ingest, terminate.
 *
 * Notes:
 *   - This module *only* decides; it does not mutate state.
 *   - The transform interface is explicit (no reflective calls), so Scala 3 can typecheck fast.
 */
object UnaryDecision:

  ////////////////////////////////////////////////////////////
  // Transform interface (no reflection)
  ////////////////////////////////////////////////////////////

  /**
   * Minimal surface area required by the unary engine & decision logic.
   *
   * The self-type parameter `Z` ensures `emit/ingestX` preserve the concrete transform type.
   */
  trait UnaryTransform[Z]:
    extension (z: Z)
      /** Emit a proven-safe digit from the output continued fraction. */
      def emit(d: BigInt): Z

      /** Ingest the next continued-fraction term from the input. */
      def ingestX(t: BigInt): Z

      /** Conservative propagation of an input range through this transform. */
      def range(x: Range): Range

      /** Safe-digit extraction: Some(d) iff provably safe for all x in the input range. */
      def extractSafe(x: Range): Option[BigInt]


  ////////////////////////////////////////////////////////////
  // Action ADT
  ////////////////////////////////////////////////////////////

  sealed trait Action

  /** Emit a digit that is proven safe (i.e. extractSafe returned it). */
  final case class Emit(digit: BigInt) extends Action

  /** Ingest the next term from X. */
  case object IngestX extends Action

  /** No safe digit and no available ingestion term. */
  case object Terminate extends Action


  ////////////////////////////////////////////////////////////
  // Decision function
  ////////////////////////////////////////////////////////////

  def nextAction[Z](z: Z, xTerm: Option[BigInt], xRange: Range)(using ut: UnaryTransform[Z]): Action =
    require(z != null, "z must not be null")
    require(xRange != null, "xRange must not be null")

    ut.extractSafe(z)(xRange) match
      case Some(d) =>
        Emit(d)

      case None =>
        xTerm match
          case Some(_) => IngestX
          case None    => Terminate


  ////////////////////////////////////////////////////////////
  // ULFT instance
  ////////////////////////////////////////////////////////////

  given UnaryTransform[ULFT] with
    extension (z: ULFT)
      def emit(d: BigInt): ULFT = z.emit(d)
      def ingestX(t: BigInt): ULFT = z.ingestX(t)
      def range(x: Range): Range = z.range(x)
      def extractSafe(x: Range): Option[BigInt] = z.extractSafe(x)

// END FILE: UnaryDecision.scala