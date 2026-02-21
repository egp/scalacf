// BEGIN FILE: GosperUnaryEngine.scala
package net.egp.cf.core

/**
 * GosperUnaryEngine
 *
 * Unary streaming CF engine:
 *   - maintains a unary transform z and an operand Range approximation
 *   - repeatedly decides Emit vs Ingest based on UnaryDecision
 *
 * Safety:
 *   - LazyList-based, demand-driven
 *   - terminates when no emission is possible and no more input terms are available
 */
object GosperUnaryEngine:

  import UnaryDecision.{Action, Emit, IngestX, Terminate}

  /**
   * Run unary streaming arithmetic.
   *
   * @param z0 initial unary transform
   * @param x  operand continued fraction (may be finite or infinite)
   */
  def run[Z <: UnaryTransform[Z]](
    z0: Z,
    x: ContinuedFraction
  ): ContinuedFraction =
    require(z0 != null, "z0 must not be null")
    require(x != null, "x must not be null")

    val x0 = Range.fromCF(x)

    val outTerms =
      loop(
        z = z0,
        xTerms = x.terms,
        xRange = x0
      )

    ContinuedFraction.fromTerms(outTerms)


  ////////////////////////////////////////////////////////////
  // core loop
  ////////////////////////////////////////////////////////////

  private def loop[Z <: UnaryTransform[Z]](
    z: Z,
    xTerms: LazyList[BigInt],
    xRange: Range
  ): LazyList[BigInt] =

    val xTermOpt =
      if xTerms.isEmpty then None else Some(xTerms.head)

    UnaryDecision.nextAction(z, xTermOpt, xRange) match

      case Emit(d) =>
        val z2 = z.emit(d)
        val x2 = xRange.reciprocalSubtract(d)
        d #:: loop(z2, xTerms, x2)

      case IngestX =>
        xTermOpt match
          case Some(t) =>
            val z2 = z.ingestX(t)
            val x2 = xRange.reciprocalSubtract(t)
            loop(z2, xTerms.tail, x2)

          case None =>
            // Defensive termination (should be unreachable by decision logic)
            LazyList.empty

      case Terminate =>
        LazyList.empty

// END FILE: GosperUnaryEngine.scala