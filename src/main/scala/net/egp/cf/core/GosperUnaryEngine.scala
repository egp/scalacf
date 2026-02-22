// BEGIN FILE: GosperUnaryEngine.scala
package net.egp.cf.core

import net.egp.cf.core.UnaryDecision.*

/**
 * GosperUnaryEngine
 *
 * Streaming engine for unary transforms over a single continued fraction.
 *
 * Produces CF terms lazily by alternating:
 *   - Emit when extractSafe is defined
 *   - Otherwise ingest next input term (if any)
 *   - Terminate if neither is possible
 */
object GosperUnaryEngine:

  def run[Z <: UnaryTransform[Z]](
    z0: Z,
    xTerms0: LazyList[BigInt],
    xRange0: Range,
    maxDigits: Int = 256,
    maxSteps: Int = 100000
  ): LazyList[BigInt] =
    require(z0 != null, "z0 must not be null")
    require(xRange0 != null, "xRange0 must not be null")
    require(maxDigits >= 0, "maxDigits must be non-negative")
    require(maxSteps >= 0, "maxSteps must be non-negative")

    def loop(
      z: Z,
      xTerms: LazyList[BigInt],
      xRange: Range,
      emitted: Int,
      steps: Int
    ): LazyList[BigInt] =
      if emitted >= maxDigits || steps >= maxSteps then
        LazyList.empty
      else
        val xTermOpt = xTerms.headOption

        UnaryDecision.nextAction(z, xTermOpt, xRange) match
          case Emit(d) =>
            val z2 = z.emit(d)
            val x2 = xRange.reciprocalSubtract(d)
            d #:: loop(z2, xTerms, x2, emitted + 1, steps + 1)

          case IngestX =>
            xTermOpt match
              case Some(t) =>
                val z2 = z.ingestX(t)
                // Consuming a CF term t means: x' = 1 / (x - t)
                val x2 = xRange.reciprocalSubtract(t)
                loop(z2, xTerms.tail, x2, emitted, steps + 1)
              case None =>
                LazyList.empty

          case Terminate =>
            LazyList.empty

    loop(z0, xTerms0, xRange0, emitted = 0, steps = 0)

// END FILE: GosperUnaryEngine.scala