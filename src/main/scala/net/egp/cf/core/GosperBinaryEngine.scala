// GosperBinaryEngine.scala
package net.egp.cf.core

/**
 * GosperBinaryEngine
 *
 * Streaming continued fraction arithmetic using
 * Bilinear Fractional Transformations (BLFT).
 *
 * Design-for-test (DfT) goals:
 *   - explicit preconditions / postconditions
 *   - avoid nulls
 *   - never emit digits unless extractSafe contract holds
 *   - handle conservative interval widening safely
 */
object GosperBinaryEngine:

  ////////////////////////////////////////////////////////////
  // choose ingestion operand
  ////////////////////////////////////////////////////////////

  /**
   * Choose whether to ingest X or Y based on uncertainty reduction.
   *
   * true  → ingest X
   * false → ingest Y
   */
  private def chooseOperandByRangeReduction(
    z: BLFT,
    xHead: Option[BigInt],
    yHead: Option[BigInt],
    xRange: Range,
    yRange: Range
  ): Boolean =

    require(z != null)
    require(xRange != null)
    require(yRange != null)

    val currentWidth =
      z.range(xRange, yRange).width

    // Sanity: width is either +∞ or non-negative
    require(
      currentWidth.isPosInf || currentWidth >= Rational(0,1),
      s"invalid currentWidth: $currentWidth"
    )

    val xWidth =
      xHead match
        case Some(digit) =>
          val newZ = z.ingestX(digit)
          require(newZ != null)

          val newRange =
            newZ.range(
              xRange.reciprocalSubtract(digit),
              yRange
            )

          require(newRange != null)
          newRange.width

        case None =>
          Rational.PositiveInfinity

    val yWidth =
      yHead match
        case Some(digit) =>
          val newZ = z.ingestY(digit)
          require(newZ != null)

          val newRange =
            newZ.range(
              xRange,
              yRange.reciprocalSubtract(digit)
            )

          require(newRange != null)
          newRange.width

        case None =>
          Rational.PositiveInfinity

    val result =
      xWidth <= yWidth

    result


  ////////////////////////////////////////////////////////////
  // main streaming loop
  ////////////////////////////////////////////////////////////

  private def loop(
    z: BLFT,
    xs: LazyList[BigInt],
    ys: LazyList[BigInt],
    xRange: Range,
    yRange: Range
  ): LazyList[BigInt] =

    require(z != null)
    require(xs != null)
    require(ys != null)
    require(xRange != null)
    require(yRange != null)

    z.extractSafe(xRange, yRange) match

      ////////////////////////////////////////////////////////
      // emit digit
      ////////////////////////////////////////////////////////

      case Some(digit) =>

        // DfT: extractSafe must never emit when range is unbounded
        val out = z.range(xRange, yRange)
        require(out != null)
        require(!out.min.isInfinite && !out.max.isInfinite, "extractSafe emitted on unbounded range")

        // DfT: emitted digit must match interval floor contract
        require(out.min.floor == out.max.floor, "extractSafe emitted without common floor")
        require(out.min.floor == digit, "extractSafe emitted digit inconsistent with floor")

        val newZ =
          z.emit(digit)

        require(newZ != null)

        val newXRange =
          xRange.reciprocalSubtract(digit)

        val newYRange =
          yRange.reciprocalSubtract(digit)

        digit #:: loop(
          newZ,
          xs,
          ys,
          newXRange,
          newYRange
        )

      ////////////////////////////////////////////////////////
      // ingest operand
      ////////////////////////////////////////////////////////

      case None =>

        val ingestX =
          chooseOperandByRangeReduction(
            z,
            xs.headOption,
            ys.headOption,
            xRange,
            yRange
          )

        if ingestX then

          xs match
            case head #:: tail =>

              val newZ =
                z.ingestX(head)

              require(newZ != null)

              val newXRange =
                xRange.reciprocalSubtract(head)

              loop(
                newZ,
                tail,
                ys,
                newXRange,
                yRange
              )

            case _ =>
              // No more x terms to ingest, cannot progress
              LazyList.empty

        else

          ys match
            case head #:: tail =>

              val newZ =
                z.ingestY(head)

              require(newZ != null)

              val newYRange =
                yRange.reciprocalSubtract(head)

              loop(
                newZ,
                xs,
                tail,
                xRange,
                newYRange
              )

            case _ =>
              // No more y terms to ingest, cannot progress
              LazyList.empty


  ////////////////////////////////////////////////////////////
  // public entrypoint
  ////////////////////////////////////////////////////////////

  /**
   * Run Gosper engine.
   *
   * Preconditions:
   *   z,x,y non-null
   *
   * Postconditions:
   *   result non-null
   *   result.terms non-null
   *
   * Note:
   *   For finite rationals the terms should terminate; for general streams
   *   termination is not guaranteed.
   */
  def run(
    z: BLFT,
    x: ContinuedFraction,
    y: ContinuedFraction
  ): ContinuedFraction =

    require(z != null)
    require(x != null)
    require(y != null)
    require(x.terms != null)
    require(y.terms != null)

    // Optional DfT fast-path for terminating CFs:
    // If both are finite, exact rational evaluation is definitive and
    // avoids some conservative interval widening corner cases.
    //
    // (Kept because your current codebase already references toRationalOption.)
    val xOpt = x.toRationalOption()
    val yOpt = y.toRationalOption()

    val resultTerms =
      (xOpt, yOpt) match
        case (Some(rx), Some(ry)) =>
          // exact reference
          z.eval(rx, ry).toContinuedFraction.terms
        case _ =>
          loop(
            z,
            x.terms,
            y.terms,
            x.range,
            y.range
          )

    require(resultTerms != null)

    val result =
      ContinuedFraction.fromTerms(resultTerms)

    require(result != null)
    require(result.terms != null)
    require(result.terms.nonEmpty, "GosperBinaryEngine produced empty CF")

    result


// EOF: GosperBinaryEngine.scala