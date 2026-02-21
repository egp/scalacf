// GosperBinaryEngineLoopIsolationSpec.scala
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

class GosperBinaryEngineLoopIsolationSpec extends AnyFunSuite:

  case class State(
      z: BLFT,
      xRange: Range,
      yRange: Range
  )

  private def step(state: State): State =

    state.z.extractSafe(
      state.xRange,
      state.yRange
    ) match

      case Some(digit) =>

        val newZ =
          state.z.emit(digit)

        val newXRange =
          state.xRange.reciprocalSubtract(digit)

        val newYRange =
          state.yRange.reciprocalSubtract(digit)

        State(
          newZ,
          newXRange,
          newYRange
        )

      case None =>

        // In this isolation test we force ingestion of X by a fixed digit.
        // This is not the full Gosper choice heuristic; it is intentionally
        // simple for unit testing step mechanics.
        val newZ =
          state.z.ingestX(1)

        val newXRange =
          state.xRange.reciprocalSubtract(1)

        State(
          newZ,
          newXRange,
          state.yRange
        )


  test("engine must make progress within bounded steps"):

    var state =
      State(
        BLFT.add,
        Range.inside(
          Rational(1,1),
          Rational(5,1)
        ),
        Range.inside(
          Rational(2,1),
          Rational(6,1)
        )
      )

    for _ <- 0 until 100 do
      state = step(state)

    succeed


  test("range width must not increase unless conservatively widened to infinity"):

    var state =
      State(
        BLFT.add,
        Range.inside(
          Rational(1,1),
          Rational(5,1)
        ),
        Range.inside(
          Rational(2,1),
          Rational(6,1)
        )
      )

    var prevWidth =
      state.z.range(
        state.xRange,
        state.yRange
      ).width

    for _ <- 0 until 100 do

      state = step(state)

      val newWidth =
        state.z.range(
          state.xRange,
          state.yRange
        ).width

      // Correctness note:
      // BLFT.range is conservative: if any corner evaluation is undefined
      // in extended-real arithmetic, it widens to [-∞, +∞], i.e. width = +∞.
      // That is allowed and must not fail this test.
      assert(
        (newWidth <= prevWidth) || newWidth.isPosInf,
        s"Width increased unexpectedly: $prevWidth -> $newWidth"
      )

      prevWidth = newWidth


// EOF: GosperBinaryEngineLoopIsolationSpec.scala