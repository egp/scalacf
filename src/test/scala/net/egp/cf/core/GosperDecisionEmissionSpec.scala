////////////////////////////////////////////////////////////
// BEGIN FILE: GosperDecisionEmissionSpec.scala
////////////////////////////////////////////////////////////

package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

/**
 * GosperDecisionEmissionSpec
 *
 * Fast, bounded tests designed to execute the "emit" decision path and
 * validate its core contract:
 *
 *   If extractSafe(xr, yr) = Some(d), then d is the unique floor value
 *   across the output range z.range(xr,yr) (when defined).
 *
 * Important: after emitting d, we must update BOTH:
 *   z  := z.emit(d)
 *   xr := xr.reciprocalSubtract(d)
 *   yr := yr.reciprocalSubtract(d)
 *
 * For exact ranges, the "reference" eval must use the *current* exact
 * operand values represented by xr/yr, not the original x0/y0.
 *
 * Bounded: hard caps on steps; no test should run > 1s.
 */
final class GosperDecisionEmissionSpec extends AnyFunSuite:

  ////////////////////////////////////////////////////////////
  // helpers
  ////////////////////////////////////////////////////////////

  private final case class State(
      z: BLFT,
      xr: Range,
      yr: Range
  )

  private def stepEmitOnly(state: State): Option[State] =
    state.z.extractSafe(state.xr, state.yr).map { d =>
      State(
        state.z.emit(d),
        state.xr.reciprocalSubtract(d),
        state.yr.reciprocalSubtract(d)
      )
    }

  private def width(state: State): Rational =
    state.z.range(state.xr, state.yr).width

  private def floorMinMax(z: BLFT, xr: Range, yr: Range): Option[BigInt] =
    // Conservative: if endpoints’ floors agree, that digit is safe.
    // If evaluation is undefined for endpoints, return None.
    try
      val a = z.eval(xr.min, yr.min)
      val b = z.eval(xr.max, yr.max)
      val fa = a.floor
      val fb = b.floor
      if fa == fb then Some(fa) else None
    catch
      case _: Throwable => None

  ////////////////////////////////////////////////////////////
  // tests
  ////////////////////////////////////////////////////////////

  test("when extractSafe is defined, it matches floor over range endpoints"):

    val cases =
      List(
        // exact + exact => exact digit always safe
        State(BLFT.add, Range.exact(Rational(2, 1)), Range.exact(Rational(3, 1))),
        State(BLFT.multiply, Range.exact(Rational(2, 1)), Range.exact(Rational(3, 1))),
        // small inside ranges where floor is stable
        State(
          BLFT.add,
          Range.inside(Rational(2, 1), Rational(3, 1)),
          Range.inside(Rational(4, 1), Rational(5, 1))
        ),
        State(
          BLFT.subtract,
          Range.inside(Rational(9, 2), Rational(5, 1)),   // [4.5, 5]
          Range.inside(Rational(1, 1), Rational(3, 2))    // [1, 1.5]
        )
      )

    cases.foreach { s =>
      s.z.extractSafe(s.xr, s.yr) match
        case Some(d) =>
          val expectedOpt = floorMinMax(s.z, s.xr, s.yr)
          assert(expectedOpt.isDefined, s"Expected stable floor for case $s")
          assert(d == expectedOpt.get, s"extractSafe=$d but endpoint-floor=${expectedOpt.get} for case $s")
        case None =>
          // Allowed: not all ranges allow safe extraction.
          succeed
    }

  test("emission step preserves extractSafe contract for exact ranges (bounded)"):

    // Use exact inputs, but IMPORTANT: after each emission we must reference
    // the exact values represented by the *current* xr/yr, not the originals.
    var state =
      State(
        BLFT.add,
        Range.exact(Rational(355, 113)),
        Range.exact(Rational(22, 7))
      )

    var steps = 0
    while steps < 30 do

      state.z.extractSafe(state.xr, state.yr) match
        case Some(d) =>
          // For exact ranges, xr/yr each represent a single exact value.
          // The extracted digit must equal floor(z.eval(xExact, yExact)).
          assert(state.xr.isExact && state.yr.isExact)

          val xExact = state.xr.min
          val yExact = state.yr.min

          val exact = state.z.eval(xExact, yExact)
          assert(exact.floor == d, s"step $steps: expected floor=${exact.floor} got d=$d")

          val next = stepEmitOnly(state)
          assert(next.isDefined)
          state = next.get
          steps += 1

        case None =>
          // If extractSafe becomes undefined, we stop.
          steps = 30

    succeed

  test("emission should not increase output-range width for exact ranges"):

    val x = Rational(5, 3)
    val y = Rational(7, 4)

    val state =
      State(BLFT.add, Range.exact(x), Range.exact(y))

    val w0 = width(state)

    state.z.extractSafe(state.xr, state.yr) match
      case Some(_) =>
        val next = stepEmitOnly(state).get
        val w1 = width(next)
        assert(w1 <= w0, s"width increased: $w0 -> $w1")
      case None =>
        succeed

  test("emission reduces (or keeps) uncertainty on inside ranges when measurable (bounded)"):

    var state =
      State(
        BLFT.add,
        Range.inside(Rational(1, 1), Rational(5, 1)),
        Range.inside(Rational(2, 1), Rational(6, 1))
      )

    var i = 0
    while i < 20 do
      val w0 = width(state)

      stepEmitOnly(state) match
        case Some(next) =>
          val w1 = width(next)
          assert(w1 <= w0, s"width increased on emission: $w0 -> $w1")
          state = next
        case None =>
          i = 20

      i += 1

    succeed

////////////////////////////////////////////////////////////
// END FILE: GosperDecisionEmissionSpec.scala
////////////////////////////////////////////////////////////