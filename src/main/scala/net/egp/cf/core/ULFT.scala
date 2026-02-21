// BEGIN FILE: ULFT.scala
package net.egp.cf.core

/**
 * ULFT — Unary Linear Fractional Transform
 *
 * Represents the Mobius transform:
 *
 *   f(x) = (a*x + b) / (c*x + d)
 *
 * where a,b,c,d are Rational (extended with ±∞ as used by Rational in this codebase).
 *
 * Design goals:
 *   - total Range propagation (never throws; conservative widening is allowed)
 *   - emission/ingestion are explicit Mobius compositions (Gosper-smile)
 *   - extractSafe is conservative: returns Some(d) only when provably safe over the whole range
 *
 * Notes on correctness:
 *   - On any interval not containing a pole (c*x + d = 0), f is monotone because:
 *       f'(x) = (ad - bc) / (c*x + d)^2
 *     so the image of an *inside* interval is determined by the endpoint images.
 *   - If a pole lies inside the input interval, the image is disconnected; we conservatively
 *     return an outside range (often all reals).
 */
final case class ULFT(
  a: Rational,
  b: Rational,
  c: Rational,
  d: Rational
):
  require(a != null && b != null && c != null && d != null, "ULFT coefficients must not be null")

  ////////////////////////////////////////////////////////////
  // evaluation
  ////////////////////////////////////////////////////////////

  /** Returns f(x) if defined; None for true undefined forms (0/0). */
  def evalOption(x: Rational): Option[Rational] =
    require(x != null, "x must not be null")
    try
      val num = a * x + b
      val den = c * x + d

      // If denominator is exactly zero:
      if den.isFinite && den.numerator == 0 then
        // 0/0 is undefined; otherwise treat as ±∞ (sign from numerator).
        if num.isFinite && num.numerator == 0 then None
        else if num > Rational(0, 1) then Some(PosInf)
        else if num < Rational(0, 1) then Some(NegInf)
        else None
      else
        Some(num / den)
    catch
      // Rational may throw on undefined ∞ forms; treat as "undefined".
      case _: Throwable => None

  /** Total evaluation: returns ±∞ for denom=0 (unless 0/0, then returns PosInf conservatively). */
  private def evalTotal(x: Rational): Rational =
    evalOption(x) match
      case Some(v) => v
      case None    => PosInf // conservative "unknown" -> tends to widen ranges


  ////////////////////////////////////////////////////////////
  // CF mechanics: emission / ingestion as Mobius compositions
  ////////////////////////////////////////////////////////////

  /**
   * Emit CF digit k from the output, i.e. transform y := f(x) into:
   *
   *   y' = 1 / (y - k)
   *
   * This is left-composition with E_k(y) = 1/(y-k), matrix:
   *   [ 0  1 ]
   *   [ 1 -k ]
   *
   * So:
   *   E_k ∘ f  =>  [0 1; 1 -k] * [a b; c d] = [c d; a-kc b-kd]
   */
  def emit(k: BigInt): ULFT =
    val kk = Rational(k, 1)
    ULFT(
      a = c,
      b = d,
      c = a - kk * c,
      d = b - kk * d
    )

  /**
   * Ingest next CF term t from the input x, i.e. substitute:
   *
   *   x = t + 1/x'
   *
   * This is right-composition with I_t(x') = t + 1/x', matrix:
   *   [ t  1 ]
   *   [ 1  0 ]
   *
   * So:
   *   f ∘ I_t  =>  [a b; c d] * [t 1; 1 0] = [a t + b, a; c t + d, c]
   */
  def ingestX(t: BigInt): ULFT =
    val tt = Rational(t, 1)
    ULFT(
      a = a * tt + b,
      b = a,
      c = c * tt + d,
      d = c
    )


  ////////////////////////////////////////////////////////////
  // Range propagation
  ////////////////////////////////////////////////////////////

  /**
   * Conservative image of the input range under this ULFT.
   *
   * Policy:
   *  - exact input -> exact output (when defined; otherwise widest outside)
   *  - inside interval without pole -> inside of endpoint images (ordered)
   *  - inside interval with pole -> outside, conservatively widened (often all reals)
   *  - outside input -> conservative (outside) by mapping the two "ends"
   */
  def range(x: Range): Range =
    require(x != null, "x must not be null")

    if x.isExact then
      val vOpt = evalOption(x.min)
      vOpt match
        case Some(v) => Range.exact(v)
        case None    => Range.outside(PosInf, NegInf) // totally undefined -> widest

    else if x.isInside then
      // Check whether a pole lies in [min,max] by testing whether 0 is contained in denom range.
      val denAtMin = evalDen(x.min)
      val denAtMax = evalDen(x.max)

      val polePossible = intervalContainsZero(denAtMin, denAtMax)

      if polePossible then
        // Discontinuity likely. Safest is "outside all reals" (i.e., unknown but correct).
        Range.outside(PosInf, NegInf)
      else
        val fMin = evalTotal(x.min)
        val fMax = evalTotal(x.max)
        Range.inside(minOf(fMin, fMax), maxOf(fMin, fMax))

    else
      // Outside: x ∈ (-∞, min] ∪ [max, +∞). We conservatively map "far ends".
      // Without explicit ±∞ support for endpoints, conservatively widen.
      // If Range uses finite endpoints, the safest is outside of mapped endpoints.
      val fMin = evalTotal(x.min)
      val fMax = evalTotal(x.max)
      // Outside should remain outside (and typically more uncertain than inside).
      Range.outside(maxOf(fMin, fMax), minOf(fMin, fMax))

  private def evalDen(x: Rational): Rational =
    try c * x + d
    catch case _: Throwable => PosInf

  private def intervalContainsZero(u: Rational, v: Rational): Boolean =
    // Conservative: if either endpoint is infinite or undefined-like, assume it might contain 0.
    if !u.isFinite || !v.isFinite then true
    else
      val zero = Rational(0, 1)
      (u == zero) || (v == zero) || ((u < zero) && (v > zero)) || ((v < zero) && (u > zero))


  ////////////////////////////////////////////////////////////
  // Safe digit extraction (CF term emission)
  ////////////////////////////////////////////////////////////

  /**
   * Returns Some(k) only if the next CF digit is provably the same for all values
   * in the output range of this transform applied to xRange.
   *
   * Conservative rule:
   *   - Compute r = range(xRange).
   *   - If r is inside/exact and both endpoints are finite and floor(min)==floor(max),
   *     then that floor is safe.
   *   - Otherwise None.
   */
  def extractSafe(xRange: Range): Option[BigInt] =
    val out = range(xRange)

    if out.isExact then
      if out.min.isFinite then Some(out.min.floor)
      else None

    else if out.isInside then
      if out.min.isFinite && out.max.isFinite then
        val lo = out.min.floor
        val hi = out.max.floor
        if lo == hi then Some(lo) else None
      else None

    else
      None


  ////////////////////////////////////////////////////////////
  // small helpers
  ////////////////////////////////////////////////////////////

  private def minOf(x: Rational, y: Rational): Rational = if x <= y then x else y
  private def maxOf(x: Rational, y: Rational): Rational = if x >= y then x else y

object ULFT:
  /** Identity transform: f(x)=x */
  val identity: ULFT =
    ULFT(Rational(1, 1), Rational(0, 1), Rational(0, 1), Rational(1, 1))

// END FILE: ULFT.scala