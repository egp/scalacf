/*
# scalacf
Streaming Arithmetic for Continued Fractions (Gosper / HAKMEM 101B)

## Overview

scalacf is a Scala implementation of Gosper’s arithmetic for continued
fractions, inspired by Bill Gosper’s work (notably HAKMEM 101B).

Core idea:

    Perform arithmetic directly on continued fractions, streaming digits
    lazily, without converting to floating-point or fully materializing
    rational approximations.

Instead of computing numeric values and then re-expanding them, we operate
symbolically using linear fractional transforms, emitting continued fraction
(CF) terms only when they are provably correct.

This approach supports:

  • Exact rational arithmetic
  • Streaming binary operations (+, −, ×, ÷)
  • Infinite continued fractions (√2, φ, e, π)
  • Conservative interval reasoning
  • Demand-driven digit production

Long-term goal example:

    sqrt(3/π² + e) / tanh(sqrt(5) - sin(69))

evaluated directly as continued fractions.


---------------------------------------------------------------------

High-Level Architecture

1. Core Mathematical Types

Rational
  Exact extended rational arithmetic:
    - finite rationals
    - ±∞
    - total ordering
    - invariant-preserving operations

Range
  Represents uncertainty:
    - exact
    - inside (interval)
    - outside (two-sided complement)

  Used to conservatively determine when a digit is safe to emit.

ContinuedFraction
  Represents:
    - finite CFs
    - infinite CFs via LazyList
    - streaming term generation

TermSource
  Abstraction for infinite generators.

  Implemented examples:
    - √2 (periodic infinite CF)
    - φ (golden ratio)
    - e (known pattern)
    - π (published OEIS terms)


---------------------------------------------------------------------

2. Transform Layer

BLFT — Binary Linear Fractional Transform

        axy + bx + cy + d
z(x,y)= ------------------
        exy + fx + gy + h

Used for binary arithmetic on continued fractions.

Supports:
  - emit
  - ingestX
  - ingestY
  - extractSafe
  - conservative range propagation


ULFT — Unary Linear Fractional Transform

       ax + b
f(x) = ------
       cx + d

Foundation for unary operations:
  - sqrt
  - exp
  - log
  - trig / hyperbolic functions
  - algebraic root iteration


---------------------------------------------------------------------

3. Engines

GosperBinaryEngine

Streaming arithmetic over two CF inputs:

  1. Maintain current BLFT transform.
  2. Maintain Range approximations for operands.
  3. If extractSafe succeeds → emit digit.
  4. Otherwise ingest from the operand that reduces uncertainty most.
  5. Repeat lazily.

Guarantees:
  - Correctness
  - Termination for finite inputs
  - No unsafe digit emission


GosperUnaryEngine

Unary analogue using ULFT.

Enables:
  - Newton iteration
  - Algebraic roots
  - Transcendental expansions


---------------------------------------------------------------------

4. Decision Layer

GosperDecision
UnaryDecision

Explicitly encode:
  - Emit vs ingest logic
  - Deterministic tie-breaking
  - Conservative safety policy

Separating decision logic improves:
  - Testability
  - Branch coverage
  - Reasoning about correctness


---------------------------------------------------------------------

Development Process

This project follows correctness-first, invariant-driven TDD.

Principles:

  - All arithmetic is exact (Rational)
  - All interval reasoning is conservative (Range)
  - No digit is emitted unless provably safe
  - All engines are lazy and bounded
  - Tests must terminate quickly
  - Coverage strengthens branch logic

Typical development steps:

  1. Define transform algebra
  2. Add invariant-preserving range propagation
  3. Write black-box correctness tests
  4. Add white-box branch coverage tests
  5. Prove termination behavior
  6. Commit only when all tests are green

After a green run:

    git add -A
    git commit -m "All tests green: <description>"
    git push


---------------------------------------------------------------------

Project Goals

Near-term:
  - Complete unary transform framework
  - Add algebraic root feedback (Gosper’s successive-approximation trick)
  - Expand transcendental support

Long-term:
  - Direct evaluation of complex symbolic CF expressions
  - Research-grade correctness guarantees
  - Reference implementation of Gosper arithmetic in Scala


---------------------------------------------------------------------

Why This Matters

Gosper’s method avoids premature approximation and digit waste.

Instead of computing approximations and discarding incorrect digits,
we produce only correct digits — and only when needed.

This project aims to make Gosper smile.
*/