package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite

import net.egp.cf.core.*

class MatrixReduceInvariantExactSpec extends AnyFunSuite:

  test("Matrix.reduce satisfies exact Gosper invariant on integer grid") {

    val matrices =
      List(
        Matrix.multiplyIdentity,

        Matrix(
          1,2,3,4,
          5,6,7,8
        ),

        Matrix(
          3,1,4,1,
          5,9,2,6
        )
      )

    val digits =
      List(-2, -1, 0, 1, 2, 3)

    val inputs =
      for
        x <- -5 to 5
        y <- -5 to 5
        if x != 0 && y != 0
      yield (x,y)

    for
      m <- matrices
      t <- digits
      (xi, yi) <- inputs
    do

      val x = Rational(xi,1)
      val y = Rational(yi,1)

      val originalOpt = m.apply(x,y)

      if originalOpt.nonEmpty then

        val original = originalOpt.get

        val shifted =
          original - Rational(t,1)

        if shifted != Rational.zero then

          val expected =
            Rational.one / shifted

          val reduced = m.reduce(t)

          val actualOpt =
            reduced.apply(x,y)

          assert(
            actualOpt.nonEmpty,
            s"""
Matrix.reduce produced undefined result

Matrix: $m
digit:  $t
input:  ($x,$y)
"""
          )

          val actual =
            actualOpt.get

          assert(
            actual == expected,
            s"""
Gosper invariant violation

Matrix:    $m
digit:     $t
input:     ($x,$y)

original:  $original
expected:  $expected
actual:    $actual
"""
          )

  }

  test("Matrix.reduce preserves invariant for exact multiplication identity") {

    val m = Matrix.multiplyIdentity

    val x = Rational(2,1)
    val y = Rational(3,1)

    val original = m(x,y).get

    assert(original == Rational(6,1))

    val t = 6

    val reduced = m.reduce(t)

    val expected =
      Rational.one / (original - Rational(t,1))

    val actual =
      reduced(x,y)

    assert(actual.nonEmpty)

    assert(actual.get == expected)
  }

