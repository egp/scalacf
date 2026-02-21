// GosperBinaryEngineStreamingSpec.scala
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

class GosperBinaryEngineStreamingSpec extends AnyFunSuite:

  test("streaming arithmetic matches exact evaluation"):

    val x =
      ContinuedFraction.fromTerms(
        LazyList(3,7,15,1,292)
      )

    val y =
      ContinuedFraction.fromTerms(
        LazyList(1,2,2,2,2,2)
      )

    val ops =
      List(
        BLFT.add,
        BLFT.subtract,
        BLFT.multiply,
        BLFT.divide
      )

    ops.foreach { op =>

      val exact =
        op.eval(
          x.toRational,
          y.toRational
        ).toContinuedFraction

      val streaming =
        GosperBinaryEngine.run(op, x, y)

      val exactTerms =
        exact.terms.take(10).toList

      val streamTerms =
        streaming.terms.take(10).toList

      assert(
        exactTerms == streamTerms,
        s"""
Streaming mismatch
Exact:   $exactTerms
Stream:  $streamTerms
"""
      )
    }


// EOF: GosperBinaryEngineStreamingSpec.scala