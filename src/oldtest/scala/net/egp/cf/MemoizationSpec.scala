// MemoizationSpec.scala
package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

import java.util.concurrent.atomic.AtomicInteger

class MemoizationSpec extends AnyFunSuite:

  /**
   * Test generator that counts evaluations
   */
  class CountingSource(counter: AtomicInteger) extends TermSource:

    def terms: LazyList[BigInt] =
      LazyList.from(1).map { n =>
        counter.incrementAndGet()
        BigInt(n)
      }

  test("terms are memoized and not recomputed"):

    val counter = AtomicInteger(0)

    val cf =
      ContinuedFraction(
        new CountingSource(counter)
      )

    // First access computes 10 terms
    val first =
      cf.take(10)

    val countAfterFirst =
      counter.get()

    assert(countAfterFirst == 10)

    // Second access should compute ZERO additional terms
    val second =
      cf.take(10)

    val countAfterSecond =
      counter.get()

    assert(countAfterSecond == 10)

    assert(first == second)

  test("partial access only computes needed terms"):

    val counter = AtomicInteger(0)

    val cf =
      ContinuedFraction(
        new CountingSource(counter)
      )

    cf.take(5)

    assert(counter.get() == 5)

    cf.take(10)

    assert(counter.get() == 10)

  test("pi generator is memoized"):

    val pi = PiCF.generator

    val start1 = System.nanoTime()
    pi.take(2000)
    val end1 = System.nanoTime()

    val start2 = System.nanoTime()
    pi.take(2000)
    val end2 = System.nanoTime()

    val firstDuration  = end1 - start1
    val secondDuration = end2 - start2

    // Second access should be dramatically faster
    assert(secondDuration < firstDuration / 10)

    // Also verify correctness
    assert(pi.take(1) == Vector(3))
// EOF
