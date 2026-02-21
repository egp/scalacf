// BEGIN FILE: ConstantsSpec.scala
package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

final class ConstantsSpec extends AnyFunSuite:

  private def terms(cf: ContinuedFraction, n: Int): List[BigInt] =
    cf.terms.take(n).toList

  test("sqrt2 terms are [1; 2,2,2,...]"):

    val got =
      terms(Constants.sqrt2, 20)

    val expected =
      BigInt(1) :: List.fill(19)(BigInt(2))

    assert(got == expected)

  test("phi terms are [1; 1,1,1,...]"):

    val got =
      terms(Constants.phi, 25)

    val expected =
      List.fill(25)(BigInt(1))

    assert(got == expected)

  test("e terms follow [2; 1,2,1, 1,4,1, 1,6,1, ...]"):

    val got =
      terms(Constants.e, 16)

    val expected =
      List[BigInt](
        2,
        1, 2, 1,
        1, 4, 1,
        1, 6, 1,
        1, 8, 1,
        1, 10, 1
      )

    assert(got == expected)

  test("piPrefix matches OEIS A001203 prefix (first 50 terms)"):

    val got =
      terms(Constants.piPrefix, 50)

    val expected =
      Constants.piTermsPrefix98.take(50).toList

    assert(got == expected)

// END FILE: ConstantsSpec.scala
// EOF: ConstantsSpec.scala