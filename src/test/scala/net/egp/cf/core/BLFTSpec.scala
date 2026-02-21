////////////////////////////////////////////////////////////
// BEGIN FILE: BLFTSpec.scala
////////////////////////////////////////////////////////////

package net.egp.cf.core

import org.scalatest.funsuite.AnyFunSuite

final class BLFTSpec extends AnyFunSuite:


  ////////////////////////////////////////////////////////////
  // eval correctness
  ////////////////////////////////////////////////////////////

  test("add eval correctness"):

    val x = Rational(2,3)
    val y = Rational(3,4)

    val result =
      BLFT.add.eval(x,y)

    assert(result == x + y)


  test("multiply eval correctness"):

    val x = Rational(2,3)
    val y = Rational(3,5)

    val result =
      BLFT.multiply.eval(x,y)

    assert(result == x * y)


  ////////////////////////////////////////////////////////////
  // range correctness
  ////////////////////////////////////////////////////////////

  test("range exact input produces exact output"):

    val xr = Range.exact(Rational(2,1))
    val yr = Range.exact(Rational(3,1))

    val zr =
      BLFT.add.range(xr,yr)

    assert(zr.isExact)
    assert(zr.min == Rational(5,1))


  ////////////////////////////////////////////////////////////
  // extractSafe correctness
  ////////////////////////////////////////////////////////////

  test("extractSafe exact"):

    val xr = Range.exact(Rational(2,1))
    val yr = Range.exact(Rational(3,1))

    val digit =
      BLFT.add.extractSafe(xr,yr)

    assert(digit.contains(5))


  test("extractSafe unsafe interval"):

    val xr =
      Range.inside(
        Rational(2,1),
        Rational(3,1)
      )

    val yr =
      Range.inside(
        Rational(4,1),
        Rational(5,1)
      )

    val digit =
      BLFT.add.extractSafe(xr,yr)

    assert(digit.isEmpty)


  ////////////////////////////////////////////////////////////
  // emit invariant
  ////////////////////////////////////////////////////////////

  test("emit produces valid transform"):

    val z =
      BLFT.add.emit(1)

    assert(z != null)


////////////////////////////////////////////////////////////
// END FILE: BLFTSpec.scala
////////////////////////////////////////////////////////////