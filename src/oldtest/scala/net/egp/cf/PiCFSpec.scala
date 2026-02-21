// PiCFSpec.scala
package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite
import net.egp.cf.core.*

class PiCFSpec extends AnyFunSuite:

  test("pi continued fraction first 10 terms"):

    val pi =
      PiCF.generator

    val expected =
      Vector(
        3,7,15,1,292,1,1,1,2,1
      )

    assert(
      pi.take(10) == expected
    )
// EOF
