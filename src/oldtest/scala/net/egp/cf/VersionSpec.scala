// VersionSpec.scala v3
package net.egp.cf

import org.scalatest.funsuite.AnyFunSuite

import net.egp.cf.core.*

class VersionSpec extends AnyFunSuite:

  test("Production source versions are synchronized"):

    val versions =
      Map(
        "CFAddition"       -> CFAddition.Version,
        "CFMultiplication" -> CFMultiplication.Version,
        "CFRange"          -> CFRange.Version,
        "ContinuedFraction"-> ContinuedFraction.Version,
        "Matrix"           -> Matrix.Version,
        "Rational"         -> Rational.Version
      )

    println("\n=== Production Source Versions ===")

    versions.toSeq.sortBy(_._1).foreach { case (name, version) =>
      println(f"$name%-20s v$version")
    }

    println("==================================\n")

    //
    // REQUIRED SYNCHRONIZED VERSIONS
    // Update these whenever production code changes.
    //


    assert(CFAddition.Version        == 3)
    assert(CFMultiplication.Version  == 15)
    assert(CFRange.Version           == 40)
    assert(ContinuedFraction.Version == 3)
    assert(Matrix.Version            == 15)
    assert(Rational.Version          == 8)

// EOF
