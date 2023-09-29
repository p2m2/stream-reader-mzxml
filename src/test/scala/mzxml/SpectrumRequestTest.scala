package mzxml

import utest.{TestSuite, Tests, test}
import cats.effect.unsafe.implicits.global
object SpectrumRequestTest extends TestSuite {
  val ex1 = "/LTQ_Orbitrap_precision32.mzXML"
  val ex2 = "/Orbitrap_Exploris_240_precision64.mzXML"

  val tests: Tests = Tests {
    test("precursorMz") {
      val r = SpectrumRequest(getClass.getResource(ex1).getPath).precursorMz(288.93,9000).map {
        case Some(r) => println(r.precursorMz.head.value, r.precursorMz.head.precursorIntensity)
        case None => None
      }

      r
        .compile
        .toList
        .unsafeRunSync()
    }

    test("msLevel 2") {
      val r = SpectrumRequest(getClass.getResource(ex2).getPath).msLevel(2).map {
        case Some(r) => println(r.peaks.length)
        case None => None
      }
      r.compile
        .toList
        .unsafeRunSync()
    }
  }
}
