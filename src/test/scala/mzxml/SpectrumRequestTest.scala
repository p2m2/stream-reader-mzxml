package mzxml

import utest.{TestSuite, Tests, test}
import cats.effect.unsafe.implicits.global
object SpectrumRequestTest extends TestSuite {
  val ex1 = "/LTQ_Orbitrap_precision32.mzXML"
  val ex2 = "/Orbitrap_Exploris_240_precision64.mzXML"

  val tests: Tests = Tests {
    test("precursorMz") {
      val r = SpectrumRequest(getClass.getResource(ex1).getPath).precursorMz(288.93,5000).map {
        case Some(r) => println(r.precursorMz(0).value)
        case None => None
      }
      val res = r.compile
        .toList
        .unsafeRunSync()
      println(res)
    }
  }
}
