

import Main._
import utest.{TestSuite, Tests, test}
import cats.effect.unsafe.implicits.global
import mzxml.ScanType
object MainCTest extends TestSuite {

  val tests: Tests = Tests {
    /**
     * Generation of dump
     */
    test("") {
      val ex1 = "LTQ_Orbitrap_precision32.mzXML"
      val ex2 = "Orbitrap_Exploris_240_precision64.mzXML"
      val r =
        msRun(getClass.getResource(ex2).getPath).map {
          case Some(run) => Some(run.scan.filter(s => s.properties.num == 1))
          case None => None
        }
        .compile
        .toList
        .unsafeRunSync()
      println(r)
    }
  }

}