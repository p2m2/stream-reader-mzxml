

import Main._
import utest.{TestSuite, Tests, test}
import cats.effect.unsafe.implicits.global
object MainCTest extends TestSuite {

  val tests: Tests = Tests {
    /**
     * Generation of dump
     */
    test("") {
      val r =
        msRun(getClass.getResource("Orbitrap_Exploris_240_precision64.mzXML").getPath).map {
          case Some(run) => Some(run.scan.filter(s => s.properties.num < 20))
          case None => None
        }
        .compile
        .toList
        .unsafeRunSync()
      println(r)
    }
  }

}