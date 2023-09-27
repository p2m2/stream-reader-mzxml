

import Main._
import cats.effect.unsafe.implicits.global
import utest.{TestSuite, Tests, test}

object MainCTest extends TestSuite {

  val tests: Tests = Tests {
    /**
     * Generation of dump
     */
    test("") {
      val r = msInstrument
        .compile
        .drain
        .unsafeRunSync()
      println(r)
    }
  }

}