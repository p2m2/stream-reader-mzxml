#!/usr/bin/amm
import $cp.`target/scala-2.13/mzXML-stream-assembly-1.0.jar`
import cats.effect.IO
import fs2.text
import fs2.io.file.{Files, Path}

import cats.effect.unsafe.implicits._
import fr.inrae.p2m2.mzxml._
import fr.inrae.p2m2.mzxml.utils.ChemicalConst


@main
def main(
          mzXMLFile: String,
          startTime : Double       = 0,
          endTime : Double         = Double.MaxValue,
          numberCarbonMin : Double = 3,
          numberCarbonMax : Double = 35,
          numberSulfurMin : Double = 1.5,
          numberSulfurMax : Double = 5,
          deltaMp0Mp1     : Double = 1.0,
          deltaMp0Mp2     : Double = 1.996  // Glucosinolate filter
        ) : Unit = {

    val outputFile : String = mzXMLFile.split("/").last.replace(".mzXML",".ions.txt")

    SpectrumRequest(mzXMLFile)
      .msLevel(1)
      .filter(_.isDefined)
      .map(_.get)
      .filter(_.retentionTimeInSeconds.getOrElse(0) >= startTime)
      .filter(_.retentionTimeInSeconds.getOrElse(Int.MaxValue) <= endTime)
      .map {
        (spectrum: Spectrum) => {
          spectrum.peaks.map {
              case (mz0, int0) =>
                val (mz1, int1) = spectrum.findClosestValueMz(mz0 + deltaMp0Mp1)
                val (mz2, int2) = spectrum.findClosestValueMz(mz0 + deltaMp0Mp2)
                ((mz0, int0), (mz1, int1), (mz2, int2)) // isotopes

            }.filter {
              case (v0, v1, _) =>
                v1._2 >= v0._2 *
                  (ChemicalConst.abundanceIsotope("C")(1) * numberCarbonMin +
                    ChemicalConst.abundanceIsotope("S")(1) * numberSulfurMin)

            }.filter {
              case (v0, v1, _) =>
                v1._2 < v0._2 *
                  (ChemicalConst.abundanceIsotope("C")(1) * numberCarbonMax +
                    ChemicalConst.abundanceIsotope("S")(1) * numberSulfurMax)

            } /* criteria M2 of Isotope S are present 4.4 % */
            .filter {
              case (v0, _, v2) =>
                v2._2 >= v0._2 * ChemicalConst.abundanceIsotope("S")(2) * numberSulfurMin
            }
            .filter {
              case (v0, _, v2) =>
                v2._2 < v0._2 * ChemicalConst.abundanceIsotope("S")(2) * numberSulfurMax
            }.map(
              (spectrum.retentionTimeInSeconds.getOrElse(-1), _)
            )
        }
      }
      .map(x => x.map(y => y.toString() + "\n").mkString("\n"))
      .through(text.utf8.encode)
      .through(Files[IO].writeAll(Path(outputFile)))
      .compile
      .drain
      .unsafeRunSync()

    println(s"****************$outputFile**********")
  
}