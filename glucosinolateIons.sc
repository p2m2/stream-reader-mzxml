#!/usr/bin/amm
import $cp.`target/scala-2.13/mzXML-stream-assembly-1.0.jar`
import cats.effect.IO
import fs2.text
import fs2.io.file.{Files, Path}

import cats.effect.unsafe.implicits._
import fr.inrae.p2m2.mzxml._
import fr.inrae.p2m2.mzxml.utils.ChemicalConst

/* check consecutive ions according a delta ppm and a delta rt */
/* criteria => maximum intensity of M0 */


@main
def main(
          mzXMLFile: String,
          noiseIntensity : Double,
          ppm_precision : Double   = 4,
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

    val p_const = Math.pow(10,6)

    SpectrumRequest(mzXMLFile)
      .msLevel(1)
      .filter(_.isDefined)
      .map(_.get)
      .filter(_.retentionTimeInSeconds.getOrElse(0) >= startTime)
      .filter(_.retentionTimeInSeconds.getOrElse(Int.MaxValue) <= endTime)
      .map {
        (spectrum: Spectrum) => {
          spectrum.peaks
            .filter {
              case (_, int) => int>noiseIntensity
            }
            .map {
              case (mz0, int0) =>
                val d0 : Double = mz0 + deltaMp0Mp1
                val d1 : Double = mz0 + deltaMp0Mp2
                val (mz1, int1) = spectrum.findClosestValueMz(d0)
                val (mz2, int2) = spectrum.findClosestValueMz(d1)

                val ppm_error_0 = (Math.abs( (mz1 - d0) / d0) * p_const)
                val ppm_error_1 = (Math.abs( (mz2 - d1) / d1) * p_const)

               // print(ppm_error_0,ppm_error_1,"\n")

                (ppm_error_0 < ppm_precision,ppm_error_1 < ppm_precision,(mz0, int0), (mz1, int1), (mz2, int2)) // isotopes
            }
            .filter {
              case (ppm0, ppm1, _, _, _) => ppm0 && ppm1
            }
            .map{
                case (_,_,v0, v1, v2) => (v0,v1,v2)
              }
            .filter {
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
            }
            .map {
              case (v0, v1, v2) => (
                spectrum.retentionTimeInSeconds.getOrElse(-1),
                spectrum.msLevel,
                spectrum.num,
                v0._1,v0._2,
                v1._1,v1._2,
                v2._1,v2._2
              )
            }
        }
      }
      .map(x => x.map(y => y.toList.map(_.toString()).mkString(";") + "\n").mkString)
      .filter(_.trim.nonEmpty)
      .through(text.utf8.encode)
      .through(Files[IO].writeAll(Path(outputFile)))
      .compile
      .drain
      .unsafeRunSync()

    println(s"****************$outputFile***********************")
    println(" ----- HEADER ----")
    println("RET_TIME;MS_LEVEL;NUM_SCAN;MS0;INT0;MS1;INT1;MS2;INT2")

}