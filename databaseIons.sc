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
          databaseCsvFile : String,
          noiseIntensity : Double,
          ppm_precision : Double   = 5,
          startTime : Double       = 0,
          endTime : Double         = Double.MaxValue,
        ) : Unit = {

  val dbname = databaseCsvFile.replace("\\.*","")
  val outputFile : String = mzXMLFile.split("/").last.replace(".mzXML",s"$dbname.ions.txt")

  val p_const = Math.pow(10,6)

  val db = Map( "ID1" -> 371.8996276)


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
            case (mzExp, _) =>
              (mzExp,db.filter {
                case (_, mz) =>
                  //abs(((mz_obs - mz_theoric) / mz_theoric) * 10^6)
                 // print(mzExp,mz,(Math.abs( (mzExp - mz) / mz) * p_const),"\n")
                  (Math.abs( (mzExp - mz) / mz) * p_const) < ppm_precision
              }.keys)
          }
          .filter {
            case (_,l) => l.nonEmpty
          }
          .map {
            case (v0,lIds) => (
              spectrum.retentionTimeInSeconds.getOrElse(-1),
              spectrum.msLevel,
              spectrum.num,
              v0,lIds,
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