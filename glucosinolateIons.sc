import cats.effect.IO
import fs2.text
import fs2.io.file.{Files, Path}
import fs2.Stream
import cats.effect.unsafe.implicits._
import $file.libCandidateIons

/* check consecutive ions according a delta ppm and a delta rt */
/* criteria => maximum intensity of M0 */


@main
def main(
          mzXMLFile: String,
          noiseIntensity : Double,
          ppm_precision : Double   = 4,
          startTime : Double       = 0,
          endTime : Double         = Double.MaxValue
        ) : Unit = {

  val outputFile : String = mzXMLFile.split("/").last.replace(".mzXML",".ions.txt")

  val selectedIonsFromMs1: Stream[IO, Seq[(Int, Int, Int, Double, Double, Double, Double, Double, Double)]] =
    libCandidateIons.candidateIonsGeneric(mzXMLFile,noiseIntensity,ppm_precision,startTime,endTime)
/*
  val selectedIonsPrecursorMz = SpectrumRequest(mzXMLFile)
      .msLevel(2)
      .filter(_.isDefined)
      .map(_.get)
      .filter(_.retentionTimeInSeconds.getOrElse(0) >= startTime)
      .filter(_.retentionTimeInSeconds.getOrElse(Int.MaxValue) <= endTime)
      .map { (spectrum: Spectrum) => spectrum.precursorMz.map(_.value) }

  val ionsList = for {
    q1 <- selectedIonsFromMs1
    q2 <- selectedIonsPrecursorMz
  } yield {
    q1.filter {
      v => q2.contains(v._4)
    }
  }*/

  selectedIonsFromMs1
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