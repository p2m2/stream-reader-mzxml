import $ivy.`com.github.nscala-time::nscala-time:2.32.0`,com.github.nscala_time.time.Imports._
import cats.effect.IO
import fs2.text
import fs2.io.file.{Files, Path}
import fs2.Stream
import cats.effect.unsafe.implicits._
import $file.libCandidateIons
import org.joda.time.format.PeriodFormat

/* check consecutive ions according a delta ppm and a delta rt */
/* criteria => maximum intensity of M0 */

@main
def main(
          mzXMLFile: String,
          noiseIntensity : Double,
          ppm_precision : Double   = 5,
          startTime : Double       = 0,
          endTime : Double         = Double.MaxValue
        ) : Unit = {

  val processStart:DateTime = DateTime.now()

  val outputFile : String = mzXMLFile.split("/").last.replace(".mzXML",".gluco.ions.txt")

  /* get precursor Mz to get MS2 information*/

  val listPrecMz =
    libCandidateIons.precursorMzIons(mzXMLFile, noiseIntensity,startTime, endTime).compile.toList.unsafeRunSync()

  println(s"${listPrecMz.length} precursorMz  min:${listPrecMz.minBy(_.m0).m0} max:${listPrecMz.maxBy(_.m0).m0}")

  val selectedIonsFromMs1: Stream[IO, libCandidateIons.Ion] =
    libCandidateIons.candidateIonsGeneric(mzXMLFile,noiseIntensity,ppm_precision,startTime,endTime,Some(listPrecMz))

  val sum = selectedIonsFromMs1.map(x => (x.m1-x.m0,x.m2-x.m0)).compile.toList.unsafeRunSync()
  val sum01 = sum.map(_._1)
  val mean01 = sum01.sum/sum01.length
  val std01 = sum.map(x => mean01 - x._1)
  val sum02 = sum.map(_._2)
  val mean02 = sum02.sum/sum02.length
  val std02 = sum.map(x => mean02 - x._2)

  println(s"deltaM0M1  mean: ${mean01}  std: ${std01.sum/std01.length}")
  println(s"deltaM0M2  mean: ${mean02}  std: ${std02.sum/std02.length}")

  selectedIonsFromMs1
    .filter(ion => ion.scoreDaughterIons()>0 && ion.scoreNeutralLoss() > 0)
      .map(ion => ion.toString + "\n")
      .filter(_.trim.nonEmpty)
      .through(text.utf8.encode)
      .through(Files[IO].writeAll(Path(outputFile)))
      .compile
      .drain
      .unsafeRunSync()

    val processEnd:DateTime = DateTime.now()
    val elapsed:Interval = processStart to processEnd
    
    println(elapsed.toPeriod.toString(PeriodFormat.getDefault))

    println(s"****************$outputFile***********************")
    println(" ----- HEADER ----")
    println("RET_TIME;MS_LEVEL;NUM_SCAN;MS0;INT0;MS1;INT1;MS2;INT2")


}