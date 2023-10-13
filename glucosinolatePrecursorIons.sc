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
          startTime : Double       = 0,
          endTime : Double         = Double.MaxValue
        ) : Unit = {

  val processStart:DateTime = DateTime.now()

  val outputFile : String = mzXMLFile.split("/").last.replace(".mzXML",".prec.gluco.ions.txt")

  /* get precursor Mz to get MS2 information*/

  val listPrecMz =
    libCandidateIons.precursorMzIonsMatchingFragment(mzXMLFile,startTime, endTime)
  
  listPrecMz
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