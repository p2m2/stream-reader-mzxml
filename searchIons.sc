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
          ppm_precision : Double   = 5,
          startTime: Double = 0,
          endTime: Double = Double.MaxValue
        ) : Unit = {

        val outputFile : String = mzXMLFile.split("/").last.replace(".mzXML",".ions.txt")

        val listIons =
                libCandidateIons
                .searchIonsMS1(mzXMLFile,Seq(388.0388,388.0399,402.0912,422.0592),ppm_precision,noiseIntensity)
                  .compile.toList.unsafeRunSync()

        val listPrecMz =
                libCandidateIons.precursorMzIons(mzXMLFile,startTime,endTime,Some(listIons.)).compile.toList.unsafeRunSync()

        println(listIons.mkString("\n"))



}