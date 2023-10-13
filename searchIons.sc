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
                .fillMS2FragmentIon(mzXMLFile,Seq(388.0372),noiseIntensity=noiseIntensity,ppm_precision=ppm_precision)
                  .compile.toList.unsafeRunSync()

        println(listIons.mkString("\n"))

}