import cats.effect.unsafe.implicits._
import cats.effect.IO
import $file.libCandidateIons
import scala.util._
import java.io._

/* check consecutive ions according a delta ppm and a delta rt */
/* criteria => maximum intensity of M0 */
//amm diagnosticIon.sc 20181018-037.mzXML 1000 3 388.0772,450.0559,477.02645,408.98553
@main
def main(
          mzXMLFile: String,
          noiseIntensity : Double,
          ppm_precision : Double,
          listIonsStr : String // 2334.3333,233.222,406.3456,...
        ) : Unit = {

        val outputFile = mzXMLFile.split("/").last.replace(".mzXML",".diagnostics.txt")
        //Seq(388.0372)
        val listIonsOfInterest : Seq[Double] = Try(listIonsStr.split(",").map(_.toDouble)) match {
                case Success(l) => l
                case Failure(e) => System.err.println(s"${e.getMessage}"); Seq()
        }
        println(listIonsOfInterest.map(_.toString).mkString(","))
        val listIons =
                libCandidateIons
                .fillMS2FragmentIon(mzXMLFile,listIonsOfInterest,noiseIntensity=noiseIntensity,ppm_precision=ppm_precision)
                  .compile.toList.unsafeRunSync()


        val fileWriter = new FileWriter(new File(outputFile))


        fileWriter.write(s"user input:$listIonsStr\n")
        fileWriter.write(s"ions      :${listIons.length}\n")

        listIons.groupBy(x => (x.m0 * 10000) / 10000.toDouble).foreach {
                case (mz, ions) =>
                        fileWriter.write(s"ions:$mz\n")
                        fileWriter.write(s"rt:${ions.map(_.rt.toString).mkString(" ")}\n")
        }
        fileWriter.write("\n\n======\n\n")
        val p : Int = 10
        listIons
          .flatMap(_.fragments)
          .map(x => (x._1*p)/p.toDouble )
          .groupBy(x =>x)
          .foreach {
                case (mz : Double,l : Seq[Double]) => fileWriter.write(s"$mz ${l.length}\n")
          }

        println(outputFile)
        fileWriter.close()

}