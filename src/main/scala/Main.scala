import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.unsafe.implicits.global
import fr.inrae.p2m2.mzxml.utils.ChemicalConst
import fs2._
import fs2.io.file.{Files, Flags, Path}
import fr.inrae.p2m2.mzxml.{PrecursorMz, Spectrum, SpectrumRequest}

import scala.xml.{NodeSeq, XML}

object Main extends IOApp {

  val formatPrecursorMz : Pipe[IO, Option[Seq[PrecursorMz]], String] = {
    inStream =>
      inStream.map {
        case Some(p) => s"Precursor ${p.head.value} with precursorIntensity ${p.head.precursorIntensity} " +
          s"and precursorScanNum ${p.head.precursorScanNum}\n"
        case _ => ""
      }
  }

  def matchIsotopeGlucosinolate(mzXMLFile : String,startTime : Int=0, endTime : Int=100000,outputFile : String): Unit = {
    val deltaMp0Mp2     = 1.996 // Glucosinolate
    val numberCarbonMin = 3
    val numberCarbonMax = 35
    val numberSulfurMin = 1.5
    val numberSulfurMax = 5

    IO {
      SpectrumRequest(mzXMLFile)
        .msLevel(1)
        .filter(_.isDefined)
        .map( _.get)
        .filter( _.retentionTimeInSeconds.getOrElse(0)>=startTime)
        .filter( _.retentionTimeInSeconds.getOrElse(Int.MaxValue)<=endTime)
        .map {
          (spectrum: Spectrum) => {
            spectrum.peaks.map {
                case (mz0, int0) =>
                  val mz_ms_p2 = mz0 + deltaMp0Mp2
                  val (mz1, int1) = spectrum.findClosestValueMz(mz0 + 1.0)
                  val (mz2, int2) = spectrum.findClosestValueMz(mz_ms_p2)
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
        .map(x => x.map(y => y.toString()+"\n").mkString("\n"))
        .through(text.utf8.encode)
        .through(Files[IO].writeAll(Path(outputFile)))
        .compile
        .drain
        .unsafeRunSync()


    }.as(ExitCode.Success)
  }

  def run(args: List[String]): IO[ExitCode] = {
    val mzXMLFile = {
      args.headOption match {
        case Some(mzXML) => mzXML
        case None =>
          "./src/test/resources/LTQ_Orbitrap_precision32.mzXML"
      }
    }
    val outputFile = "precursor_288p93.txt"

    IO {
      SpectrumRequest(mzXMLFile).precursorMz(288.93, 9000).map {
          case Some(r) => Some(r.precursorMz)
          case None => None
        }
        .filter(_.isDefined)
        .through(formatPrecursorMz)
        .through(text.utf8.encode)
        .through(Files[IO].writeAll(Path(outputFile)))
        .compile
        .drain
        .unsafeRunSync()

      println(outputFile)

    }.as(ExitCode.Success)
  }
}