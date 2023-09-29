import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2._
import fs2.io.file.{Files, Flags, Path}
import fr.inrae.p2m2.mzxml.{PrecursorMz, SpectrumRequest}

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