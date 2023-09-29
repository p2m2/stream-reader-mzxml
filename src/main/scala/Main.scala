import cats.Show
import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream
import fs2._
import fs2.data.xml
import fs2.io.file.{Files, Flags}
import fs2.data.xml._
import fs2.data.xml.xpath.XPath
import fs2.data.xml.xpath.literals._

import java.nio.charset.Charset
import com.lucidchart.open.xtract.XmlReader
import mzxml.ScanOrigin

import scala.xml.{NodeSeq, XML}

object Main extends IOApp {


  def run(args: List[String]): IO[ExitCode] =
    args.headOption match {
      case Some(mzXML) =>
        IO {
          println("*** mzXML :"+mzXML+"*")
          /*val scanListOfInterest : Stream[IO,Seq[Scan]] = msRun(mzXML)
            .map {
              case Some(run) =>
                run.scan.filter(s => s.properties.num==20)
              case None => Seq()
            }

          val res = scanListOfInterest.compile
            .toList
            .unsafeRunSync()
          println(res)*/
        }.as(ExitCode.Success)
      case None =>
        IO(System.err.println("Usage: MyApp name")).as(ExitCode(2))
    }
}