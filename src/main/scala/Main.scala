import cats.Show
import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream
import fs2._
import fs2.data.xml
import fs2.io.file.{Files, Flags, Path}
import fs2.data.xml._
import fs2.data.xml.dom.DocumentBuilder
import fs2.data.xml.xpath.{XPath, XPathParser}
import fs2.data.xml.xpath.literals._

import java.nio.charset.Charset
import com.lucidchart.open.xtract.XmlReader

import scala.xml.{NodeSeq, XML}

object Main extends IOApp {
  def converter (path: XPath, filepath : String): Stream[IO, String] = {

    val stream: Stream[IO, Byte] = Files[IO].readAll(fs2.io.file.Path(filepath), 1024, Flags.Read)
    //.through(text.utf8.decode)
    //.through(text.lines)
    // .through(events[IO, String]())

    val iso88591charset: Charset = Charset.forName("ISO-8859-1")

    val streamTxt: Stream[IO, XmlEvent] = stream
      .through(text.utf8.decode)
      .through(text.lines)
      .through(events[IO, String]())
    //val nsResolved = streamTxt.through(namespaceResolver[IO])
    //val entityResolved = nsResolved.through(referenceResolver[IO]())



    streamTxt
      .through(xml.xpath.filter.raw(path))
      .parEvalMapUnbounded(_.map(Show[XmlEvent].show(_)).compile.foldMonoid)
      //.through(xml.xpath.filter.collect(path, scalaxb.fromXML[mzxml.Scan]))
     // .through(normalize)
    //  .map( eventXml => scalaxb.fromXML[mzxml.Scan](XML.loadString(eventXml)) )

 //   eventXmlsIo.map( eventXmls => eventXmls.map( e => scalaxb.fromXML[mzxml.Scan](XML.loadString(e))) )
  }

  def msRun(mzXMLpath : String): Stream[IO, Option[mzxml.MsRun]] =
    converter(xpath"//msRun",mzXMLpath)
      .map(eventXml => {
        XmlReader.of[mzxml.MsRun].read(XML.loadString(eventXml)).toOption
      })


  def run(args: List[String]): IO[ExitCode] =
    args.headOption match {
      case Some(mzXML) =>
        IO {
          println(mzXML)
          msRun(mzXML)
            .map {
              case Some(run) => Some(run.scan.filter(s => s.properties.num==20))
              case None => None
            }
            .compile
            .toList
            .unsafeRunSync()
        }.as(ExitCode.Success)
      case None =>
        IO(System.err.println("Usage: MyApp name")).as(ExitCode(2))
    }
}