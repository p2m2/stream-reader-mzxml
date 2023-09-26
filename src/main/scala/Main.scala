import cats.Show

import cats.effect.{IO, IOApp}
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

object Main extends IOApp.Simple {

  val base= "/media/ofilangi/hdd-local/workspace/INRAE"
  val data1 = s"$base/P2M2/mzxml-glucosinolate-analyser/src/test/resources/test.mzXML"
  val data2 = s"$base/P2M2/mzxml-glucosinolate-analyser/src/test/resources/20181018-037.mzXML"
  val data3 = s"$base/P2M2/brachemdb-workflow/fragmentation/MetFrag/matchms_processing/Brassinet F 20 A 1500 uL 0.14 ug Api Neg_01_7768.mzXML"

  def converter (path: XPath): Stream[IO, String] = {

    val stream: Stream[IO, Byte] = Files[IO].readAll(fs2.io.file.Path("example.mzXML"), 1024, Flags.Read)
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
/*
  val msRun: Stream[IO, mzxml.MsRun] =
    converter(xpath"//msRun")
      .map(eventXml => scalaxb.fromXML[mzxml.MsRun](XML.loadString(eventXml)))*/

  def ontologyTable(tag : String): Stream[IO, Option[mzxml.Software]] =
    converter(xpath"//software")
      .map(eventXml => {
        println(eventXml)
        XmlReader.of[mzxml.Software].read(XML.loadString(eventXml)).toOption
      })

  val msInstrument: Stream[IO, Option[mzxml.MsInstrument]] =
    converter(xpath"//msInstrument")
      .map(eventXml => {
        println(eventXml)
        XmlReader.of[mzxml.MsInstrument].read(XML.loadString(eventXml)).toOption
      })

  val dataProcessing: Stream[IO, Option[mzxml.DataProcessing]] =
    converter(xpath"//dataProcessing")
      .map(eventXml => XmlReader.of[mzxml.DataProcessing].read(XML.loadString(eventXml)).toOption)
/*
  val scans : Stream[IO, mzxml.Scan] =
    converter(xpath"//scan")
      .map( eventXml => scalaxb.fromXML[mzxml.Scan](XML.loadString(eventXml)) )*/

  def run: IO[Unit] =
    {
      IO {
        println(
          dataProcessing
           // .filter(x => x.num == 10)
            .compile
            .toList
            .unsafeRunSync())
      }
      //scans.compile.drain
    }

}