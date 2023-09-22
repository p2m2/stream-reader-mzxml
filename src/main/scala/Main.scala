import cats.Show
import fs2.Stream
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2._
import fs2.data.xml
import fs2.io.file.{Files, Flags, Path}
import fs2.data.xml._
import fs2.data.xml.xpath.{XPath, XPathParser}
import fs2.data.xml.xpath.literals._
import mzxml.ScanOrigin

import scala.xml.{NodeSeq, XML}

object Main extends IOApp.Simple {
  println("Hello, World!")
  val base= "/media/ofilangi/hdd-local/workspace/INRAE"
  val data1 = s"$base/P2M2/mzxml-glucosinolate-analyser/src/test/resources/test.mzXML"
  val data2 = s"$base/P2M2/mzxml-glucosinolate-analyser/src/test/resources/20181018-037.mzXML"
  val data3 = s"$base/P2M2/brachemdb-workflow/fragmentation/MetFrag/matchms_processing/Brassinet F 20 A 1500 uL 0.14 ug Api Neg_01_7768.mzXML"

  def converter (path: XPath): Stream[IO, String] = {
    val stream: Stream[IO, Byte] = Files[IO].readAll(fs2.io.file.Path("test.mzXML"))
    //.through(text.utf8.decode)
    //.through(text.lines)
    // .through(events[IO, String]())
    IO{ println("Hello World !!!2") }

    val streamTxt: Stream[IO, XmlEvent] = stream.through(text.utf8.decode).through(events[IO, String]())
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

  val msRun: Stream[IO, mzxml.MsRun] =
    converter(xpath"//msRun")
      .map(eventXml => scalaxb.fromXML[mzxml.MsRun](XML.loadString(eventXml)))

  val msInstrument: Stream[IO, mzxml.MsInstrument] =
    converter(xpath"//msInstrument")
      .map(eventXml => { println(eventXml);scalaxb.fromXML[mzxml.MsInstrument](XML.loadString(eventXml)) })

  val dataProcessing: Stream[IO, mzxml.DataProcessing] =
    converter(xpath"/mzXML/msRun/dataProcessing")
      .map(eventXml => scalaxb.fromXML[mzxml.DataProcessing](XML.loadString(eventXml)))

  val scans : Stream[IO, mzxml.ScanOrigin] =
    converter(xpath"//scan")
      .map( eventXml => scalaxb.fromXML[mzxml.ScanOrigin](XML.loadString(eventXml)) )



  def run: IO[Unit] =
    {
      IO {
        println(
          scans
            .filter(x => x.num == 10)
            .compile
            .toList
            .unsafeRunSync())
      }
      //scans.compile.drain
    }

}