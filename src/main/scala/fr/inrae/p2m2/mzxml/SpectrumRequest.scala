package fr.inrae.p2m2.mzxml

import cats.effect.IO

import fs2.Stream
import com.lucidchart.open.xtract.XmlReader
import fs2.data.xml.xpath.{XPath, XPathParser}

import scala.xml._

// doc FS2 / xpath => https://fs2-data.gnieh.org/documentation/xml/xpath/

case class SpectrumRequest (mzXMLpath: String) {

  def msLevel(num : Int) : Stream[IO, Option[Spectrum]] = {
    XPathParser.either(s"""//scan[@msLevel == "$num"]""") match {
      case Right(x: XPath) =>
        val e = XmlStreamRequest(mzXMLpath).requestXpath(x)
        e.map(eventXml => {
          XmlReader.of[ScanOrigin].read(XML.loadString(eventXml)).toOption match {
            case Some(scan) => Some(scan)
            case _ => None
          }
        })
      case Left(s : Throwable) => Stream { println(s"${s.getMessage}") ; None }
    }
  }


  /**
   *
   * @param mz m/z  mass divided by charge number to match
   * @param precursorIntensityMin minimum intensity of the precursor to be recovered
   * @param tolMz tolerance for mz
   * @return
   */
  def precursorMz(mz : Double, precursorIntensityMin:Double=0.0,tolMz : Double = 0.005): Stream[IO, Option[Spectrum]] = {
    XPathParser.either("//scan") match {
      case Right(x : XPath) =>
        val e = XmlStreamRequest(mzXMLpath).requestXpath(x)
        e.map(eventXml => {
          XmlReader.of[ScanOrigin].read(XML.loadString(eventXml)).toOption match {
            case Some(scan) if scan.precursorMz.nonEmpty =>
              if(scan
                .precursorMz
                .exists(precMz =>
                  (precMz.value > mz - tolMz) &&
                    (precMz.value < mz + tolMz) &&
                    precMz.precursorIntensity.exists(_ > precursorIntensityMin)))
                Some(scan) else None
            case _ => None
          }
        })
      case Left(s : Throwable) => Stream { println(s"${s.getMessage}") ; None}
    }

  }


}

