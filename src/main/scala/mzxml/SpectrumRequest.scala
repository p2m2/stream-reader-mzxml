package mzxml

import cats.effect.IO

import fs2.Stream
import fs2.data.xml.xpath.literals._
import com.lucidchart.open.xtract.XmlReader
import fs2.data.xml.xpath.{XPath, XPathParser}

import scala.xml._
case class SpectrumRequest (mzXMLpath: String) {
  def precursorMz(mz : Double, precursorIntensityMin:Double=0.0,tol : Double = 0.005): Stream[IO, Option[mzxml.Scan]] = {
    XPathParser.either("//scan") match {
      case Right(x : XPath) =>
        val e = XmlStreamRequest(mzXMLpath).requestXpath(x)
        e.map(eventXml => {
          XmlReader.of[mzxml.Scan].read(XML.loadString(eventXml)).toOption match {
            case Some(scan) if scan.precursorMz.nonEmpty =>
              if(scan
                .precursorMz
                .exists(precMz =>
                  (precMz.value > mz - tol) &&
                    (precMz.value < mz + tol) &&
                    precMz.precursorIntensity.exists(_ > precursorIntensityMin)))
                Some(scan) else None
            case _ => None
          }
        })
      case Left(s : Throwable) => Stream { println(s"${s.getMessage}") ; None}
    }

  }
}

