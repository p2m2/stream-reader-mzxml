package mzxml

import cats.Show
import cats.effect.IO

import fs2.Stream
import fs2._
import fs2.data.xml
import fs2.io.file.{Files, Flags}
import fs2.data.xml._
import fs2.data.xml.xpath.XPath

import cats.effect.unsafe.implicits.global
case class XmlStreamRequest(filepath : String) {
  def requestXpath(path: XPath): Stream[IO, String] = {

    val stream: Stream[IO, Byte] = Files[IO].readAll(fs2.io.file.Path(filepath), 1024, Flags.Read)

    val streamTxt: Stream[IO, XmlEvent] = stream
      .through(text.utf8.decode)
      .through(text.lines)
      .through(events[IO, String]())

    streamTxt
      .through(xml.xpath.filter.raw(path))
      .parEvalMapUnbounded(_.map(Show[XmlEvent].show(_)).compile.foldMonoid)
  }
}
