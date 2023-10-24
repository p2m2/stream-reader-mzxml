import $cp.`target/scala-2.13/mzXML-stream-assembly-1.0.jar`
import cats.effect.{IO, IOApp}
import fs2.{Stream, text, Pipe}
import fs2.io.file.{Files, Path}
import java.nio.file.Paths

import cats.effect.unsafe.implicits._
import fr.inrae.p2m2.mzxml._


val mzXMLFile = "./src/test/resources/LTQ_Orbitrap_precision32.mzXML"
val outputFile = "precursor_288p93.txt"


val formatPrecursorMz : Pipe[IO, Option[Seq[PrecursorMz]], String] = {
inStream =>
    inStream.map {
    case Some(p) => s"Precursor ${p.head.value} with precursorIntensity ${p.head.precursorIntensity} " +
        s"and precursorScanNum ${p.head.precursorScanNum}\n"
    case _ => ""
    }
}

SpectrumRequest(mzXMLFile).precursorMz(288.93,5000).map {
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