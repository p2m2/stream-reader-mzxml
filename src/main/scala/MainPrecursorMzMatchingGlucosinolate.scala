import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO, IOApp}
import com.github.nscala_time.time.Imports.{DateTime, Interval, richReadableInstant}
import fr.inrae.p2m2.`type`.IonEligibleGlucosinolate
import fr.inrae.p2m2.mzxml.{PrecursorMz, Spectrum, SpectrumRequest}
import fs2.io.file.{Files, Path}
import fs2.{Stream, text}
import org.joda.time.format.PeriodFormat

object MainPrecursorMzMatchingGlucosinolate extends IOApp {


  import scopt.OParser

  private case class Config(
                             mzFile: Option[String] = None,
                             startRT: Option[Double] = None,
                             endRT: Option[Double] = None,
                             minIntensity : Double   = 1000,
                             thresholdDiffIntensity : Double = 0.001, // 1 %
                           )

  private val builder = OParser.builder[Config]
  private val parser1 = {
    import builder._
    OParser.sequence(
      programName("MainDistributionIntensityIons"),
      head("MainDistributionIntensityIons", "1.0"),
      opt[Double]('s', "startRT")
        .optional()
        .action((x, c) => c.copy(startRT = Some(x)))
        .text(s"start RT"),
      opt[Double]('e', "endRT")
        .optional()
        .action((x, c) => c.copy(endRT = Some(x)))
        .text(s"start RT"),
      opt[Double]('i', "minIntensity")
        .optional()
        .action((x, c) => c.copy(minIntensity = x))
        .text(s"Minimum intensity threshold selecting the ions of interest"),
      arg[String]("<file>")
        .action((x, c) => c.copy(mzFile = Some(x))),
      help("help").text("prints this usage text"),
      note("some notes." + sys.props("line.separator")),
      checkConfig(_ => success)
    )
  }
  def run(args: List[String]): IO[ExitCode] = {
    OParser.parse(parser1, args, Config()) match {
      case Some(config) =>
        config.mzFile match {
          case Some(mzXMLFile) =>
            IO {
              val startTime: Double = config.startRT.getOrElse(0.0)
              val endTime: Double = config.endRT.getOrElse(Double.MaxValue)

              val processStart: DateTime = DateTime.now()
              val outputFile = "out.txt"

              precursorMzIonsMatchingFragment(mzXMLFile,startTime,endTime)
                .map(ion => ion.toString + "\n")
                .filter(_.trim.nonEmpty)
                .through(text.utf8.encode)
                .through(Files[IO].writeAll(Path(outputFile)))
                .compile
                .drain
                .unsafeRunSync()

              val processEnd: DateTime = DateTime.now()
              val elapsed: Interval = processStart to processEnd

              println(elapsed.toPeriod.toString(PeriodFormat.getDefault))
              println(s"out:$outputFile")

          }.as(ExitCode.Success)
          case _ =>
            IO {
              System.err.println("missing mzXML file")
            }.as(ExitCode.Error)
        }

      case _ =>
        IO {
          System.err.println("Ko")
        }.as(ExitCode.Error)
    }

  }

  def precursorMzIonsMatchingFragment(
                                       mzXMLFile: String,
                                       startTime: Double,
                                       endTime: Double,
                                       scoreMinDI: Int = 1,
                                       scoreMinNL: Int = 1
                                     )
  : Stream[IO, IonEligibleGlucosinolate] = {
    SpectrumRequest(mzXMLFile)
      .msLevel(2)
      .filter(_.isDefined)
      .map(_.get)
      .filter(_.retentionTimeInSeconds.getOrElse(0) >= startTime)
      .filter(_.retentionTimeInSeconds.getOrElse(Int.MaxValue) <= endTime)
      .map {
        (spectrum: Spectrum) => {
          spectrum.precursorMz.map((x: PrecursorMz) =>
            IonEligibleGlucosinolate(
              spectrum.retentionTimeInSeconds.getOrElse(0),
              spectrum.msLevel,
              spectrum.num,
              x.value, x.precursorIntensity.getOrElse(-1.0), 0, 0, 0, 0, spectrum.peaks.filter(_._2 > 0)
            )
          )
        }
      }
      .map((ions: Seq[IonEligibleGlucosinolate]) =>
        ions.filter(
          ion => (ion.scoreDaughterIons() >= scoreMinDI) && (ion.scoreNeutralLoss() >= scoreMinNL)
        ))
      .filter(_.nonEmpty)
  }.flatMap(Stream.emits(_))
}
