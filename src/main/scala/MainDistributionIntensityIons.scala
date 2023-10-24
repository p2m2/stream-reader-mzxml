import cats.effect.{ExitCode, IO, IOApp}
import com.github.nscala_time.time.Imports.{DateTime, Interval, richReadableInstant}
import cats.effect.unsafe.implicits.global
import fr.inrae.p2m2.mzxml.{Spectrum, SpectrumRequest}
import org.joda.time.format.PeriodFormat

object MainDistributionIntensityIons extends IOApp {

  import scopt.OParser

  private case class Config(
                             mzFile: Option[String] = None,
                             startRT: Option[Double] = None,
                             endRT: Option[Double] = None
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
      arg[String]("<file>")
        .action((x, c) => c.copy(mzFile = Some(x))),
      help("help").text("prints this usage text"),
      note("some notes." + sys.props("line.separator")),
      checkConfig(_ => success)
    )
  }

  //private val maxvalue : Int = 10000000
  //0.to(maxvalue,(maxvalue/40)).map(_.toDouble)

  private val listThresholdValues: Seq[Double] =
      Seq(
      0, 500, 1000, 2000, 3500, 5000, 7500, 10000,
      15000, 20000, 30000, 50000, 100000, 150000,
      300000, 500000, 750000, 1000000, 1500000,
      2000000,3000000,4000000,5000000,6000000,7000000,8000000,9000000,
      10000000,15000000,20000000,30000000,40000000
    ).map(_.toDouble)

  def run(args: List[String]): IO[ExitCode] = {
    OParser.parse(parser1, args, Config()) match {
      case Some(config) =>
        config.mzFile match {
          case Some(mzXMLFile) =>
            IO {
              val startTime: Double = config.startRT.getOrElse(0.0)
              val endTime: Double = config.endRT.getOrElse(Double.MaxValue)

              val processStart: DateTime = DateTime.now()

              val AlllListMat: List[Map[Double, Int]] =
                SpectrumRequest(mzXMLFile)
                  .msLevel(1)
                  .filter(_.isDefined)
                  .map(_.get)
                  .filter(_.retentionTimeInSeconds.getOrElse(0) >= startTime)
                  .filter(_.retentionTimeInSeconds.getOrElse(Int.MaxValue) <= endTime)
                  .map {
                    (spectrum: Spectrum) => {
                      spectrum.peaks
                        .foldLeft(Map[Double, Int]()) {
                          case (countAccumulator, (_, intensity)) =>
                            countAccumulator ++ listThresholdValues
                              .filter(_ <= intensity).map(m => m -> (countAccumulator.getOrElse(m, 0) + 1))
                        }
                    }
                  }.compile.toList.unsafeRunSync()

              val processEndInter: DateTime = DateTime.now()
              val elapsedInter: Interval = processStart to processEndInter
              println("*************** Duration *********************")
              println
              println(s" collect          :: ${elapsedInter.toPeriod.toString(PeriodFormat.getDefault)}")

              val listMat: Map[Double, Int] = AlllListMat.fold(Map.empty[Double, Int]) {
                (count: Map[Double, Int], mzsLocalCount: Map[Double, Int]) =>
                  count ++ mzsLocalCount.map { case (mz, c) => mz -> (count.getOrElse(mz, 0) + c) }
              }

              val processEnd: DateTime = DateTime.now()
              val elapsed: Interval = processStart to processEnd
              println(s" collect/fold     :: ${elapsed.toPeriod.toString(PeriodFormat.getDefault)}")
              println
              println("*************** Number of ions by intensity thresholds *********************")
              println
              listMat
                .toList
                .sortBy(_._2)
                .take(listThresholdValues.length)
                .filter ( obj => obj._2>0 )
                .foreach { case (intensity : Double, count: Int) =>
                  println(f"$count% 12d ions ions above intensity threshold ${intensity}% 15f ")
                }
              println
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
}
