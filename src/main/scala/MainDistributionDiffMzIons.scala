import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO, IOApp}
import com.github.nscala_time.time.Imports.{DateTime, Interval, richReadableInstant}
import fr.inrae.p2m2.mzxml.{Spectrum, SpectrumRequest}
import org.joda.time.format.PeriodFormat
import scala.collection.parallel.CollectionConverters._

object MainDistributionDiffMzIons extends IOApp {

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
  val fixCom: Int = 100000
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
                      .toList.par
                      .flatMap {
                        case (mz, intensity) => if (intensity > config.minIntensity) {
                          val diffMat: Seq[Double] =
                            spectrum.peaks
                              .filter { case (_, i) => i > 0.0 }
                              .filter { case (_, i) => (intensity / i) > config.thresholdDiffIntensity }
                              .map(mz - _._1) // calcul du diff
                              .map(x => (x * fixCom).toInt / fixCom.toDouble)
                          Some(diffMat)
                        } else {
                          None
                        }
                      }
                      .foldLeft(Map[Double, Int]()) {
                        (countAccumulator, mzs) =>
                          mzs
                            .map(mz => {
                              mz -> (countAccumulator.getOrElse(mz, 0) + 1)
                            }).toMap
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
              println("*************** Gives the occurrences of the difference between the Mz (mass on charge) of " +
                "interest and the other ions in the same mass spectrum to detect the formation of adducts. " +
                "*********************")
              println
              listMat
                .filter(_._2 > 2)
                .groupBy(_._2)
                .toList
                .sortBy(_._1)
                .reverse
                .map { case (count: Int, mzs: Map[Double, Int]) => (count, mzs.keys.toSeq) }
                .take(40)
                .foreach { case (count: Int, mzs: Seq[Double]) =>
                  println(s"Occurence:$count ionsList :${mzs.mkString(",")}")
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
