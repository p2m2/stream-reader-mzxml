import cats.effect.{ExitCode, IO, IOApp}
import com.github.nscala_time.time.Imports.{DateTime, Interval}
import fr.inrae.p2m2.mzxml.{Spectrum, SpectrumRequest}
import fs2.{Stream, text}
import fs2.io.file.{Files, Path}
import fr.inrae.p2m2.mzxml.utils.ChemicalConst
import org.joda.time.format.PeriodFormat
import scopt.OParser
import scala.math._
import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global


object MainGlucosinolates extends IOApp {

  private case class Config(
    mzFile: Option[String] = None,
    outputFile: Option[String] = None,
    startRT: Option[Double] = None,
    endRT: Option[Double] = None,
    minIntensity: Double = 7500,
    deltaMp0Mp2: Double = 1.9958, // Glucosinolate
    numberCarbonMin: Int = 3,
    numberCarbonMax: Int = 35,
    numberSulfurMin: Double = 1.5,
    numberSulfurMax: Double = 5,
    precisionMz: Double = 0.0001
  )

  private val builder = OParser.builder[Config]
  private val parser1 = {
    import builder._
    OParser.sequence(
      programName("MainGlucosinolates"),
      head("MainGlucosinolates", "1.0"),
      opt[String]('o', "output")
        .required()
        .action((x, c) => c.copy(outputFile = Some(x)))
        .text("Output file"),
      opt[Double]('s', "startRT")
        .optional()
        .action((x, c) => c.copy(startRT = Some(x)))
        .text("start RT"),
      opt[Double]('e', "endRT")
        .optional()
        .action((x, c) => c.copy(endRT = Some(x)))
        .text("end RT"),
      opt[Double]('i', "minIntensity")
        .optional()
        .action((x, c) => c.copy(minIntensity = x))
        .text("Minimum intensity threshold selecting the ions of interest"),
      arg[String]("<file>")
        .required()
        .action((x, c) => c.copy(mzFile = Some(x)))
        .text("Input mzXML file"),
      help("help").text("prints this usage text")
    )
  }

  def run(args: List[String]): IO[ExitCode] = {
    OParser.parse(parser1, args, Config()) match {
      case Some(config) =>
        processFile(config).as(ExitCode.Success)
      case _ =>
        IO.raiseError(new IllegalArgumentException("Failed to parse arguments")).as(ExitCode.Error)
    }
  }
private def processFile(config: Config): IO[Unit] = {
  for {
    mzXMLFile <- IO.fromOption(config.mzFile)(new IllegalArgumentException("Missing mzXML file"))
    startTime = config.startRT.getOrElse(0.0)
    endTime = config.endRT.getOrElse(Double.MaxValue)
    processStart <- IO.realTime.map(rt => DateTime.now().plus(rt.toMillis))
    _ <- IO.println(s"Start analyze: $mzXMLFile")
    
    // Progress tracking
    progressRef <- Ref[IO].of(0)
    totalSpectraRef <- Ref[IO].of(0)
    
    // First, count total spectra
    totalSpectra <- SpectrumRequest(mzXMLFile)
      .msLevel(1)
      .filter(_.isDefined)
      .map(_.get)
      .filter(s => s.retentionTimeInSeconds.exists(rt => rt >= startTime && rt <= endTime))
      .compile
      .count

    _ <- totalSpectraRef.set(totalSpectra.toInt)

    results <- SpectrumRequest(mzXMLFile)
      .msLevel(1)
      .filter(_.isDefined)
      .map(_.get)
      .filter(s => s.retentionTimeInSeconds.exists(rt => rt >= startTime && rt <= endTime))
      .evalMap { spectrum => 
        for {
          _ <- progressRef.update(_ + 1)
          current <- progressRef.get
          total <- totalSpectraRef.get
          _ <- IO.println(s"Progress: ${(current.toDouble / total * 100).round}% ($current/$total)")
          result <- IO.blocking(processSpectrum(config, spectrum))
        } yield result
      }  
      .compile
      .toList

    _ <- IO.println(s"Processing completed. Number of results: ${results.length}")
    sortedResults = results.flatten.sortBy { case (_, ((mz0, _), _, _)) => -mz0.toDouble }
    
    _ <- IO.println(s"Sorting completed. Number of results: ${sortedResults.size}")
    
    _ <- Stream.emits(sortedResults)
      .evalMap(result => IO.println(result.toString))
      .compile
      .drain

    processEnd <- IO.realTime.map(rt => DateTime.now().plus(rt.toMillis))
    duration = new Interval(processStart, processEnd).toPeriod
    _ <- IO.println(s"Duration: ${PeriodFormat.getDefault.print(duration)}")
    
  } yield ()
}


private def processSpectrum(config: Config, spectrum: Spectrum): Seq[(Double, ((Double, Double), (Double, Double), (Double, Double)))] = {
    spectrum.peaks
      .filter { case (_, int0) => int0 > config.minIntensity }
      .flatMap { case (mz0, int0) =>
        val mz_ms_p2 = mz0 + config.deltaMp0Mp2
        val (mz1, int1) = spectrum.findClosestValueMz(mz0 + 1.0)
        val (mz2, int2) = spectrum.findClosestValueMz(mz_ms_p2)
        
        val isotopes = ((mz0, int0), (mz1, int1), (mz2, int2))
        
        if (abs(mz2 - mz0 - config.deltaMp0Mp2) < config.precisionMz && isValidIsotope(config, isotopes)) {
          spectrum.retentionTimeInSeconds.map(rt => (rt.toDouble, isotopes))
        } else {
          None
        }
      }
  }

  private def isValidIsotope(config: Config, isotopes: ((Double, Double), (Double, Double), (Double, Double))): Boolean = {
    val ((mz0, int0), (_, int1), (_, int2)) = isotopes
    
    val carbonCriteria = int1 >= int0 * (ChemicalConst.abundanceIsotope("C")(1) * config.numberCarbonMin) &&
                         int1 < int0 * (ChemicalConst.abundanceIsotope("C")(1) * config.numberCarbonMax)
    
    val sulfurCriteria = int2 >= int0 * ChemicalConst.abundanceIsotope("S")(2) * config.numberSulfurMin &&
                         int2 < int0 * ChemicalConst.abundanceIsotope("S")(2) * config.numberSulfurMax
    
    carbonCriteria && sulfurCriteria
  }
}
