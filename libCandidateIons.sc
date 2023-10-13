import $cp.target.`scala-2.13`.`mzXML-stream-assembly-1.0.jar`
import cats.effect.IO
import fs2.{Stream,Pipe}
import cats.effect.unsafe.implicits._
import fr.inrae.p2m2.mzxml._
import fr.inrae.p2m2.mzxml.utils.ChemicalConst

val p_const = Math.pow(10,6)
def precisionTest(m0 : Double,m1 : Double, ppm_error: Double) : Boolean =
  Math.abs((m0 - m1) / m0) * p_const <= ppm_error

object Ion {
  val mzsDI = Map(
    "C6H11O8S2_275" -> 275.0,
    "C6H11O9S_259" -> 259.0,
    "C6H10O8S-_241" -> 242.0,
    "C6H9NO8S_241" -> 241.0,
    "C6H11O7S-_227" -> 227.0,
    "C6H11O5S-_195" -> 195.03,
    "C6H11O2-_153" -> 163.0,
    "C2H4O5NS-_154" -> 153.98,
    "C2H3O5S-_139" -> 138.97,
    "C2H2O4S-_136" -> 135.97,
    "HO4S2-_128" -> 128.93,
    "HSO4-_97" -> 96.95,
    "SO4-_96" -> 96.0,
    "C2H3OS-_75" -> 74.99
  )
  val mzsNL = Map(
    "thioglucose_242" -> 242.0,
    "glucosinolate_223" -> 223.0,
    "thioglucose_196" -> 196.0,
    "gluconolactone_178" -> 178.0,
    "RCNO4S2-_163" -> 163.0,
    "anhydroglucose_162" -> 162.0,
    "sulfureTrioxide_80" -> 80.0)

}

case class Ion(
                 rt : Int,
                 level : Int,
                 scan : Int,
                 m0 : Double,
                 i0 : Double,
                 m1 : Double,
                 i1:Double,
                 m2: Double,
                 i2 : Double,
                 fragments : Seq[(Double,Double)] = Seq()
          ) {

  def scoreNeutralLoss(): Int =
    Ion.mzsNL.values
      .map( m0 - _)
      .filter(_>0)
      .filter(
        mzNL =>
          fragments
            .map(_._1)
            .exists(mzF => Math.abs(mzNL-mzF)<0.2)).toSeq.length


  def scoreDaughterIons() : Int =
    Ion.mzsDI
      .values
      .filter(mzNL =>
        fragments
          .map(_._1)
          .exists(mzF => Math.abs(mzNL-mzF)<0.2)).toSeq.length


  override def toString: String = {
    val sc1 = scoreNeutralLoss()
    val sc2 = scoreDaughterIons()
    s"$rt;$level;$scan;$m0;$i0;${sc1+sc2};$sc1;$sc2"
  }
}
/**
 *
 * @param mzXMLFile data file
 * @param noiseIntensity filter lowest intensisity
 * @param ppm_precision precision
 * @param startTime filter on start time
 * @param endTime filter on end time
 * @param numberCarbonMin filter on the number min of carbon
 * @param numberCarbonMax filter on the number max of carbon
 * @param numberSulfurMin filter on the number min of sulfur
 * @param numberSulfurMax filter on the number max of sulfur
 * @param deltaMp0Mp1 filter on the distance between isotope M0 and M1
 * @param deltaMp0Mp2 filter on the distance between isotope M0 and M2
 *
 * default value are tweak for glucosinolate detection
 *
 * RT,MS_LEVEL,NUM_SCAN, M0, I0,M1, I1, M2, I2
 */
def candidateIonsGeneric(
                               mzXMLFile : String,
                               noiseIntensity: Double,
                               ppm_precision: Double,
                               startTime: Double,
                               endTime: Double,
                               listPrecMz : Option[Seq[Ion]],
                               numberCarbonMin: Double = 3,
                               numberCarbonMax: Double = 35,
                               numberSulfurMin: Double = 1.5,
                               numberSulfurMax: Double = 5,
                               deltaMp0Mp1: Double = 1.0,
                               deltaMp0Mp2: Double = 1.996
                             ) : Stream[IO, Ion]   = {

    SpectrumRequest(mzXMLFile)
      .msLevel(1)
      .filter(_.isDefined)
      .map(_.get)
      .filter(_.retentionTimeInSeconds.getOrElse(0) >= startTime)
      .filter(_.retentionTimeInSeconds.getOrElse(Int.MaxValue) <= endTime)
      .map {
        (spectrum: Spectrum) => {
          spectrum.peaks
            .filter {
              case (_, int) => int > noiseIntensity
            }
            .flatMap {
              case (mz,i) => listPrecMz match {
                case Some(listP) =>
                  Some(mz,i,listP.filter(precIon =>
                    spectrum.retentionTimeInSeconds.forall( x=> x >= precIon.rt-2 && x <= precIon.rt+2 )
                      && (Math.abs(precIon.m0 - mz) < 0.01)))
                case _ => None
              }
            }
            .filter {
              case (_, _, l) => l.nonEmpty
            }
            .map {
              case (mz, i, l) => (mz,i,l.head.fragments)
            }
            .map {
              case (mz0, int0,fragments) =>
                val d0: Double = mz0 + deltaMp0Mp1
                val d1: Double = mz0 + deltaMp0Mp2
                val (mz1, int1) = spectrum.findClosestValueMz(d0)
                val (mz2, int2) = spectrum.findClosestValueMz(d1)

                val ppm_error_0 = Math.abs((mz1 - d0) / d0) * p_const
                val ppm_error_1 = Math.abs((mz2 - d1) / d1) * p_const

                // print(ppm_error_0,ppm_error_1,"\n")

                (ppm_error_0 < ppm_precision, ppm_error_1 < ppm_precision, (mz0, int0), (mz1, int1), (mz2, int2),fragments) // isotopes
            }
            .filter {
              case (ppm0, ppm1, _, _, _,_) => ppm0 && ppm1
            }
            .map {
              case (_, _, v0, v1, v2,f) => (v0, v1, v2,f)
            }
            .filter {
              case (v0, v1, _,_) =>
                v1._2 >= v0._2 *
                  (ChemicalConst.abundanceIsotope("C")(1) * numberCarbonMin +
                    ChemicalConst.abundanceIsotope("S")(1) * numberSulfurMin)

            }.filter {
              case (v0, v1, _,_) =>
                v1._2 < v0._2 *
                  (ChemicalConst.abundanceIsotope("C")(1) * numberCarbonMax +
                    ChemicalConst.abundanceIsotope("S")(1) * numberSulfurMax)

            } /* criteria M2 of Isotope S are present 4.4 % */
            .filter {
              case (v0, _, v2,_) =>
                v2._2 >= v0._2 * ChemicalConst.abundanceIsotope("S")(2) * numberSulfurMin
            }
            .filter {
              case (v0, _, v2,_) =>
                v2._2 < v0._2 * ChemicalConst.abundanceIsotope("S")(2) * numberSulfurMax
            }
            .map {
              case (v0, v1, v2,fragments) => (
                Ion(
                spectrum.retentionTimeInSeconds.getOrElse(-1),
                spectrum.msLevel,
                spectrum.num,
                v0._1, v0._2,
                v1._1, v1._2,
                v2._1, v2._2,fragments)
              )
            }
        }
      }.flatMap(Stream.emits(_))
}

def precursorMzIons(
                      mzXMLFile: String,
                      noiseIntensity : Double,
                      startTime: Double,
                      endTime: Double
                   )
: Stream[IO, Ion]  = {
  SpectrumRequest(mzXMLFile)
    .msLevel(2)
    .filter(_.isDefined)
    .map(_.get)
    .filter(_.retentionTimeInSeconds.getOrElse(0) >= startTime)
    .filter(_.retentionTimeInSeconds.getOrElse(Int.MaxValue) <= endTime)
    .map {
      (spectrum: Spectrum) => {
        spectrum.precursorMz.map( (x : PrecursorMz) =>
          Ion(
            spectrum.retentionTimeInSeconds.getOrElse(0),
            spectrum.msLevel,
            spectrum.num,
            x.value,x.precursorIntensity.getOrElse(-1.0),0,0,0,0,spectrum.peaks.filter(_._2<noiseIntensity)
          )
        )
      }
    }
}.flatMap(Stream.emits(_))

def fillMS2FragmentIon(
                     mzXMLFile: String,
                     listIons : Seq[Double],
                     noiseIntensity : Double,
                     ppm_precision: Double
                   )
: Stream[IO, Ion]  = {
      SpectrumRequest(mzXMLFile)
        .msLevel(2)
        .filter(_.isDefined)
        .filter(_.isDefined)
        .map(_.get)
        .map {
          (spectrum: Spectrum) => {
            spectrum.precursorMz.flatMap((x: PrecursorMz) => x match {
              case _ if listIons.exists(y => precisionTest(y, x.value, ppm_precision)) => Some(Ion(
                spectrum.retentionTimeInSeconds.getOrElse(0),
                spectrum.msLevel,
                spectrum.num,
                x.value, x.precursorIntensity.getOrElse(-1.0), 0, 0, 0, 0, spectrum.peaks
              ))
              case _ => None
            })
          }
        }
}.flatMap(Stream.emits(_))

def searchIonsMS1(
                  mzXMLFile: String,
                  mzs : Seq[Double],
                  ppm_precision: Double,
                  noiseIntensity: Double,
                ): Stream[IO, Ion]  = {
  SpectrumRequest(mzXMLFile)
    .msLevel(1)
    .filter(_.isDefined)
    .map(_.get)
    .map {
      (spectrum: Spectrum) => {
        spectrum.peaks
          .filter {
            case (_, int) => int > noiseIntensity
          }
          .filter {
            case (mzP, _) => mzs.exists(precisionTest(_, mzP, ppm_precision))
          }
          .map {
            case (m, i) =>
              Ion(
                spectrum.retentionTimeInSeconds.getOrElse(-1),
                spectrum.msLevel,
                spectrum.num,
                m, i,
                0, 0,
                0, 0,Seq())
          }
      }
    }
}.flatMap(Stream.emits(_))
