package mzxml

import scala.language.implicitConversions

/**
 * interface class for users. This class does not integrate the recursion of the "scan" tag defined in the XML format.
 *
 * @param num
 * @param msLevel
 * @param peaksCount
 */
case class Spectrum(
                     num: Int,
                     msLevel: Int,
                     peaksCount: Int,
                     polarity: Option[Polarity.Value] = None,
                     scanType: Option[ScanType.Value] = None,
                     filterLine: Option[String] = None,
                     centroided: Option[Boolean] = None,
                     deisotoped: Option[Boolean] = None,
                     chargeDeconvoluted: Option[Boolean] = None,
                     retentionTimeInSeconds: Option[Int] = None, // in seconds
                     ionisationEnergy: Option[Double] = None,
                     collisionEnergy: Option[Double] = None,
                     cidGasPressure: Option[Double] = None,
                     startMz: Option[Double] = None,
                     endMz: Option[Double] = None,
                     lowMz: Option[Double] = None,
                     highMz: Option[Double] = None,
                     basePeakMz: Option[Double] = None,
                     basePeakIntensity: Option[Double] = None,
                     totIonCurrent: Option[Double] = None,
                     msInstrumentID: Option[Int] = None,
                     compensationVoltage: Option[Double] = None,
                     precursorMz: Seq[mzxml.PrecursorMz] = Seq(),
                     peaks: Seq[(Double,Double)] = Seq()
                   )  {
}

object Spectrum {
  implicit def scanOrigin(scan : ScanOrigin): Spectrum = {
    Spectrum.apply(
      scan.properties.num,
      scan.properties.msLevel,
      scan.properties.peaksCount,
      scan.properties.polarity,
      scan.properties.scanType,
      scan.properties.filterLine,
      scan.properties.centroided,
      scan.properties.deisotoped,
      scan.properties.chargeDeconvoluted,
      scan.properties.retentionTimeInSeconds, // in seconds
      scan.properties.ionisationEnergy,
      scan.properties.collisionEnergy ,
      scan.properties.cidGasPressure,
      scan.properties.startMz,
      scan.properties.endMz,
      scan.properties.lowMz,
      scan.properties.highMz,
      scan.properties.basePeakMz,
      scan.properties.basePeakIntensity,
      scan.properties.totIonCurrent,
      scan.properties.msInstrumentID,
      scan.properties.compensationVoltage,
      scan.precursorMz,
      scan.peaks.head.mzsIntensitiesPair // suppose that a scan have only on peak tag ...
    )
  }

  implicit def subScan(scan: SubScan): Spectrum = {

    Spectrum.apply(
      scan.properties.num,
      scan.properties.msLevel,
      scan.properties.peaksCount,
      scan.properties.polarity,
      scan.properties.scanType,
      scan.properties.filterLine,
      scan.properties.centroided,
      scan.properties.deisotoped,
      scan.properties.chargeDeconvoluted,
      scan.properties.retentionTimeInSeconds, // in seconds
      scan.properties.ionisationEnergy,
      scan.properties.collisionEnergy,
      scan.properties.cidGasPressure,
      scan.properties.startMz,
      scan.properties.endMz,
      scan.properties.lowMz,
      scan.properties.highMz,
      scan.properties.basePeakMz,
      scan.properties.basePeakIntensity,
      scan.properties.totIonCurrent,
      scan.properties.msInstrumentID,
      scan.properties.compensationVoltage,
      scan.precursorMz,
      scan.peaks.head.mzsIntensitiesPair
    )
  }
}