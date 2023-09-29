package fr.inrae.p2m2.mzxml

import com.lucidchart.open.xtract.{XmlReader, __}
import com.lucidchart.open.xtract.XmlReader._
import cats.syntax.all._
import com.github.marklister.base64.Base64._
import fr.inrae.p2m2.mzxml

import java.nio._
import javax.xml.datatype.DatatypeFactory

case class MzXML(
                  msRun : MsRun,
                  index : Seq[mzxml.IndexScan] = Nil,
                  indexOffset : Option[mzxml.IndexOffset],
                  sha1: Option[String] = None
                )

object MzXML {
  implicit val reader: XmlReader[MzXML] = (
    (__ \ "msRun").read[MsRun],
    (__ \ "index").read(seq[mzxml.IndexScan]),
    (__ \ "indexOffset").read[mzxml.IndexOffset].optional,
    attribute[String]("sha1").optional
  ).mapN(apply)
}
case class MsRun(
                  scanCount : Option[Int],
                  startTimeInSeconds : Option[Int],
                  endTimeInSeconds : Option[Int],
                  parentFile: Seq[ParentFile] = Nil,
                  msInstrument: Seq[MsInstrument] = Nil,
                  dataProcessing: Seq[DataProcessing] = Nil,
                  //    separation: Option[mzxml.Separation] = None,
                  //    spotting: Option[mzxml.Spotting] = None,
                  scan: Seq[mzxml.ScanOrigin] = Nil,
                  sha1: Option[String] = None,
           )

object MsRun {
  implicit val reader: XmlReader[MsRun] = (
    attribute[String]("scanCount").map(_.toInt).optional,
    attribute[String]("startTime").map(DatatypeFactory.newInstance().newDuration).map(_.getSeconds).optional,
    attribute[String]("endTime").map(DatatypeFactory.newInstance().newDuration).map(_.getSeconds).optional,
    (__ \ "parentFile").read(seq[ParentFile]),
    (__ \ "msInstrument")read(seq[MsInstrument]),
    (__ \ "dataProcessing")read(seq[DataProcessing]),
    (__ \ "scan")read(seq[mzxml.ScanOrigin]),
    attribute[String]("sha1").optional
  ).mapN(apply)
}

object FileType extends Enumeration {
  val unknown: FileType.Value = Value("unknown")
  val RAWData: FileType.Value = Value("RAWData")
  val ProcessedData : FileType.Value = Value("processedData")
}

case class ParentFile(
                       fileName : String,
                       fileType : FileType.Value,
                       fileSha1 : String
                     )

object ParentFile {
  implicit val reader: XmlReader[ParentFile] = (
    attribute[String]("fileName"),
    attribute("fileType")(enum(FileType)).default(FileType.unknown),
    attribute[String]("fileSha1")
  ).mapN(apply)
}

case class OntologyEntryTypable(category : String,value : String)

object OntologyEntryTypable {
  implicit val reader: XmlReader[OntologyEntryTypable] = (
    attribute[String]("category"),
    attribute[String]("value")
  ).mapN(apply)
}

case class Software(`type` : String,name : String,version : String, completionTime : Option[String])

object Software {
  implicit val reader: XmlReader[Software] = (
    attribute[String]("type"),
    attribute[String]("name"),
    attribute[String]("version"),
    attribute[String]("completionTime").optional //should be a date
  ).mapN(apply)
}


case class Operator(first : String,last : String,phone : Option[String],email : Option[String] = None,URI : java.net.URI)

object Operator {

  private def validateEmail(email: String): Boolean = email contains "@"

  implicit val reader: XmlReader[Operator] = (
    attribute[String]("first"),
    attribute[String]("last"),
    attribute[String]("phone").optional,
    attribute[String]("email").filter(validateEmail _).optional,
    attribute[String]("URI").map(java.net.URI.create)
  ).mapN(apply)
}


case class MsInstrumentSequence(nameValue: String,comment: Option[String] = None)

object MsInstrumentSequence {
  implicit val reader: XmlReader[MsInstrumentSequence] = (
    attribute[String]("nameValue"),
    attribute[String]("comment").optional,
  ).mapN(apply)
}

case class MsInstrument(
                         msInstrumentID : Option[String],
                         msManufacturer: OntologyEntryTypable,
                         msModel: OntologyEntryTypable,
                         msIonisation: OntologyEntryTypable,
                         msMassAnalyzer: OntologyEntryTypable,
                         msDetector: OntologyEntryTypable,
                         software: Software,
                         msResolution: Option[OntologyEntryTypable] = None,
                         operator: Option[Operator] = None,
                         msinstrumentsequence1: Seq[MsInstrumentSequence] = Nil)

object MsInstrument {

  implicit val reader: XmlReader[MsInstrument] = {
   (
    attribute[String]("msInstrumentID").optional,
    (__ \ "msManufacturer").read[OntologyEntryTypable],
    (__ \ "msModel").read[OntologyEntryTypable],
    (__ \ "msIonisation").read[OntologyEntryTypable],
   (__ \ "msMassAnalyzer").read[OntologyEntryTypable],
   (__ \ "msDetector").read[OntologyEntryTypable],
   (__ \ "software").read[Software],
   (__ \ "msResolution").read[OntologyEntryTypable].optional,
    (__ \ "operator").read[Operator].optional,
   (__ \ "msinstrumentsequence").read(seq[MsInstrumentSequence])
  ).mapN(apply)
  }
}


case class DataProcessing(
                           intensityCutoff : Option[Double],
                           centroided : Option[Boolean],
                           deisotoped : Option[Boolean],
                           chargeDeconvoluted : Option[Boolean],
                           spotIntegration : Option[Boolean],
                           software: Software,
                           dataprocessingsequence1: Seq[DataProcessingSequence] = Nil
                         )
object DataProcessing {
  implicit val reader: XmlReader[DataProcessing] = (
    attribute[Double]("intensityCutoff").optional,
    attribute[Boolean]("centroided").optional,
    attribute[Boolean]("deisotoped").optional,
    attribute[Boolean]("chargeDeconvoluted").optional,
    attribute[Boolean]("spotIntegration").optional,
    (__ \ "software").read[Software],
    (__ \ "dataprocessingsequence").read(seq[DataProcessingSequence])
  ).mapN(apply)
}
case class DataProcessingSequence(
                                    processingOperation: String,
                                    comment: Option[String] = None)

object DataProcessingSequence {
  implicit val reader: XmlReader[DataProcessingSequence] = (
    attribute[String]("processingOperation"),
    attribute[String]("comment").optional,
  ).mapN(apply)
}

object ActivationMethod extends Enumeration {
  val ETD : ActivationMethod.Value = Value("ETD")
  val ECD : ActivationMethod.Value = Value("ECD")
  val CID : ActivationMethod.Value = Value("CID")
  val HCD : ActivationMethod.Value = Value("HCD")
  val ETDu43SA : ActivationMethod.Value = Value("ETDu43SA")
}

case class PrecursorMz(
                        value: Double,
                        precursorScanNum : Option[BigInt],
                        precursorIntensity : Option[Double],
                        precursorCharge : Option[BigInt],
                        possibleCharges : Option[String],
                        windowWideness : Option[Double],
                        activationMethod : Option[ActivationMethod.Value]
                      )
object PrecursorMz {
  implicit val reader: XmlReader[PrecursorMz] = (
    ( __ ).read[Double],
    attribute[String]("precursorScanNum").map(BigInt(_)).optional,
    attribute[Double]("precursorIntensity").optional,
    attribute[String]("precursorCharge").map(BigInt(_)).optional,
    attribute[String]("possibleCharges").optional,
    attribute[Double]("windowWideness").optional,
    attribute("activationMethod")(enum(ActivationMethod)).optional,
  ).mapN(apply)
}

object ScanType extends Enumeration {
  val Full : ScanType.Value  = Value("Full")
  val Zoom : ScanType.Value  = Value("Zoom")
  val SIM : ScanType.Value  = Value("SIM")
  val SRM : ScanType.Value  = Value("SRM")
  val CRM : ScanType.Value  = Value("CRM")
  val Q1 : ScanType.Value  = Value("Q1")
  val Q3 : ScanType.Value  = Value("Q3")
}


case class Maldi(
                  plateID : String,
                  spotID : String,
                  laserShootCount : BigInt,
                  laserFrequency : String,
                  laserIntensity : BigInt,
                  collisionGas : Boolean
                )

object Maldi  {
  implicit val reader: XmlReader[Maldi] = (
    attribute[String]("plateID"),
    attribute[String]("spotID"),
    attribute[String]("laserShootCount").map(BigInt(_)),
    attribute[String]("laserFrequency"),
    attribute[String]("laserIntensity").map(BigInt(_)),
    attribute[Boolean]("collisionGas")
  ).mapN(apply)
}


object Precision extends Enumeration {
  val Number32 : Precision.Value = Value("32")
  val Number64 : Precision.Value = Value("64")
}

object ContentType extends Enumeration {
  val Mu47zu45int : ContentType.Value = Value("m/z-int")
  val Mu47z : ContentType.Value = Value("m/z")
  val Mu47zruler : ContentType.Value = Value("m/z ruler")
  val TOF : ContentType.Value = Value("TOF")
  val intensity : ContentType.Value = Value("intensity")
  val Su47N : ContentType.Value = Value("S/N")
  val Charge : ContentType.Value= Value("charge")

}

object Polarity extends Enumeration {
  val unknown : Polarity.Value = Value("unknown")
  val U43 : Polarity.Value = Value("+")
  val U45 : Polarity.Value = Value("-")
  val AnyType : Polarity.Value = Value("any")
}

object CompressionType extends Enumeration {
  val NoneType : CompressionType.Value = Value("none")
  val Zlib : CompressionType.Value = Value("zlib")
}

object PairOrder extends Enumeration {
  val Mu47zu45int : PairOrder.Value = Value("m/z-int")
}

case class Peaks(
                  mzsIntensitiesPair : Seq[(Double,Double)], // m/z - intensities
                  precision : Precision.Value,
                  byteOrder : String,
                  pairOrder : Option[PairOrder.Value],
                  compressionType : Option[CompressionType.Value],
                  compressedLen : Option[Int])

object Peaks  {
  import java.util.zip.{Inflater, Deflater} // Zlib library
  private def decompress(inData: Array[Byte]): Array[Byte] = {
    val inflater = new Inflater()
    inflater.setInput(inData)
    val decompressedData = new Array[Byte](inData.length * 2)
    var count = inflater.inflate(decompressedData)
    var finalData = decompressedData.take(count)
    while (count > 0) {
      count = inflater.inflate(decompressedData)
      finalData = finalData ++ decompressedData.take(count)
    }
    finalData
  }

  private def mzIntensitiesReader(
                                   strBase64 : String,
                                   precision : Precision.Value,
                                   compression : CompressionType.Value,
                                 ) : Seq[(Double,Double)] = {

    val arr: Array[Byte] = compression match {
      case CompressionType.NoneType => strBase64.toByteArray
      case CompressionType.Zlib => decompress(strBase64.toByteArray)
    }

    val buffer = ByteBuffer.wrap(arr).order(ByteOrder.BIG_ENDIAN)

   val (mzs, intensities) = precision match {
      case Precision.Number32 =>
        val values = new Array[Float](arr.length / 4)
        val fb: FloatBuffer = buffer.asFloatBuffer();
        fb.get(values)
        val mzs: Seq[Double] = values.zipWithIndex.filter(_._2 % 2 == 0).map(_._1.toDouble)
        val intensities: Seq[Double] = values.zipWithIndex.filter(_._2 % 2 != 0).map(_._1.toDouble)
        (mzs,intensities)
      case Precision.Number64 =>
        val values = new Array[Double](arr.length / 8) // arr.length / 16 => check should be equal => peakCount attribute
        val db: DoubleBuffer = buffer.asDoubleBuffer();
        db.get(values)
        val mzs: Seq[Double] = values.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
        val intensities: Seq[Double] = values.zipWithIndex.filter(_._2 % 2 != 0).map(_._1)
        (mzs,intensities)
    }
    mzs.zipWithIndex.map( x => (x._1,intensities(x._2))).filter(_._2>0) // removes intensities equal to zero
  }

  private def strToPrecision(name: String): Precision.Value =
    mzxml.Precision.values.find(_.toString.toLowerCase() == name.toLowerCase()).getOrElse(Precision.Number32)

  private val mzsIntensitiesPairReader: XmlReader[Seq[(Double,Double)]] = {
    for {
      base64 <- __.read[String]
      precision <- attribute[String]("precision")
      compression <- attribute("compressionType")(enum(CompressionType)).default(CompressionType.NoneType)
    } yield {
      mzIntensitiesReader(base64,strToPrecision(precision),compression)
    }
  }

  implicit val reader: XmlReader[Peaks] =
    (
      mzsIntensitiesPairReader,
      attribute("precision")(enum(Precision)).default(Precision.Number32),
      attribute[String]("byteOrder"),
      attribute("pairOrder")(enum(PairOrder)).default(PairOrder.Mu47zu45int).optional,
      attribute("compressionType")(enum(CompressionType)).default(CompressionType.NoneType).optional,
      attribute[Int]("compressedLen").optional
    ).mapN(apply)
}

case class ScanSequence(nameValue: String, comment: Option[String] = None)

object ScanSequence {
  implicit val reader: XmlReader[ScanSequence] = (
    attribute[String]("nameValue"),
    attribute[String]("comment").optional,
  ).mapN(apply)
}

case class ScanProperties(
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
                           totIonCurrent : Option[Double] = None,
                           msInstrumentID : Option[Int] = None,
                           compensationVoltage : Option[Double] = None,
                       )

object ScanProperties {
  implicit val reader: XmlReader[ScanProperties] = (
    attribute[Int]("num"),
    attribute[Int]("msLevel"),
    attribute[Int]("peaksCount"),
    attribute("polarity")(enum(Polarity)).optional,
    attribute("scanType")(enum(ScanType)).optional,
    attribute[String]("filterLine").optional,
    attribute[Boolean]("centroided").optional,
    attribute[Boolean]("deisotoped").optional,
    attribute[Boolean]("chargeDeconvoluted").optional,
    attribute[String]("retentionTime").map(DatatypeFactory.newInstance().newDuration).map(_.getSeconds).optional,
    attribute[Double]("ionisationEnergy").optional,
    attribute[Double]("collisionEnergy").optional,
    attribute[Double]("cidGasPressure").optional,
    attribute[Double]("startMz").optional,
    attribute[Double]("endMz").optional,
    attribute[Double]("lowMz").optional,
    attribute[Double]("highMz").optional,
    attribute[Double]("basePeakMz").optional,
    attribute[Double]("basePeakIntensity").optional,
    attribute[Double]("totIonCurrent").optional,
    attribute[Int]("msInstrumentID").optional,
    attribute[Double]("compensationVoltage").optional
  ).mapN(apply)
}

case class ScanOrigin(
                       properties : mzxml.ScanProperties,
                       precursorMz: Seq[PrecursorMz] = Nil,
                       maldi: Option[Maldi] = None,
                       peaks: Seq[mzxml.Peaks] = Nil,
                       scanSequence: Seq[mzxml.ScanSequence] = Nil,
                       scan: Seq[mzxml.SubScan] = Nil
                 )

object ScanOrigin {
    implicit val reader: XmlReader[ScanOrigin] = {
      (__.read[ScanProperties],
      ( __ \ "precursorMz").read(seq[PrecursorMz]),
      ( __ \ "maldi").read[Maldi].optional,
      ( __ \ "peaks").read(seq[mzxml.Peaks]),
      ( __ \ "scanSequence").read(seq[mzxml.ScanSequence]),
      ( __ \ "scan").read(seq[mzxml.SubScan])
    ).mapN(apply)
    }
}

case class SubScan(
                    properties : mzxml.ScanProperties,
                    precursorMz: Seq[PrecursorMz] = Nil,
                    maldi: Option[Maldi] = None,
                    peaks: Seq[mzxml.Peaks] = Nil,
                    scansequence: Seq[mzxml.ScanSequence] = Nil,
                    scan: Seq[mzxml.ScanOrigin] = Nil
               )

object SubScan {
  implicit val reader: XmlReader[mzxml.SubScan] = (
    __.read[ScanProperties],
    ( __ \ "precursorMz").read(seq[PrecursorMz]),
    ( __ \ "maldi").read[Maldi].optional,
    ( __ \ "peaks").read(seq[Peaks]),
    ( __ \ "scanSequence").read(seq[ScanSequence]),
    ( __ \ "scan").read(seq[mzxml.ScanOrigin])
  ).mapN(apply)
}

case class Offset(id : Int, value : Int)

object Offset {
  implicit val reader: XmlReader[mzxml.Offset] = ( attribute[Int]("id"), __.read[Int] ).mapN(apply)
}
case class IndexScan(offset : Seq[mzxml.Offset]= Nil)

object IndexScan {
  implicit val reader: XmlReader[IndexScan] = (__ \ "offset").read(seq[mzxml.Offset]).map(apply)
}

case class IndexOffset(value : Int)

object IndexOffset {
  implicit val reader: XmlReader[mzxml.IndexOffset] =
    (__ \ "indexOffset").read[Int].map(apply)
}

