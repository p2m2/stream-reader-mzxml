package mzxml

import com.lucidchart.open.xtract.{XmlReader, __}
import com.lucidchart.open.xtract.XmlReader._
import cats.syntax.all._

import javax.xml.datatype.{DatatypeFactory, Duration}

object FileType extends Enumeration {
  val unknown = Value("unknown")
  val RAWData = Value("RAWData")
  val ProcessedData = Value("processedData")
}

case class ParentFile(
                       fileName : java.net.URI,
                       fileType : FileType.Value,
                       fileSha1 : String
                     )

object ParentFile {
  implicit val reader: XmlReader[ParentFile] = (
    attribute[String]("fileName").map(java.net.URI.create),
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


case class Operator(first : String,last : String,phone : String,email : Option[String] = None,URI : java.net.URI)

object Operator {
  
  def validateEmail(email: String): Boolean = email contains "@"

  implicit val reader: XmlReader[Operator] = (
    attribute[String]("first"),
    attribute[String]("last"),
    attribute[String]("phone"),
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
  msManufacturer: mzxml.OntologyEntryTypable,
  msModel: mzxml.OntologyEntryTypable,
  msIonisation: mzxml.OntologyEntryTypable,
  msMassAnalyzer: mzxml.OntologyEntryTypable,
  msDetector: mzxml.OntologyEntryTypable,
  software: mzxml.Software,
  msResolution: Option[mzxml.OntologyEntryTypable] = None,
  operator: Option[mzxml.Operator] = None,
  msinstrumentsequence1: Seq[mzxml.MsInstrumentSequence] = Nil)

object MsInstrument {

  implicit val reader: XmlReader[MsInstrument] = {
    println("Hello Wolrd")
    println(attribute[String]("msInstrumentID").optional.toString)
   (
    attribute[String]("msInstrumentID").optional,
    (__ \ "msManufacturer").read[mzxml.OntologyEntryTypable],
    (__ \ "msModel").read[mzxml.OntologyEntryTypable],
    (__ \ "msIonisation").read[mzxml.OntologyEntryTypable],
   (__ \ "msMassAnalyzer").read[mzxml.OntologyEntryTypable],
   (__ \ "msDetector").read[mzxml.OntologyEntryTypable],
   (__ \ "software").read[Software],
   (__ \ "msResolution").read[mzxml.OntologyEntryTypable].optional,
    (__ \ "operator").read[mzxml.Operator].optional,
   (__ \ "msinstrumentsequence").read(seq[mzxml.MsInstrumentSequence])
  ).mapN(apply)
  }
}
      

case class DataProcessing(
                           intensityCutoff : Option[Double],
                           centroided : Option[Boolean],
                           deisotoped : Option[Boolean],
                           chargeDeconvoluted : Option[Boolean],
                           spotIntegration : Option[Boolean],
                           software: mzxml.Software,
                           dataprocessingsequence1: Seq[mzxml.DataProcessingSequence] = Nil
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
  val unknown = Value("unknown")
  val ETD = Value("ETD")
  val ECD = Value("ECD")
  val CID = Value("CID")
  val HCD = Value("HCD")
  val ETDu43SA = Value("ETDu43SA")
}

case class PrecursorMz(
                        value: Double,
                        precursorScanNum : Option[BigInt],
                        precursorIntensity : Option[Double],
                        precursorCharge : Option[BigInt],
                        possibleCharges : Option[String],
                        windowWideness : Option[Double],
                        activationMethod : ActivationMethod.Value
                      )
object PrecursorMz {
  implicit val reader: XmlReader[PrecursorMz] = (
    ( __ ).read[Double],
    attribute[String]("precursorScanNum").map(BigInt(_)).optional,
    attribute[Double]("precursorIntensity").optional,
    attribute[String]("precursorCharge").map(BigInt(_)).optional,
    attribute[String]("possibleCharges").optional,
    attribute[Double]("windowWideness").optional,
    attribute("activationMethod")(enum(ActivationMethod)).default(ActivationMethod.unknown),
  ).mapN(apply)
}

object ScanType extends Enumeration {
  val unknown = Value("unknown")
  val Full = Value("Full")
  val Zoom = Value("Zoom")
  val SIM = Value("SIM")
  val SRM = Value("SRM")
  val CRM = Value("CRM")
  val Q1 = Value("Q1")
  val Q3 = Value("Q3")
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
  val unknown = Value("unknown")
  val Number32 = Value("32")
  val Number64 = Value("64")
}

object ContentType extends Enumeration {
  val unknown = Value("unknown")
  val Mu47zu45int = Value("m/z-int")
  val Mu47z = Value("m/z")
  val Mu47zruler = Value("m/z ruler")
  val TOF = Value("TOF")
  val intensity = Value("intensity")
  val Su47N = Value("S/N")
  val Charge = Value("charge")

}

object Polarity extends Enumeration {
  val unknown = Value("unknown")
  val U43 = Value("+")
  val U45 = Value("-")
  val AnyType = Value("any")
}

object CompressionType extends Enumeration {
  val unknown = Value("unknown")
  val NoneType = Value("none")
  val Zlib = Value("zlib")
}

case class Peaks(
                  valueBase64 : String,
                  precision : Option[Precision.Value],
                  byteOrder : String,
                  contentType : Option[ContentType.Value],
                  compressionType : Option[CompressionType.Value],
                  compressedLen : Option[Int])

object Peaks  {
  implicit val reader: XmlReader[Peaks] = (
    ( __ ).read[String],
    attribute("precision")(enum(Precision)).default(Precision.unknown).optional,
    attribute[String]("byteOrder"),
    attribute("contentType")(enum(ContentType)).default(ContentType.unknown).optional,
    attribute("compressionType")(enum(CompressionType)).default(CompressionType.unknown).optional,
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
                         num: BigInt,
                         msLevel: Int,
                         peaksCount: BigInt,
                         polarity: Polarity.Value,
                         scanType: ScanType.Value,
                         filterLine: String,
                         centroided: Option[Boolean],
                         deisotoped: Option[Boolean],
                         chargeDeconvoluted: Option[Boolean],
                         retentionTime: Option[Duration],
                         ionisationEnergy: Option[Double],
                         collisionEnergy: Option[Double],
                         cidGasPressure: Option[Double],
                         startMz: Option[Double],
                         endMz: Option[Double],
                         lowMz: Option[Double],
                         highMz: Option[Double],
                         basePeakMz: Option[Double],
                         basePeakIntensity: Option[Double],
                         totIonCurrent : Option[Double],
                         msInstrumentID : Option[Int],
                         compensationVoltage : Option[Double],
                       )

object ScanProperties {
  implicit val reader: XmlReader[ScanProperties] = (
    attribute[String]("num").map(BigInt(_)),
    attribute[Int]("msLevel"),
    attribute[String]("peaksCount").map(BigInt(_)),
    attribute("polarity")(enum(Polarity)).default(Polarity.unknown),
    attribute("scanType")(enum(ScanType)).default(ScanType.unknown),
    attribute[String]("filterLine"),
    attribute[Boolean]("centroided").optional,
    attribute[Boolean]("deisotoped").optional,
    attribute[Boolean]("chargeDeconvoluted").optional,
    attribute[String]("retentionTime").map(DatatypeFactory.newInstance().newDuration).optional,
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

case class Scan1(
                 properties : ScanProperties,
                 precursorMz: Seq[mzxml.PrecursorMz] = Nil,
                 maldi: Option[mzxml.Maldi] = None,
                 peaks: Seq[mzxml.Peaks] = Nil,
                 scansequence: Seq[mzxml.ScanSequence] = Nil,
                 scan: Seq[mzxml.Scan2] = Nil
                 )

object Scan1 {
    implicit val reader: XmlReader[Scan1] = (
        __.read[ScanProperties],
      ( __ \ "precursorMz").read(seq[PrecursorMz]),
      ( __ \ "maldi").read[Maldi].optional,
      ( __ \ "peaks").read(seq[Peaks]),
      ( __ \ "scanSequence").read(seq[ScanSequence]),
      ( __ \ "scan").read(seq[mzxml.Scan2])
    ).mapN(apply)
}

case class Scan2(
                 properties : ScanProperties,
                 precursorMz: Seq[mzxml.PrecursorMz] = Nil,
                 maldi: Option[mzxml.Maldi] = None,
                 peaks: Seq[mzxml.Peaks] = Nil,
                 scansequence: Seq[mzxml.ScanSequence] = Nil,
                 scan: Seq[mzxml.Scan1] = Nil
               )

object Scan2 {
  implicit val reader: XmlReader[Scan2] = (
    __.read[ScanProperties],
    ( __ \ "precursorMz").read(seq[PrecursorMz]),
    ( __ \ "maldi").read[Maldi].optional,
    ( __ \ "peaks").read(seq[Peaks]),
    ( __ \ "scanSequence").read(seq[ScanSequence]),
    ( __ \ "scan").read(seq[mzxml.Scan1])
  ).mapN(apply)
}

/*
case class Separation(separationTechnique: Seq[mzxml.SeparationTechniqueType] = Nil)
      
      


case class Orientation(attributes: Map[String, scalaxb.DataRecord[Any]] = Map.empty) {
  lazy val firstSpotID = attributes("@firstSpotID").as[String]
  lazy val secondSpotID = attributes("@secondSpotID").as[String]
}

      
      


case class Pattern(spottingPattern: mzxml.OntologyEntryTypable,
  orientation: mzxml.Orientation)
      
      


case class Spot(maldiMatrix: mzxml.OntologyEntryTypable,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map.empty) {
  lazy val spotID = attributes("@spotID").as[String]
  lazy val spotXPosition = attributes("@spotXPosition").as[String]
  lazy val spotYPosition = attributes("@spotYPosition").as[String]
  lazy val spotDiameter = attributes.get("@spotDiameter") map { _.as[BigInt]}
}

      
      


case class Plate(plateManufacturer: mzxml.OntologyEntryTypable,
  plateModel: mzxml.OntologyEntryTypable,
  pattern: Option[mzxml.Pattern] = None,
  spot: Seq[mzxml.Spot] = Nil,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map.empty) {
  lazy val plateID = attributes("@plateID").as[BigInt]
  lazy val spotXCount = attributes("@spotXCount").as[BigInt]
  lazy val spotYCount = attributes("@spotYCount").as[BigInt]
}

      
      


case class Robot(robotManufacturer: mzxml.OntologyEntryTypable,
  robotModel: mzxml.OntologyEntryTypable,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map.empty) {
  lazy val timePerSpot = attributes("@timePerSpot").as[javax.xml.datatype.Duration]
  lazy val deadVolume = attributes.get("@deadVolume") map { _.as[BigInt]}
}

      
    
case class Spotting(plate: Seq[mzxml.Plate] = Nil,
  robot: Option[mzxml.Robot] = None)
      
    

case class MsRun(parentFile: Seq[mzxml.ParentFile] = Nil,
  msInstrument: Seq[mzxml.MsInstrument] = Nil,
  dataProcessing: Seq[mzxml.DataProcessing] = Nil,
  separation: Option[mzxml.Separation] = None,
  spotting: Option[mzxml.Spotting] = None,
  scan: Seq[mzxml.Scan] = Nil,
  sha1: Option[String] = None,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map.empty) {
  lazy val scanCount = attributes.get("@scanCount") map { _.as[BigInt]}
  lazy val startTime = attributes.get("@startTime") map { _.as[javax.xml.datatype.Duration]}
  lazy val endTime = attributes.get("@endTime") map { _.as[javax.xml.datatype.Duration]}
}


sealed trait Type

object Type {
  def fromString(value: String, scope: scala.xml.NamespaceBinding)(implicit fmt: scalaxb.XMLFormat[mzxml.Type]): Type = fmt.reads(scala.xml.Text(value), Nil) match {
    case Right(x: Type) => x
    case x => throw new RuntimeException(s"fromString returned unexpected value $x for input $value")
  }
  lazy val values: Seq[Type] = Seq(Acquisition, Conversion, Processing)
}

case object Acquisition extends Type { override def toString = "acquisition" }
case object Conversion extends Type { override def toString = "conversion" }
case object Processing extends Type { override def toString = "processing" }
      
      

sealed trait Polarity

object Polarity {
  def fromString(value: String, scope: scala.xml.NamespaceBinding)(implicit fmt: scalaxb.XMLFormat[mzxml.Polarity]): Polarity = fmt.reads(scala.xml.Text(value), Nil) match {
    case Right(x: Polarity) => x
    case x => throw new RuntimeException(s"fromString returned unexpected value $x for input $value")
  }
  lazy val values: Seq[Polarity] = Seq(U43, U45, AnyType)
}



sealed trait ScanType

object ScanType {
  def fromString(value: String, scope: scala.xml.NamespaceBinding)(implicit fmt: scalaxb.XMLFormat[mzxml.ScanType]): ScanType = fmt.reads(scala.xml.Text(value), Nil) match {
    case Right(x: ScanType) => x
    case x => throw new RuntimeException(s"fromString returned unexpected value $x for input $value")
  }
  lazy val values: Seq[ScanType] = Seq(Full, Zoom, SIM, SRM, CRM, Q1, Q3)
}




case class ScanOrigin(attributes: Map[String, scalaxb.DataRecord[Any]] = Map.empty) {
  lazy val parentFileID = attributes("@parentFileID").as[String]
  lazy val num = attributes("@num").as[BigInt]
}

      
      

sealed trait ActivationMethod

object ActivationMethod {
  def fromString(value: String, scope: scala.xml.NamespaceBinding)(implicit fmt: scalaxb.XMLFormat[mzxml.ActivationMethod]): ActivationMethod = fmt.reads(scala.xml.Text(value), Nil) match {
    case Right(x: ActivationMethod) => x
    case x => throw new RuntimeException(s"fromString returned unexpected value $x for input $value")
  }
  lazy val values: Seq[ActivationMethod] = Seq(ETD, ECD, CID, HCD, ETDu43SA)
}

case object ETD extends ActivationMethod { override def toString = "ETD" }
case object ECD extends ActivationMethod { override def toString = "ECD" }
case object CID extends ActivationMethod { override def toString = "CID" }
case object HCD extends ActivationMethod { override def toString = "HCD" }
case object ETDu43SA extends ActivationMethod { override def toString = "ETD+SA" }




      
      



      
      

sealed trait Precision

object Precision {
  def fromString(value: String, scope: scala.xml.NamespaceBinding)(implicit fmt: scalaxb.XMLFormat[mzxml.Precision]): Precision = fmt.reads(scala.xml.Text(value), Nil) match {
    case Right(x: Precision) => x
    case x => throw new RuntimeException(s"fromString returned unexpected value $x for input $value")
  }
  lazy val values: Seq[Precision] = Seq(Number32, Number64)
}

case object Number32 extends Precision { override def toString = "32" }
case object Number64 extends Precision { override def toString = "64" }

sealed trait ContentType

object ContentType {
  def fromString(value: String, scope: scala.xml.NamespaceBinding)(implicit fmt: scalaxb.XMLFormat[mzxml.ContentType]): ContentType = fmt.reads(scala.xml.Text(value), Nil) match {
    case Right(x: ContentType) => x
    case x => throw new RuntimeException(s"fromString returned unexpected value $x for input $value")
  }
  lazy val values: Seq[ContentType] = Seq(Mu47zu45int, Mu47z, Mu47zruler, TOF, Intensity, Su47N, Charge)
}

case object Mu47zu45int extends ContentType { override def toString = "m/z-int" }
case object Mu47z extends ContentType { override def toString = "m/z" }
case object Mu47zruler extends ContentType { override def toString = "m/z ruler" }
case object TOF extends ContentType { override def toString = "TOF" }
case object Intensity extends ContentType { override def toString = "intensity" }
case object Su47N extends ContentType { override def toString = "S/N" }
case object Charge extends ContentType { override def toString = "charge" }

sealed trait CompressionType

object CompressionType {
  def fromString(value: String, scope: scala.xml.NamespaceBinding)(implicit fmt: scalaxb.XMLFormat[mzxml.CompressionType]): CompressionType = fmt.reads(scala.xml.Text(value), Nil) match {
    case Right(x: CompressionType) => x
    case x => throw new RuntimeException(s"fromString returned unexpected value $x for input $value")
  }
  lazy val values: Seq[CompressionType] = Seq(NoneType, Zlib)
}

case object NoneType extends CompressionType { override def toString = "none" }
case object Zlib extends CompressionType { override def toString = "zlib" }


      
      


      
      


      
*/
