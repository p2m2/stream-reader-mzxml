import $ivy.`com.github.nscala-time::nscala-time:2.32.0`,com.github.nscala_time.time.Imports._
import $ivy.`org.scala-lang.modules::scala-parallel-collections:1.0.4`
import $cp.target.`scala-2.13`.`mzXML-stream-assembly-1.0.jar`
import scala.collection.parallel.CollectionConverters._
import cats.effect.IO
import fs2.{Stream,Pipe}
import cats.effect.unsafe.implicits._
import fr.inrae.p2m2.mzxml._
import cats.effect.IO
import fs2.text
import fs2.io.file.{Files, Path}
import $file.libCandidateIons
import org.joda.time.format.PeriodFormat

//JAVA_OPTS=-Xmx1500M amm distributionDiffIons.sc 23_0234.mzXML 10000 1000

@main
def main(
          mzXMLFile      : String,
          minIntensity   : Double,   // catch peak of interest obove this threshold.
          noiseIntensity : Double,
          thresholdDiffIntensity : Double = 0.001, // above 1 %
          startTime : Double        = 0,
          endTime : Double          = Double.MaxValue
        ) : Unit = {
        
      val processStart:DateTime = DateTime.now()

      val AlllListMat : List[Map[Double,Int]] = 
      candidateIonsGeneric(mzXMLFile,minIntensity,noiseIntensity,thresholdDiffIntensity,startTime,endTime)
        .compile
        .toList
        .unsafeRunSync()
        
      val processEndInter:DateTime = DateTime.now()
      val elapsedInter:Interval = processStart to processEndInter
      println("***************Collect*********************")
      println(elapsedInter.toPeriod.toString(PeriodFormat.getDefault))
      println("************************************\n")

      val listMat : Map[Double,Int] = AlllListMat.fold(Map.empty[Double, Int]) {
         (count : Map[Double,Int], mzsLocalCount : Map[Double,Int]) =>
           count ++ mzsLocalCount.map{  case (mz,c) => mz -> (count.get(mz).getOrElse(0)+c) }
       }
      
      val processEnd:DateTime = DateTime.now()
      val elapsed:Interval = processStart to processEnd
      println("***************collect+fold*********************")
      println(elapsed.toPeriod.toString(PeriodFormat.getDefault))
      println("************************************\n")

      listMat
        .filter(_._2>2)
        .groupBy(_._2)
        .toList
        .sortBy(_._1)
        .reverse
        .map{ case (count : Int,mzs : Map[Double,Int]) => (count,mzs.map(_._1)) }
        .take(40)
        .foreach{ case (count : Int,mzs : Seq[Double]) => {
          println(s"Occurence:$count ionsList :${mzs.mkString(",")}")
        }}
}


def candidateIonsGeneric(
                               mzXMLFile : String,
                               minIntensity: Double,
                               noiseIntensity: Double,
                               thresholdDiffIntensity : Double,
                               startTime: Double,
                               endTime: Double
                             ) : Stream[IO, Map[Double,Int]]   = {

    val fixCom : Int = 100000
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
              case (mz, intensity) => if (intensity>minIntensity) {
                val diffMat : Seq[Double] = 
                  spectrum.peaks
                          .filter{ case (m,i) => i>noiseIntensity && i!=0.0 }
                          .filter{ case (m,i) => (intensity / i)>thresholdDiffIntensity }
                          .map( mz - _._1 ) // calcul du diff
                          .map( x => ((x*fixCom).toInt / fixCom.toDouble) )
                Some(diffMat) 
              } else {
                None
              }
            }
            .foldLeft(Map[Double,Int]()) {
                (countAccumulator, mzs) =>
                mzs
                .map( mz => { mz -> (countAccumulator.get(mz).getOrElse(0)+1)}).toMap
            }
        }
      }
      /*
      .fold(Map.empty[Double, Int]) {
         (count : Map[Double,Int], mzsLocalCount : Map[Double,Int]) =>
           count ++ mzsLocalCount.map{  case (mz,c) => mz -> (count.get(mz).getOrElse(0)+c) }
       }*/
}
