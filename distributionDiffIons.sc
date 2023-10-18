import $ivy.`com.github.nscala-time::nscala-time:2.32.0`,com.github.nscala_time.time.Imports._
import $cp.target.`scala-2.13`.`mzXML-stream-assembly-1.0.jar`
import cats.effect.IO
import fs2.{Stream,Pipe}
import cats.effect.unsafe.implicits._
import fr.inrae.p2m2.mzxml._
import cats.effect.IO
import fs2.text
import fs2.io.file.{Files, Path}
import $file.libCandidateIons
import org.joda.time.format.PeriodFormat


@main
def main(
          mzXMLFile      : String,
          minIntensity   : Double,   // catch peak of interest obove this threshold.
          noiseIntensity : Double,
          startTime : Double        = 0,
          endTime : Double          = Double.MaxValue
        ) : Unit = {
        
      val listMat : Map[Double,Int] = 
      candidateIonsGeneric(mzXMLFile,minIntensity,noiseIntensity,startTime,endTime)
        .compile
        .toList
        .unsafeRunSync()
        .fold(Map.empty[Double, Int]) {
         (count : Map[Double,Int], mzsLocalCount : Map[Double,Int]) =>
           count ++ mzsLocalCount.map{  case (mz,c) => mz -> (count.get(mz).getOrElse(0)+c) }
       }

      listMat
        .filter(_._2>2)
        .groupBy(_._2)
        .toList
        .sortBy(_._1)
        .reverse
        .map( (count,mzs) => (count,mzs.map(_._1)) )
        .take(20)
        .foreach( (count,mzs) => {
          println(s"Occurence:$count ionsList :${mzs.mkString(",")}")
        })
}


def candidateIonsGeneric(
                               mzXMLFile : String,
                               minIntensity: Double,
                               noiseIntensity: Double,
                               startTime: Double,
                               endTime: Double
                             ) : Stream[IO, Map[Double,Int]]   = {

    val fixCom : Int = 10
    SpectrumRequest(mzXMLFile)
      .msLevel(1)
      .filter(_.isDefined)
      .map(_.get)
      .filter(_.retentionTimeInSeconds.getOrElse(0) >= startTime)
      .filter(_.retentionTimeInSeconds.getOrElse(Int.MaxValue) <= endTime)
      .map {
        (spectrum: Spectrum) => {
          spectrum.peaks
            .flatMap {
              case (mz, intensity) => if (intensity>minIntensity) {
                val diffMat : Seq[Double] = spectrum.peaks.filter(_._2>noiseIntensity).map( mz - _._1)
                Some(diffMat.map( mz => (mz*fixCom) / fixCom )) // group by decimal precision 
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
      .fold(Map.empty[Double, Int]) {
         (count : Map[Double,Int], mzsLocalCount : Map[Double,Int]) =>
           count ++ mzsLocalCount.map{  case (mz,c) => mz -> (count.get(mz).getOrElse(0)+c) }
       }
}
