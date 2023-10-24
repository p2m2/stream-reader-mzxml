
## ammonite example

## Using Docker

```bash
docker build . -t stream-reader-mzxml
docker run -v /Users/andy/mydata:/mnt/mydata stream-reader-mzxml
```

### build jar

```bash
sbt assembly
```
### Number of ions by intensity thresholds

```bash
java -cp ./assembly/pack.jar MainDistributionIntensityIons
```

### Occurrences of the most frequent Mz (Ions)

```bash
java -cp ./assembly/pack.jar MainDistributionMzIons
```

### Gives the occurrences of the difference between the Mz (mass on charge) of interest and the other ions in the same mass spectrum to detect the formation of adducts.

```bash
java -cp ./assembly/pack.jar MainDistributionDiffMzIons
```

### Precursor Mz search

```scala
import $cp.`target/scala-2.13/mzXML-stream-assembly-1.0.jar`
import cats.effect.{IO, IOApp}
import fs2.{Stream, text, Pipe}
import fs2.io.file.{Files, Path}
import java.nio.file.Paths

import cats.effect.unsafe.implicits._
import fr.inrae.p2m2.mzxml._


val mzXMLFile = "./src/test/resources/LTQ_Orbitrap_precision32.mzXML"
val outputFile = "precursor_288p93.txt"


val formatPrecursorMz : Pipe[IO, Option[Seq[PrecursorMz]], String] = {
inStream =>
    inStream.map {
    case Some(p) => s"Precursor ${p.head.value} with precursorIntensity ${p.head.precursorIntensity} " +
        s"and precursorScanNum ${p.head.precursorScanNum}\n"
    case _ => ""
    }
}

SpectrumRequest(mzXMLFile).precursorMz(288.93,5000).map {
    case Some(r) => Some(r.precursorMz)
    case None => None
}
.filter(_.isDefined)
.through(formatPrecursorMz)
.through(text.utf8.encode)
.through(Files[IO].writeAll(Path(outputFile)))
.compile
.drain
.unsafeRunSync()

println(outputFile)
```


###

```scala
import $cp.`target/scala-2.13/mzXML-stream-assembly-1.0.jar`
import cats.effect.{IO, IOApp}
import fs2.{Stream, text, Pipe}
import fs2.io.file.{Files, Path}
import java.nio.file.Paths

import cats.effect.unsafe.implicits._
import fr.inrae.p2m2.mzxml._
import fr.inrae.p2m2.mzxml.utils.ChemicalConst

val mzXMLFile = "./src/test/resources/LTQ_Orbitrap_precision32.mzXML"
val deltaMp0Mp2     = 1.996 // Glucosinolate
val numberCarbonMin = 3
val numberCarbonMax = 35
val numberSulfurMin = 1.5
val numberSulfurMax = 5
val startTime = 0
val endTime = 5

{
  SpectrumRequest(mzXMLFile)
    .msLevel(1)
    .filter(_.isDefined)
    .map( _.get)
    .filter( _.retentionTimeInSeconds.getOrElse(0)>=startTime)
    .filter( _.retentionTimeInSeconds.getOrElse(Int.MaxValue)<=endTime)
    .map {
      (spectrum: Spectrum) => {
        spectrum.peaks.map {
            case (mz0, int0) =>
              val mz_ms_p2 = mz0 + deltaMp0Mp2
              val (mz1, int1) = spectrum.findClosestValueMz(mz0 + 1.0)
              val (mz2, int2) = spectrum.findClosestValueMz(mz_ms_p2)
              ((mz0, int0), (mz1, int1), (mz2, int2)) // isotopes

          }.filter {
            case (v0, v1, _) =>
              v1._2 >= v0._2 *
                (ChemicalConst.abundanceIsotope("C")(1) * numberCarbonMin +
                  ChemicalConst.abundanceIsotope("S")(1) * numberSulfurMin)

          }.filter {
            case (v0, v1, _) =>
              v1._2 < v0._2 *
                (ChemicalConst.abundanceIsotope("C")(1) * numberCarbonMax +
                  ChemicalConst.abundanceIsotope("S")(1) * numberSulfurMax)

          } /* criteria M2 of Isotope S are present 4.4 % */
          .filter {
            case (v0, _, v2) =>
              v2._2 >= v0._2 * ChemicalConst.abundanceIsotope("S")(2) * numberSulfurMin
          }
          .filter {
            case (v0, _, v2) =>
              v2._2 < v0._2 * ChemicalConst.abundanceIsotope("S")(2) * numberSulfurMax
          }.map(
            (spectrum.retentionTimeInSeconds.getOrElse(-1), _)
          )
      }
    }
    .map(x => x.map(y => y.toString()+"\n").mkString("\n"))
    .through(text.utf8.encode)
    .through(Files[IO].writeAll(Path(outputFile)))
    .compile
    .drain
    .unsafeRunSync()
}

```

`amm example.sc`
```bash
amm glucosinolateIons.sc ../mzxml-glucosinolate-analyser/src/test/resources/20181018-037.mzXML 100000
```


## test

```bash
sbt "run ./src/test/resources/LTQ_Orbitrap_precision32.mzXML"
```


## specifications

https://sashimi.sourceforge.net/schema_revision/mzXML_2.1/Doc/mzXML_2.0_tutorial.pdf

