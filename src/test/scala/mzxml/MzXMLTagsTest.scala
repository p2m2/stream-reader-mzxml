package mzxml

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.lucidchart.open.xtract.{ParseResult, XmlReader}
import fs2.Stream
import fs2.io.file.{Files, Flags, Path}
import fs2.data.xml.xpath.{XPath, XPathParser}
import fs2.data.xml.xpath.literals._
import utest.{TestSuite, Tests, test}

import java.nio.ByteOrder
import java.util.Calendar
import scala.xml.{Elem, XML}

object MzXMLTagsTest extends TestSuite {
  val tests: Tests = Tests {
    /**
     * Generation of dump
     */
    test("msInstrument") {
      val xml: Elem =
        <msInstrument>
          <msManufacturer category="msManufacturer" value="Thermo Scientific"/>
          <msModel category="msModel" value="LTQ Orbitrap"/>
          <msIonisation category="msIonisation" value="NSI"/>
          <msMassAnalyzer category="msMassAnalyzer" value="FTMS"/>
          <msDetector category="msDetector" value="unknown"/>
          <software type="acquisition" name="Xcalibur" version="2.4 SP1"/>
        </msInstrument>

      val instrumentPR: ParseResult[MsInstrument] = XmlReader.of[mzxml.MsInstrument].read(xml)
      assert(instrumentPR.isSuccessful)
      instrumentPR.map {
        instrument => {
          assert(instrument.msManufacturer.value =="Thermo Scientific")
          assert(instrument.msModel.value =="LTQ Orbitrap")
          assert(instrument.msIonisation.value =="NSI")
          assert(instrument.software.name=="Xcalibur")
          assert(instrument.software.`type`=="acquisition")
          assert(instrument.software.version=="2.4 SP1")
        }
      }
    }
    test("scan 1") {
      val xml: Elem = <scan num="1"
                            msLevel="1"
                            peaksCount="221"
                            polarity="+"
                            scanType="Full"
                            filterLine="FTMS + p NSI Full ms [375.00-2000.00]"
                            retentionTime="PT480.065S"
                            lowMz="376.83"
                            highMz="1979.57"
                            basePeakMz="1125.3"
                            basePeakIntensity="10168"
                            totIonCurrent="175256">
                      <peaks precision="32"
                             byteOrder="network"
                             pairOrder="m/z-int">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</peaks>
                    </scan>

      val scanPR: ParseResult[Scan1] = XmlReader.of[mzxml.Scan1].read(xml)
      assert(scanPR.isSuccessful)
    }

    /*
    Number of spectrums: 4023
    [129.02807617 136.17666626 145.2933197  147.28193665 148.23001099
     164.23022461 181.37910461 192.29307556 203.05554199 209.21598816
     211.11482239 212.90615845 214.02807617 248.16415405 250.3973999
     259.33673096 273.45806885 286.21496582 289.67486572 292.40325928
     296.4197998  298.52716064 301.0223999  307.16491699 310.19085693
     314.2237854  317.36737061 319.18548584 321.99084473 323.0005188
     325.12640381 334.30078125 345.48345947 349.34121704 356.33081055
     363.5506897  372.27127075 373.24145508 374.08572388 375.11300659
     384.56472778 386.92816162 388.29507446 393.96408081 398.7401123
     399.5645752  408.07421875 412.36166382 413.61959839 416.3565979
     417.00106812 419.69650269 424.73068237 425.62432861 455.62564087
     463.69638062 476.06442261 501.28952026 540.59924316 550.52252197
     558.31573486 559.61352539 575.43243408 593.5145874  641.4553833
     642.37762451 654.50640869 659.644104   660.44561768 703.46331787
     704.78533936 721.54681396 722.40887451 723.19628906]
    [  6.99214268   6.34042501   7.8654871   15.37589359   4.86122608
       4.24357557  13.49875069  20.64873314  11.56574726  72.13915253
      20.25110435  22.30627632   7.66394186   7.54580593  10.86832142
       3.0814364    4.17395067   9.8266325    9.60938835   9.31804085
       4.82175636  21.71094131   4.60328674   4.38589096  11.11993599
      15.02805996  10.71109104   6.13724422  10.6579752   35.058815
      11.33490086   7.98913002   4.50365019   3.51633668  17.21712112
      11.55242825   3.95101547   2.6941781   10.25451279   3.74962711
      12.90698433   2.21336246  21.60174942   9.51155949  22.89513779
      29.35125542  25.38956642   2.21471691  10.27428627 192.66702271
      29.94878006   2.88619065  50.18415833 379.41033936  15.24835014
       7.45273352  12.22535038   3.72599435   5.27811623  17.70541191
       7.46117115   6.59019375  20.70717049   2.64894652  10.79693794
       3.06972408  15.71404934  52.34156799   3.40394187  21.75473404
      12.32615948  20.90596581   9.20400429   8.7657299 ]
    {'charge': 1, 'scan_number': '380', 'title': None, 'precursor_mz': 434.27627563, 'scan_start_time': [], 'retention_time': 15.776100000000001}

     */
    test("scan 2") {
      val xml : Elem = <scan num="380"
                             msLevel="2"
                             peaksCount="74"
                             polarity="+"
                             scanType="Full"
                             filterLine="ITMS + c NSI d Full ms2 434.28@cid35.00 [105.00-880.00]"
                             retentionTime="PT946.566S"
                             lowMz="129.028"
                             highMz="723.196"
                             basePeakMz="425.624"
                             basePeakIntensity="379.41"
                             totIonCurrent="1517.96"
                             collisionEnergy="35">
        <precursorMz precursorIntensity="4773.44" precursorCharge="2" activationMethod="CID">434.27627563</precursorMz>
        <peaks precision="32"
               byteOrder="network"
               pairOrder="m/z-int">QwEHMEDfv6JDCC06QMrkw0MRSxdA+7ISQxNILUF2A6lDFDriQJuPKkMkOvBAh8tfQzVhDUFX+uJDQEsHQaUwm0NLDjhBOQ1NQ1E3S0KQRz9DUx1lQaICQ0NU5/pBsnNBQ1YHMED1PwNDeCoGQPF3PkN6ZbxBLeSlQ4GrGkBFNkFDiLqiQIWRAUOPG4RBHTnjQ5DWYkEZwA5DkjOeQRUWskOUNbxAmkvUQ5VDekGtsAJDloLeQJNOIEOZlRxAjFk4Q5sYbkEx60JDnRylQXBy70OerwZBK2ChQ5+XvkDEZE5DoP7UQSqHEUOhgBFCDDw6Q6KQLkE1W8FDpyaAQP+m9EOsveJAkB3nQ66rrUBhC6lDsipYQYm8qkO1xn1BONa/Q7oiuUB83XBDup7oQCxtakO7CvlBJBJ8Q7uOd0Bv+eRDwEhJQU6DAkPBds5ADae7Q8IlxUGs0GJDxPtnQRgvWUPHXrxBtyk+Q8fIREHqz19DzAmAQcsd1UPOLktADb3sQ87PT0EkY3pD0C2lQ0CqwkPQgCNB75caQ9HZJ0A4t1lD1F2HQki8lEPUz+pDvbSGQ+PQFUFz+T5D59kjQO58y0PuCD9BQ5sJQ/qlD0BudrFEByZaQKjmVEQJoXFBjaSvRAuUNUDuwepEC+dEQNLi3kQP261BpahJRBRg70ApiFdEIF0lQSzAQkQgmCtARHZcRCOgaUF7bL9EJOk5QlFdxEQlHIVAWdovRC/dp0GuCbJEMDJDQUU380Q0Yv9Bpz9rRDSaK0ETQ5pENMyQQQxAbg==</peaks>
      </scan>

      val scanPR: ParseResult[Scan1] = XmlReader.of[mzxml.Scan1].read(xml)
      // println(scanPR)
      assert(scanPR.isSuccessful)
      scanPR.toOption match {
        case Some(scan) =>
          //https://sashimi.sourceforge.net/schema_revision/mzXML_2.1/Doc/mzXML_2.0_tutorial.pdf
          //println("=============>",scan.peaks)
          // LEN(MZ) => 74
          //MZ[0] => 129.02807617
          //INTE[0] => 6.99214268
          println("peaks count:", scan.properties.peaksCount)

          println(scan.peaks.head.mzsIntensitiesPair)

          /* retention time */
          println(scan.properties.retentionTime.get/60.0)

        //arr.foreach(x => println(Integer.reverseBytes(x).toDouble))
        case None => assert(false)
      }

    }
    test("scan 3") {
      val xml : Elem = <scan num="379"
                      msLevel="1"
                      peaksCount="460"
                      polarity="+"
                      scanType="Full"
                      filterLine="FTMS + p NSI Full ms [375.00-2000.00]"
                      retentionTime="PT945.974S"
                      lowMz="381.411"
                      highMz="1992.94"
                      basePeakMz="445.121"
                      basePeakIntensity="738662"
                      totIonCurrent="4.40055e+006">
        <peaks precision="32"
               byteOrder="network"
               pairOrder="m/z-int">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</peaks>


        <scan num="380"
              msLevel="2"
              peaksCount="74"
              polarity="+"
              scanType="Full"
              filterLine="ITMS + c NSI d Full ms2 434.28@cid35.00 [105.00-880.00]"
              retentionTime="PT946.566S"
              lowMz="129.028"
              highMz="723.196"
              basePeakMz="425.624"
              basePeakIntensity="379.41"
              totIonCurrent="1517.96"
              collisionEnergy="35">
          <precursorMz precursorIntensity="4773.44" precursorCharge="2" activationMethod="CID">434.27627563</precursorMz>
          <peaks precision="32"
                 byteOrder="network"
                 pairOrder="m/z-int">QwEHMEDfv6JDCC06QMrkw0MRSxdA+7ISQxNILUF2A6lDFDriQJuPKkMkOvBAh8tfQzVhDUFX+uJDQEsHQaUwm0NLDjhBOQ1NQ1E3S0KQRz9DUx1lQaICQ0NU5/pBsnNBQ1YHMED1PwNDeCoGQPF3PkN6ZbxBLeSlQ4GrGkBFNkFDiLqiQIWRAUOPG4RBHTnjQ5DWYkEZwA5DkjOeQRUWskOUNbxAmkvUQ5VDekGtsAJDloLeQJNOIEOZlRxAjFk4Q5sYbkEx60JDnRylQXBy70OerwZBK2ChQ5+XvkDEZE5DoP7UQSqHEUOhgBFCDDw6Q6KQLkE1W8FDpyaAQP+m9EOsveJAkB3nQ66rrUBhC6lDsipYQYm8qkO1xn1BONa/Q7oiuUB83XBDup7oQCxtakO7CvlBJBJ8Q7uOd0Bv+eRDwEhJQU6DAkPBds5ADae7Q8IlxUGs0GJDxPtnQRgvWUPHXrxBtyk+Q8fIREHqz19DzAmAQcsd1UPOLktADb3sQ87PT0EkY3pD0C2lQ0CqwkPQgCNB75caQ9HZJ0A4t1lD1F2HQki8lEPUz+pDvbSGQ+PQFUFz+T5D59kjQO58y0PuCD9BQ5sJQ/qlD0BudrFEByZaQKjmVEQJoXFBjaSvRAuUNUDuwepEC+dEQNLi3kQP261BpahJRBRg70ApiFdEIF0lQSzAQkQgmCtARHZcRCOgaUF7bL9EJOk5QlFdxEQlHIVAWdovRC/dp0GuCbJEMDJDQUU380Q0Yv9Bpz9rRDSaK0ETQ5pENMyQQQxAbg==</peaks>
        </scan>
      </scan>

      val scanPR: ParseResult[Scan1] = XmlReader.of[mzxml.Scan1].read(xml)
      assert(scanPR.isSuccessful)
      assert(scanPR.toOption.isDefined)
      }
  }
}
