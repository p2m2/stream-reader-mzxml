package fr.inrae.p2m2.mzxml.utils

import scala.math.BigDecimal.double2bigDecimal
import scala.math.{exp, log, sqrt}

case object ChemicalConst {

  val massProton: Double = 1.007276
  val massNeutron: Double = 1.008665
  val massElectron: Double = 0.0000549

  val abundanceIsotope: Map[String, Seq[Double]] = Map(
    "C" -> Seq(1.0,0.0108),
    "H" -> Seq(1.0,0.00012),
    "O" -> Seq(1.0,0.0004,0.002),
    "S" -> Seq(1.0,0.00789,0.0444),
    "N" -> Seq(1.0, 0.0037),
    "Cl" -> Seq(1.0,0.0,0.3198)
  )

  val massAtom : Map[String, Double] = Map(
    "C" ->  12.0,
    "12C" ->  12.0,
    "13C" -> 13.0033,
    "H" -> 1.007825,
    "2H" -> 2.0140,
    "O" -> 15.99499,
    "16O" -> 15.99499,
    "17O" -> 16.9991,
    "18O" -> 17.9992,
    "S" -> 31.9721,
    "32S" -> 31.9721,
    "33S" -> 32.9715,
    "34S" -> 33.9679,
    "N" -> 14.00307,
    "14N" -> 14.00307,
    "15N" -> 15.0001,
    "Cl" -> 34.9688,
    "35Cl"-> 34.9688,
    "37Cl" -> 36.9659
  )

  /**
   * Molecule composition
   * @param atoms Atomic and number of element
   */
  case class MoleculeComposition(atoms : Map[String,Int] ) {
    def monoIsotopicMass: Double = atoms.foldLeft(0.0) {
      case (sum,at -> nb) => sum + nb * massAtom.getOrElse(at,0.0)
    }

    /**
     * probability that there are the Ieme Isopope "n" times
     * i=1, n=2 => part of M+2 - probability of Isotope 1 is two times
     *
     * @param i
     * @param nb
     */
    def probabilityIsotope(i: Int, n: Int): Double = {
      atoms.map {
        case atom -> nb => abundanceIsotope.get(atom) match {
          case Some(listProbaIsotope)
            if
              (listProbaIsotope.size > i) // check isotope existance
                && (nb>=n)  // formula should contains at least "n" times the atom
          =>
            1.to(n).foldLeft(1.0)((s, _) => s * listProbaIsotope(i)) * nb.toDouble
          case _ => 0.0
        }
      }.foldLeft(0.0)((s, u) => s + u)
    }

    /**
     * Get isotopic distribution
     * @return
     */
    def isotopicCluster : Seq[Double] = {

      Seq(
        probabilityIsotope(1,1), //M1
        probabilityIsotope(1,2) + probabilityIsotope(2,1), //M2
        probabilityIsotope(1,3) + (probabilityIsotope(2,1)*probabilityIsotope(1,1)), // M3
        probabilityIsotope(1,4)
          + (probabilityIsotope(2,1)*probabilityIsotope(1,2))
          + probabilityIsotope(2,2), // M4
        probabilityIsotope(1,5)
          + (probabilityIsotope(2,1)*probabilityIsotope(1,3))
          + (probabilityIsotope(2,2)*probabilityIsotope(1,1)), //M5
        probabilityIsotope(1, 6)
          + (probabilityIsotope(2, 1) * probabilityIsotope(1, 4))
          + (probabilityIsotope(2, 2) * probabilityIsotope(1, 2))
          + probabilityIsotope(2, 3), //M6
      )
    }

  }

  /**
   * Decompose a molecule into atomic element
   * @param formula
   * @return
   */
  def composition(formula : String): MoleculeComposition = {
    MoleculeComposition("(\\w)(\\d*)".r.findAllMatchIn(formula).map  {
      sol =>
        sol.group(2) match {
          case "" => sol.group(1) -> 1
          case _ =>  sol.group(1) -> sol.group(2).toInt
        }
    }.toSeq.toMap)
  }

  /**
   * Return a correlation R given a formula and mz of Isotope (M0, M1, M2, ...)
   * @param formula
   * @param mzIsotopicMolecule
   * @return
   */
  def correlation(formula : String, mzIsotopicMolecule : Seq[Double] ) : Double = {

    val isotopicDistribution = Seq(1.0) ++ composition(formula).isotopicCluster.slice(0,mzIsotopicMolecule.size-1)

    val t1 = mzIsotopicMolecule //.map(exp)
    val t2 = isotopicDistribution // .map(exp)

    val meanX : Double = t1.sum / t1.size
    val meanY  : Double = t2.sum / t2.size

    val sNum = t1.indices.map(
      i => (t1(i)-meanX)*(t2(i)-meanY)
    ).sum
    val s2 = t1.map( v => (v - meanX)*(v - meanX) ).sum
    val s3 = t2.map( v => (v - meanY)*(v - meanY) ).sum

    val r = sNum / sqrt( s2*s3 )
    r*r
  }

}
