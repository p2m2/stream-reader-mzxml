package fr.inrae.p2m2.`type`

object IonEligibleGlucosinolate {
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
case class IonEligibleGlucosinolate(
                rt: Int,
                level: Int,
                scan: Int,
                m0: Double,
                i0: Double,
                m1: Double,
                i1: Double,
                m2: Double,
                i2: Double,
                fragments: Seq[(Double, Double)] = Seq()) {

  def scoreNeutralLoss(): Int =
    IonEligibleGlucosinolate.mzsNL.values
      .map(m0 - _)
      .filter(_ > 0)
      .filter(
        mzNL =>
          fragments
            .map(_._1)
            .exists(mzF => Math.abs(mzNL - mzF) < 0.1)).toSeq.length


  def scoreDaughterIons(): Int =
    IonEligibleGlucosinolate.mzsDI
      .values
      .filter(mzNL =>
        fragments
          .map(_._1)
          .exists(mzF => Math.abs(mzNL - mzF) < 0.2)).toSeq.length


  override def toString: String = {
    val sc1 = scoreNeutralLoss()
    val sc2 = scoreDaughterIons()
    s"$rt;$level;$scan;$m0;$i0;${sc1 + sc2};$sc1;$sc2"
  }
}