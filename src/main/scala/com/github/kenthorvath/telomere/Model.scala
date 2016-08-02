package com.github.kenthorvath.telomere

/**
  * Created by kent on 8/1/16.
  */
object Model {

  def baseProbabilityOfDeath(age: Int): Double = age match {
    //Citation: DOI: 10.1002/ajpa.22495
    case 0 => 0.07
    case 1 => 0.07
    case 2 => 0.06
    case 3 => 0.05
    case 4 => 0.04
    case 5 => 0.03
    case n if n <= 10 => 0.02
    case n if n <= 40 => 0.015
    case n if n <= 45 => 0.018
    case n if n <= 50 => 0.02
    case n if n <= 55 => 0.03
    case n if n <= 60 => 0.04
    case n if n <= 65 => 0.08
    case n if n <= 70 => 0.12
    case n if n <= 75 => 0.20
    case n if n <= 85 => 0.30
    case _ => 1.00
  }

  case class AllCauseMortalityModel(f: (Int) => Double, description: String) {
    override def toString = description
  }

  val archaicMortality = AllCauseMortalityModel(baseProbabilityOfDeath _, "Archaic")

  def baseProbabilityOfPregnancy(age: Int): Double = age match {
    //Citation: DOI: 10.1002/ajpa.22495
    case n if n <= 13 => 0.0
    case n if n <= 15 => 0.01
    case n if n <= 20 => 0.15
    case n if n <= 25 => 0.28
    case n if n <= 35 => 0.28
    case n if n <= 40 => 0.25
    case n if n <= 45 => 0.15
    case n if n <= 50 => 0.08
    case n if n <= 55 => 0.01
    case _ => 0.0
  }

  case class FecundityModel(f: (Int) => Double, description: String) {
    override def toString = description
  }

  val archaicFecundity = FecundityModel(baseProbabilityOfPregnancy _, "Archaic")

  case class CancerIncidenceAgeAdjustment(f: (Int, Int) => (Int, Int), description: String) {
    override def toString = description
  }

  def csvHeader: String = {
    s"sexEffect," +
      s"pacEffect," +
      s"tlDependentCA," +
      s"CA Risk Adjustment," +
      s"Maternal Inheritance," +
      s"Fecundity Model," +
      s"Mortality Model," +
      s"Initial Population TL"
  }

  case class Options(pacEffect: Boolean, sexEffect: Boolean, maternalInheritance: Double,
                     tlDependentCancer: Boolean, cancerIncidenceAgeTLAdjustment: Option[CancerIncidenceAgeAdjustment] = None,
                     allCauseMortalityForAge: AllCauseMortalityModel, fecundityForAge: FecundityModel, initialPopulationTL: Int) {
    assert(maternalInheritance >= 0.0 && maternalInheritance <= 1.0, "Maternal contribution must be between 0 and 1")
    assert(initialPopulationTL > 0, "Telomere length must be greater than zero")


    override def toString: String = {
      def withOrWithoutIndicator(x: Boolean): String = if (x) "(+)" else "(-)"
      s"${withOrWithoutIndicator(sexEffect)}," +
        s"${withOrWithoutIndicator(pacEffect)}," +
        s"${withOrWithoutIndicator(tlDependentCancer)}," +
        s"${cancerIncidenceAgeTLAdjustment.getOrElse("None")}," +
        s"$maternalInheritance," +
        s"$fecundityForAge," +
        s"$allCauseMortalityForAge," +
        s"$initialPopulationTL"
    }
  }

}
