/**
  * Model
  * -----
  *
  * A collection of epidemiological data and descriptions taken from CDC sources required for our models
  *
  * Author: Kent Horvath, MD PhD
  * Date: October 6, 2016
  * License: MIT
  */

package com.github.kenthorvath.telomere

object Model {

  def baseProbabilityOfDeath(age: Int): Double = age match {
    //Citation: http://www.cdc.gov/nchs/data/dvs/lcwk1_2014.pdf
    case n if n <= 4 => 24.0 / 1e5
    case n if n <= 9 => 11.5 / 1e5
    case n if n <= 14 => 14.0 / 1e5
    case n if n <= 19 => 45.0 / 1e5
    case n if n <= 24 => (83.8 - 4.2) / 1e5
    case n if n <= 29 => (99.7 - 6.2) / 1e5
    case n if n <= 34 => (117.3 - 10.5) / 1e5
    case n if n <= 39 => (147.2 - 39.5) / 1e5
    case n if n <= 44 => (202.4 - 39.8) / 1e5
    case n if n <= 49 => (311.3 - 69.5) / 1e5
    case n if n <= 54 => (491.3 - 134.3) / 1e5
    case n if n <= 59 => (730.6 - 232.2) / 1e5
    case n if n <= 64 => (1032.2 - 351.9) / 1e5
    case n if n <= 69 => (1454.0 - 508.3) / 1e5
    case n if n <= 74 => (2246.1 - 734.3) / 1e5
    case n if n <= 79 => (3560.5 - 999.4) / 1e5
    case n if n <= 84 => (5944.6 - 1448.3) / 1e5
    case _ => 1.00
  }

  case class AllCauseMortalityModel(f: (Int) => Double, description: String) {
    override def toString: String = description
  }

  val mortality = AllCauseMortalityModel(baseProbabilityOfDeath, "Modern")

  def baseProbabilityOfPregnancy(age: Int): Double = {

    val scalingFactor = 1.5

    val probability = age match {
      //Citation: http://www.cdc.gov/nchs/data/nvsr/nvsr64/nvsr64_12.pdf Table 12.
      case n if n <= 14 => 0.3 / 1000
      case n if n <= 19 => 24.2 / 1000
      case n if n <= 24 => 79.0 / 1000
      case n if n <= 29 => 105.8 / 1000
      case n if n <= 34 => 100.8 / 1000
      case n if n <= 39 => 51.0 / 1000
      case n if n <= 44 => 10.6 / 1000
      case n if n <= 49 => 0.8 / 1000
      case _ => 0.0
    }

    scalingFactor * probability
  }

  case class FecundityModel(f: (Int) => Double, description: String) {
    override def toString: String = description
  }

  val fecundity = FecundityModel(baseProbabilityOfPregnancy, "Modern")

  case class CancerIncidenceAdjustment(increasedIncidence: Double) {
    override def toString: String = increasedIncidence.toString
  }

  def csvHeader: String = {
    s"pacAgeCenter," +
      s"brinkEffect," +
      s"CA Risk Adjustment," +
      s"Maternal Inheritance," +
      s"Fecundity Model," +
      s"Mortality Model," +
      s"Initial Population TL"
  }

  case class Options(pacAgeCenter: Option[Double],
                     maternalInheritance: Double,
                     brinkEffect: Boolean,
                     cancerIncidenceAdjustment: CancerIncidenceAdjustment,
                     allCauseMortalityForAge: AllCauseMortalityModel,
                     fecundityForAge: FecundityModel,
                     initialPopulationTL: Int) {
    assert(maternalInheritance >= 0.0 && maternalInheritance <= 1.0, "Maternal contribution must be between 0 and 1")
    assert(initialPopulationTL > 0, "Telomere length must be greater than zero")


    override def toString: String = {
      def withOrWithoutIndicator(x: Boolean): String = if (x) "(+)" else "(-)"

      def pacIndicator(ageCenter: Option[Double]): String = ageCenter.map(_.toString).getOrElse("")

      s"${pacIndicator(pacAgeCenter)}," +
        s"${withOrWithoutIndicator(brinkEffect)}," +
        s"$cancerIncidenceAdjustment," +
        f"$maternalInheritance%4.3f," +
        s"$fecundityForAge," +
        s"$allCauseMortalityForAge," +
        s"$initialPopulationTL"
    }
  }

}
