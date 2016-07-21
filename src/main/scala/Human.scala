package com.kenthorvath.telomere

import scala.util.Random

/**
  * Created by kent on 6/17/16.
  */
sealed trait Sex

case object Male extends Sex

case object Female extends Sex


abstract class Human {
  val sex: Sex
  val birthTL: Int
  val birthYear: Int

  val deathYear: Int = predictDeathYear
  val pregnancyAges: List[Int] = predictPregnancyAges

  def ageForYear(year: Int): Int = year - birthYear

  def LTLForYear(year: Int): Int = sex match {
    //Citation: Hum Mol Gen. 2007
    case Male => birthTL - 31 * ageForYear(year)
    case Female => birthTL - 21 * ageForYear(year)
  }

  def hasChildAtYear(year: Int): Boolean = {
    pregnancyAges.contains(ageForYear(year))
  }

  def baseProbabilityOfPregnancy(age: Int): Double = age match {
    //Citation: DOI: 10.1002/ajpa.22495
    case n if n <= 10 => 0.0
    case n if n <= 15 => 0.01
    case n if n <= 20 => 0.15
    case n if n <= 25 => 0.25
    case n if n <= 35 => 0.28
    case n if n <= 40 => 0.25
    case n if n <= 45 => 0.15
    case n if n <= 50 => 0.08
    case n if n <= 55 => 0.01
    case _ => 0.0
  }

  def hasReachedSexualMaturityByYear(year: Int): Boolean = year match {
    case n if n >= 15 => true
    case _ => false
  }

  def isAliveAtYear(year: Int): Boolean = (year >= birthYear) && (year < deathYear)

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

  def predictDeathYear: Int = {
    // This should always return a value because probabilityOfDeath defaults to 1.00 by age 85
    val deathAge: Int = (0 to 100).find(age => Random.nextFloat() <= baseProbabilityOfDeath(age)).get
    birthYear + deathAge
  }

  def predictPregnancyAges: List[Int] = {
    val allAges = (0 until deathYear).filter(age => Random.nextFloat() <= baseProbabilityOfPregnancy(age)).toList
    val nonConsecutiveAges: List[Int] =
      allAges
        .sorted
        .foldLeft(List[Int]())((acc, age) => if (!acc.contains(age - 1)) age :: acc else acc)
    nonConsecutiveAges
  }

}

case class Child(birthYear: Int, father: Human, mother: Human) extends Human {
  val sex = if (Random.nextBoolean()) Male else Female
  val baseTL: Int = (father.birthTL + mother.birthTL) / 2
  val stochasticEffect: Int = math.round(Random.nextGaussian() * 200).toInt
  val pacEffect: Int = 15 * father.ageForYear(birthYear)
  val birthTL: Int = baseTL + stochasticEffect + pacEffect
}

case object Adam extends Human {
  val sex = Male
  val birthYear = 0
  val birthTL = 10000
}

case object Eve extends Human {
  val sex = Female
  val birthYear = 0
  val birthTL = 10000
}
