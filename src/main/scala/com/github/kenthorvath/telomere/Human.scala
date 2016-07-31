package com.github.kenthorvath.telomere

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
  assert(birthYear >= 0, "Birth year must be non-negative")

  val deathYear: Int
  val pregnancyAges: List[Int] = predictPregnancyAges

  def deathAge: Int = ageForYear(deathYear)

  def ageForYear(year: Int): Int = year - birthYear

  def LTLForYear(year: Int): Int = {
    val result = ageForYear(year) match {
      case n if n <= 20 => birthTL - 70 * n
      case n => birthTL - (70 * 20) - (25 * (n - 20))
    }
    assert(result >= 0, "LTL cannot be negative")
    result
  }

  def hasChildAtYear(year: Int): Boolean = {
    pregnancyAges.contains(ageForYear(year))
  }

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

  def isCapableOfMatingForYear(year: Int): Boolean = ageForYear(year) match {
    case n if n >= 13 && n <= 55 => true
    case _ => false
  }

  def isAliveAtYear(year: Int): Boolean = (year >= birthYear) && (year < deathYear)

  def baseProbabilityOfCancer(age: Int): Double = {
    def cancerIncidenceAfter20(age: Int): Double = {
      val c: Map[Sex, Int] = Map(Male -> 311, Female -> 178)
      val h: Map[Sex, Double] = Map(Male -> 5.50, Female -> 4.44)
      val r: Map[Sex, Double] = Map(Male -> 0.090, Female -> 0.063)
      val p2: Map[Sex, Double] = Map(Male -> 1.0e-9, Female -> 6.0e-9)
      val mcs20: Double = 1.0e8
      val tlCrit: Double = 6.5
      val tl20: Double = LTLForYear(birthYear + 20).toFloat / 1000
      assert(tl20 > 0, "LTL20 parameter cannot be negative for cancer incidence")
      val q: Double = 0.025
      val dTL = tl20 - tlCrit
      val ageAfter20 = age - 20
      val incidencePer100K: Double =
        c(sex) * (mcs20 / (1 + math.exp(-h(sex) * (dTL - q * ageAfter20)))) * p2(sex) * math.exp(r(sex) * ageAfter20)
      incidencePer100K / 100e3
    }

    age match {
      case n if n <= 20 => 0.0
      case n if n > 20 => cancerIncidenceAfter20(n)
    }

  }

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
    // This should always return a value because baseProbabilityOfDeath defaults to 1.00
    val deathAgeFromBrink: Int = Stream.from(1).find(age => LTLForYear(birthYear + age) <= 5500).get
    val deathAgeFromCancer: Option[Int] = (0 to 100).find(age => Random.nextFloat() <= baseProbabilityOfCancer(age))
    val deathAgeWithoutCancer: Int = Stream.from(0).find(age => Random.nextFloat() <= baseProbabilityOfDeath(age)).get

    birthYear + List(
      deathAgeFromBrink,
      deathAgeFromCancer.getOrElse(deathAgeWithoutCancer),
      deathAgeWithoutCancer).min
  }

  def predictPregnancyAges: List[Int] = {
    val allAges = (0 until deathAge).filter(age => Random.nextFloat() <= baseProbabilityOfPregnancy(age)).toList
    val nonConsecutiveAges: List[Int] =
      allAges
        .sorted
        .foldLeft(List[Int]())((acc, age) => if (!acc.contains(age - 1)) age :: acc else acc)
    nonConsecutiveAges
  }
}

case class Child(birthYear: Int, father: Human, mother: Human) extends Human {
  assert(birthYear > father.birthYear, "Child cannot be born before father")
  assert(birthYear > mother.birthYear, "Child cannot be born before mother")

  val sex: Sex = if (Random.nextBoolean()) Male else Female

  val baseTL: Int = (father.birthTL + mother.birthTL) / 2
  val stochasticEffect: Int = math.round(Random.nextGaussian() * 700).toInt
  val sexEffect: Int = if (sex == Female) 150 else 0
  val pacEffect: Int = father match {
    case Adam => 0
    case _ => -15 * (55 - father.ageForYear(birthYear))
  }

  val birthTL: Int = baseTL + stochasticEffect

  val deathYear = predictDeathYear

  override def ageForYear(year: Int): Int = year - birthYear

  override val pregnancyAges: List[Int] = predictPregnancyAges
}

case object Adam extends Human {
  override val sex = Male
  val birthYear = 0
  val birthTL = 9500
  val deathYear = birthYear
}

case object Eve extends Human {
  override val sex = Female
  val birthYear = 0
  val birthTL = 9500
  val deathYear = birthYear
}
