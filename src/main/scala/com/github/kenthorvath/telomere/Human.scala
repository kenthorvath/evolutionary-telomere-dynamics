package com.github.kenthorvath.telomere

import scala.util.Random

/**
  * Created by kent on 6/17/16.
  */

sealed trait Sex

case object Male extends Sex

case object Female extends Sex


trait Human {
  val modelOptions: Model.Options
  val sex: Sex
  val birthTL: Int

  val birthYear: Int
  assert(birthYear >= 0, "Birth year must be non-negative")

  val deathYear: Int
  val pregnancyAges: List[Int] = predictPregnancyAges(modelOptions)

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

  def isCapableOfMatingForYear(year: Int): Boolean = ageForYear(year) match {
    case n if n >= 13 && n <= 55 => true
    case _ => false
  }

  def isAliveAtYear(year: Int): Boolean = (year >= birthYear) && (year < deathYear)

  def baseProbabilityOfCancer(age: Int, modelOptions: Model.Options): Double = {
    def cancerIncidenceAfter20(age: Int): Double = {
      val c: Map[Sex, Int] = Map(Male -> 311, Female -> 178)
      val h: Map[Sex, Double] = Map(Male -> 5.50, Female -> 4.44)
      val r: Map[Sex, Double] = Map(Male -> 0.090, Female -> 0.063)
      val p2: Map[Sex, Double] = Map(Male -> 1.0e-9, Female -> 6.0e-9)
      val mcs20: Double = 1.0e8
      val tlCritical: Double = 6.5
      val tl20: Double = (LTLForYear(birthYear + 20).toFloat +
        modelOptions.cancerIncidenceAgeTLAdjustment.map(_.tlAdjustment).getOrElse(0)) / 1000
      assert(tl20 > 0, "LTL20 parameter cannot be negative for cancer incidence")
      val q: Double = 0.025
      val dTL = tl20 - tlCritical
      val ageAfter20 = age - 20 + modelOptions.cancerIncidenceAgeTLAdjustment.map(_.ageAdjustment).getOrElse(0)
      val incidencePer100K: Double =
        c(sex) * (mcs20 / (1 + math.exp(-h(sex) * (dTL - q * ageAfter20)))) * p2(sex) * math.exp(r(sex) * ageAfter20)
      incidencePer100K / 100e3
    }

    age match {
      case n if n <= 20 => 0.0
      case n if n > 20 => cancerIncidenceAfter20(age = n)
    }

  }

  def predictDeathYear(modelOptions: Model.Options): Int = {
    // This should always return a value because baseProbabilityOfDeath defaults to 1.00
    val deathAgeFromBrink: Int = Stream.from(1).find(age => LTLForYear(birthYear + age) <= 5500).get
    val deathAgeFromCancer: Option[Int] = modelOptions.tlDependentCancer match {
      case true => (1 to 100).find(age => Random.nextFloat() <= baseProbabilityOfCancer(age, modelOptions))
      case false => None
    }
    val deathAgeWithoutCancer: Int = Stream.from(0).find(age => Random.nextFloat() <= modelOptions.allCauseMortalityForAge.f(age)).get

    birthYear + List(
      deathAgeFromBrink,
      deathAgeFromCancer.getOrElse(deathAgeWithoutCancer),
      deathAgeWithoutCancer).min
  }

  def predictPregnancyAges(modelOptions: Model.Options): List[Int] = {
    val allAges = (0 until deathAge).filter(age => Random.nextFloat() <= modelOptions.fecundityForAge.f(age)).toList
    val nonConsecutiveAges: List[Int] =
      allAges
        .sorted
        .foldLeft(List[Int]())((acc, age) => if (!acc.contains(age - 1)) age :: acc else acc)
    nonConsecutiveAges
  }
}

case class Child(birthYear: Int, father: Human, mother: Human, modelOptions: Model.Options) extends Human {
  assert(birthYear > father.birthYear, "Child cannot be born before father")
  assert(birthYear > mother.birthYear, "Child cannot be born before mother")

  val sex: Sex = if (Random.nextBoolean()) Male else Female
  val tlStandardDeviation: Int = 700

  val baseTL: Int = ((1.0 - modelOptions.maternalInheritance) * father.birthTL +
    modelOptions.maternalInheritance * mother.birthTL).round.toInt
  val stochasticEffect: Int = math.round(Random.nextGaussian() * tlStandardDeviation).toInt
  val sexEffect: Int = if (sex == Female) 150 else 0
  val pacEffect: Int = father match {
    case Adam(_) => 0
    case _ => -15 * (modelOptions.pacAgeCenter - father.ageForYear(birthYear))
  }

  val birthTL: Int = baseTL + stochasticEffect +
    (if (modelOptions.sexEffect) sexEffect else 0) +
    (if (modelOptions.pacEffect) pacEffect else 0)


  val deathYear = predictDeathYear(modelOptions)

  override def ageForYear(year: Int): Int = year - birthYear

  override val pregnancyAges: List[Int] = predictPregnancyAges(modelOptions)
}

case class Adam(modelOptions: Model.Options) extends Human {
  override val sex = Male
  val birthYear = 0
  val birthTL = modelOptions.initialPopulationTL
  val deathYear = birthYear
}

case class Eve(modelOptions: Model.Options) extends Human {
  override val sex = Female
  val birthYear = 0
  val birthTL = modelOptions.initialPopulationTL
  val deathYear = birthYear
}
