/**
  * Created by kent on 6/17/16.
  */
sealed trait Sex
case object Male extends Sex
case object Female extends Sex

abstract class Human {
  val sex: Sex
  val birthTL: Int //Telomere length at birth
  val birthYear: Int

  def ageForYear(year: Int): Int = year - birthYear
  def LTLForYear(year: Int): Int = birthTL - 50 * ageForYear(year)
}

case class Child(birthYear: Int, sex: Sex, father: Human, mother: Human) extends Human {
  val birthTL: Int = father.birthTL
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
