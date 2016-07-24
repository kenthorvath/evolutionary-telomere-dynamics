/**
  * Created by kent on 6/17/16.
  */

import com.kenthorvath.telomere._

import scala.annotation.tailrec
import scala.util.{Random, Try}

object Simulator {

  @tailrec
  def iterate(startYear: Int, stopYear: Int, stepSize: Int, population: List[Human]): List[Human] = {
    if (startYear >= stopYear)
      population
    else {
      val femalePopulation = population
        .filter(_.sex == Female)
        .filter(_ isAliveAtYear startYear)
        .filter(_ hasChildAtYear startYear)
      val malePopulation = population
        .filter(_.sex == Male)
        .filter(_ isAliveAtYear startYear)
        .filter(_ isCapableOfMatingForYear startYear)
      val nextGeneration: List[Human] = femalePopulation
        .map(mother => Child(birthYear = startYear, father = Random.shuffle(malePopulation).head, mother = mother))
      iterate(startYear + stepSize, stopYear, stepSize, population = nextGeneration union population)
    }
  }

  def main(args: Array[String]) {
    val population: List[Human] = iterate(startYear = 1, stopYear = 300, stepSize = 1,
      population = (for {
        i <- 1 to 1000
      } yield Child(father = Adam, mother = Eve, birthYear = 0)).toList)

    val result = (0 to 300).map(year =>
      (year, {
        val birthByYear = population.filter(_.birthYear == year).map(_.birthTL);
        Try(birthByYear.sum / birthByYear.length).getOrElse(0)
      },
        population.count(_.birthYear == year),
        population.count(_.isAliveAtYear(year)))
    )
    println(result)

    System.exit(0)
  }

}
