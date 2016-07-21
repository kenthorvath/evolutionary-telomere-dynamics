/**
  * Created by kent on 6/17/16.
  */

import com.kenthorvath.telomere._
import scala.annotation.tailrec

object Simulator {

  @tailrec
  def iterate(startYear: Int, stopYear: Int, stepSize: Int, population: List[Human]): List[Human] = {
    if (startYear >= stopYear)
      population
    else {
      val nextGeneration: List[Human] = population
        .filter(_ hasChildAtYear startYear)
        .map(h => Child(birthYear = startYear, father = h, mother = Eve))
      iterate(startYear + stepSize, stopYear, stepSize, population = nextGeneration union population)
    }
  }

  def main(args: Array[String]) {
    val population: List[Human] = iterate(startYear = 1, stopYear = 100, stepSize = 1,
      population = List(Adam))
    val lastGeneration = population.groupBy(_.birthYear)
    val yearMeanTL = lastGeneration.map(x => (x._1, x._2.foldLeft(0: BigInt)((TL: BigInt, h: Human) => TL + h.birthTL) / x._2.size)).toList.sortBy(_._1)
    val yearBirthCount = lastGeneration.map(x => (x._1, x._2.size)).toList.sortBy(_._1)
    val result = yearMeanTL.zip(yearBirthCount).map(x => (x._1._1, x._1._2, x._2._2))
    println(result)
    System.exit(0)
  }
}
