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
      //      val birthByYear = nextGeneration.map(x => x.deathYear - x.birthYear)
      //      val avgDeathAge = Try(birthByYear.sum / birthByYear.length).getOrElse(None)
      //      println(s"year= ${startYear}, fertile females = ${femalePopulation.size}, fertile males = ${malePopulation.size}, deathAge = ${avgDeathAge}")

      iterate(startYear + stepSize, stopYear, stepSize, population = nextGeneration union population)
    }
  }

  def main(args: Array[String]) {
    // Print CSV header
    println("trialNumber,year,avgNewbornTL,birthRate,populationCount,avgNewbornLifeExpectancy,deathRate")
    //Initialize Random number generator for reproducibility
    (1 to 20).foreach(trialNumber => {
      val randomSeed: Int = 0xdf2c9fb9 + trialNumber // Taken from truncated first commit hash, if curious
      Random.setSeed(randomSeed)

      val runLength = 200
      val population: List[Human] = iterate(startYear = 1, stopYear = runLength + 1, stepSize = 1,
        population = (for {
          i <- 1 to 1000
        } yield Child(father = Adam, mother = Eve, birthYear = 1)).toList)

      val result = (1 to runLength).map(year =>
        Vector(Some(year), {
          // average newborn TL
          val birthByYear = population.filter(_.birthYear == year).map(_.birthTL);
          Try(Some(birthByYear.sum / birthByYear.length)).getOrElse(None)
        },
          Some(population.count(_.birthYear == year)),
          Some(population.count(_.isAliveAtYear(year))), {
            // average newborn death age
            val birthByYear = population.filter(_.birthYear == year).map(x => x.deathYear - x.birthYear)
            Try(Some(birthByYear.sum / birthByYear.length)).getOrElse(None)
          }, {
            val populationSizeLastYear = population.count(_.isAliveAtYear(year - 1))
            val populationSizeThisYear = population.count(_.isAliveAtYear(year))
            val birthsThisYear = population.count(_.birthYear == year)
            val deathsThisYear = populationSizeLastYear - (populationSizeThisYear - birthsThisYear)
            Some(deathsThisYear)
          }
        )
      )

      // Print results as CSV
      println({
        result.map(line =>
          trialNumber +
            line.map(item =>
              s"${item.getOrElse("")}")
              .foldLeft("")(_ + "," + _))
          .foldLeft("")(_ + _ + "\n")
      })
    })

    System.exit(0)
  }

}
