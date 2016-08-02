/**
  * Created by kent on 6/17/16.
  */

import com.github.kenthorvath.telomere._

import scala.annotation.tailrec
import scala.util.{Random, Try}

object Simulator {

  @tailrec
  def iterate(startYear: Int, stopYear: Int, stepSize: Int, population: List[Human], modelOptions: Model.Options): List[Human] = {
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
        .map(mother => Child(birthYear = startYear, father = Random.shuffle(malePopulation).head,
          mother = mother, modelOptions = modelOptions))

      iterate(startYear + stepSize, stopYear, stepSize, population = nextGeneration union population, modelOptions)
    }
  }

  def main(args: Array[String]) {


    val models = for {
      pacEffect <- List(true, false)
      sexEffect <- List(true, false)
      tlDependentCancer <- List(true, false)
      cancerIncidenceAgeTLAdjustment <- List(None)
      maternalInheritance <- List(0.5, 0.65)
      allCauseMortalityForAge <- List(Model.archaicMortality)
      fecundityForAge <- List(Model.archaicFecundity)
      initialPopulationTL <- 8000 to 15000 by 500
    } yield
      Model.Options(pacEffect = pacEffect, sexEffect = sexEffect, maternalInheritance = maternalInheritance,
        tlDependentCancer = tlDependentCancer, cancerIncidenceAgeTLAdjustment = cancerIncidenceAgeTLAdjustment,
        allCauseMortalityForAge = allCauseMortalityForAge, fecundityForAge = fecundityForAge, initialPopulationTL = initialPopulationTL)

    for {model <- models} yield {
      println(Model.csvHeader)
      println(model)
      //Initialize Random number generator for reproducibility
      val results = (1 to 50).flatMap(trialNumber => {
        val randomSeed: Int = 0xdf2c9fb9 + trialNumber // Taken from truncated first commit hash, if curious
        Random.setSeed(randomSeed)
        println(s"Trial $trialNumber")

        val runLength = 200
        val initialPopulation: List[Human] = {
          for {i <- 1 to 1000}
            yield Child(father = Adam(modelOptions = model),
              mother = Eve(modelOptions = model),
              birthYear = 1, modelOptions = model)
        }.toList

        val resultPopulation: List[Human] = iterate(startYear = 1, stopYear = runLength + 1, stepSize = 1,
          population = initialPopulation, modelOptions = model)

        val trialResult = (1 to runLength).map(year =>
          Vector(
            Some(trialNumber),
            Some(year), {
              // average newborn TL
              val birthByYear = resultPopulation.filter(_.birthYear == year).map(_.birthTL)
              Try(Some(birthByYear.sum / birthByYear.length)).getOrElse(None)
            },
            Some(resultPopulation.count(_.birthYear == year)),
            Some(resultPopulation.count(_.isAliveAtYear(year))), {
              // average newborn death age
              val birthByYear = resultPopulation.filter(_.birthYear == year).map(x => x.deathYear - x.birthYear)
              Try(Some(birthByYear.sum / birthByYear.length)).getOrElse(None)
            }, {
              val populationSizeLastYear = resultPopulation.count(_.isAliveAtYear(year - 1))
              val populationSizeThisYear = resultPopulation.count(_.isAliveAtYear(year))
              val birthsThisYear = resultPopulation.count(_.birthYear == year)
              val deathsThisYear = populationSizeLastYear - (populationSizeThisYear - birthsThisYear)
              Some(deathsThisYear)
            },
            Some(model.toString)
          )
        )
        trialResult
      }
      )

      // Print results as CSV
      // Print CSV header
      println(s"trialNumber,year,avgNewbornTL,birthRate,populationCount,avgNewbornLifeExpectancy,deathRate,${Model.csvHeader}")
      println({
        results.map(line =>
          line.head.getOrElse("") +
            line.tail.map(item =>
              s"${item.getOrElse("")}")
              .foldLeft("")(_ + "," + _))
          .foldLeft("")(_ + _ + "\n")
      })
    }

    System.exit(0)
  }

}
