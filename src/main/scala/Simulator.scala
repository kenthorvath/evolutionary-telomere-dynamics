/**
  * Created by kent on 6/17/16.
  */

import com.github.kenthorvath.telomere._

import scala.annotation.tailrec
import scala.util.{Random, Try}
import java.io._

import com.github.kenthorvath.telomere.Model.CancerIncidenceAgeAdjustment

object Simulator {

  @tailrec
  def iterate(startYear: Int, stopYear: Int, stepSize: Int, population: List[Human], modelOptions: Model.Options): List[Human] = {
    if (startYear >= stopYear)
      population
    else {
      val femalePopulation: List[Human] = population
        .filter(_.sex == Female)
        .filter(_ isAliveAtYear startYear)
        .filter(_ hasChildAtYear startYear)
      val malePopulation: List[Human] = population
        .filter(_.sex == Male)
        .filter(_ isAliveAtYear startYear)
        .filter(_ isCapableOfMatingForYear startYear)

      val nextGeneration: List[Human] = femalePopulation
        .flatMap(mother => Try(List(Child(birthYear = startYear, father = Random.shuffle(malePopulation).head,
          mother = mother, modelOptions = modelOptions))).getOrElse(Nil))

      iterate(startYear + stepSize, stopYear, stepSize, population = nextGeneration union population, modelOptions)
    }
  }

  def main(args: Array[String]) {


    val models = for {
      pacEffect <- List(false)
//      pacAgeCenter <- 29 to 33 by 1 if pacEffect
      pacAgeCenter <- List(31)
      sexEffect <- List(false)
      tlDependentCancer <- List(true, false)
      cancerIncidenceAgeTLAdjustment <- {
        List(None,
          Some(CancerIncidenceAgeAdjustment(ageAdjustment = 10, tlAdjustment = 0, "+10y"))
        )
      }
      maternalInheritance <- List(0.575)
      allCauseMortalityForAge <- List(Model.archaicMortality)
      fecundityForAge <- List(Model.archaicFecundity)
      initialPopulationTL <- 9000 to 15000 by 1000
    } yield
      Model.Options(pacEffect = pacEffect, pacAgeCenter = pacAgeCenter, sexEffect = sexEffect, maternalInheritance = maternalInheritance,
        tlDependentCancer = tlDependentCancer, cancerIncidenceAgeTLAdjustment = cancerIncidenceAgeTLAdjustment,
        allCauseMortalityForAge = allCauseMortalityForAge, fecundityForAge = fecundityForAge, initialPopulationTL = initialPopulationTL)


    val pw = new PrintWriter(new File("20160817-A.csv"))
    // PrintWriter
    // Write CSV header
    pw.write(s"trialNumber,year,avgNewbornTL,birthRate,populationCount,avgNewbornLifeExpectancy,deathRate,${Model.csvHeader}\n")

    for {model <- models} yield {
      println(Model.csvHeader)
      println(model)
      //Initialize Random number generator for reproducibility
      val results = (1 to 2).flatMap(trialNumber => {
        val randomSeed: Int = 0xdf2c9fb9 + trialNumber // Taken from truncated first commit hash, if curious
        Random.setSeed(randomSeed)
        println(s"Trial $trialNumber")

        val runLength = 300
        val initialPopulation: List[Human] = {
          for {i <- 1 to 1000}
            yield Child(father = Adam(modelOptions = model),
              mother = Eve(modelOptions = model),
              birthYear = 1, modelOptions = model)
        }.toList

        val resultPopulation: List[Human] = iterate(startYear = 1, stopYear = runLength + 1, stepSize = 1,
          population = initialPopulation, modelOptions = model)

        val trialResult = (1 to runLength).map(year => {
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
        }
        )
        trialResult
      }
      )

      // Write results as CSV
      pw.write({
        results.map(line =>
          line.head.getOrElse("") +
            line.tail.map(item =>
              s"${item.getOrElse("")}")
              .foldLeft("")(_ + "," + _))
          .foldLeft("")(_ + _ + "\n")
      })
    }

    pw.close
    System.exit(0)
  }

}
