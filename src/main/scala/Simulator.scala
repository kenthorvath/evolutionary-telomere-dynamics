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

    if (startYear % 50 == 0)
      println(startYear)
    else
      print("")

    val adjustedModelOptions = modelOptions
    //      if (startYear > 200)
    //      modelOptions.copy(cancerIncidenceAgeTLAdjustment = Some(CancerIncidenceAgeAdjustment(10,0,"+10y")))
    //    else
    //      modelOptions

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
          mother = mother, modelOptions = adjustedModelOptions))).getOrElse(Nil))

      iterate(startYear + stepSize, stopYear, stepSize, population = nextGeneration union population, adjustedModelOptions)
    }
  }

  def main(args: Array[String]) {


    val model = {
      val pacEffect = args(0).toBoolean
      val pacAgeCenter = args(1).toInt
      val sexEffect = args(2).toBoolean
      val tlDependentCancer = args(3).toBoolean
      val cancerIncidenceAgeTLAdjustment =

        if (args(4).toBoolean) {
          None
        } else {
          Some(CancerIncidenceAgeAdjustment(ageAdjustment = 10, tlAdjustment = 0, "+10y"))
        }
      val maternalInheritance = args(5).toFloat
      val allCauseMortalityForAge = Model.archaicMortality
      val fecundityForAge = Model.archaicFecundity
      val initialPopulationTL = args(6).toInt //(7000 to 12000 by 1000) ++ (9100 to 9900 by 100)

      Model.Options(pacEffect = pacEffect, pacAgeCenter = pacAgeCenter, sexEffect = sexEffect, maternalInheritance = maternalInheritance,
        tlDependentCancer = tlDependentCancer, cancerIncidenceAgeTLAdjustment = cancerIncidenceAgeTLAdjustment,
        allCauseMortalityForAge = allCauseMortalityForAge, fecundityForAge = fecundityForAge, initialPopulationTL = initialPopulationTL)
    }


    val pw = new PrintWriter(new File(args(9)))
    // PrintWriter
    // Write CSV header
    pw.write(s"trialNumber,year,avgNewbornTL,birthRate,populationCount,avgNewbornLifeExpectancy,deathRate,${Model.csvHeader}\n")

    println(Model.csvHeader)
    println(model)
    //Initialize Random number generator for reproducibility
    val results = (1 to args(8).toInt).flatMap(trialNumber => {
      val randomSeed: Int = 0xdf2c9fb9 + trialNumber // Taken from truncated first commit hash, if curious
      Random.setSeed(randomSeed)
      println(s"Trial $trialNumber")

      val runLength = args(7).toInt
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

    pw.close
    System.exit(0)
  }

}
