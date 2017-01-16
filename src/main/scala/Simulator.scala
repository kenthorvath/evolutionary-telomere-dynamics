/**
  * Created by kent on 6/17/16.
  */

import com.github.kenthorvath.telomere._

import scala.annotation.tailrec
import scala.util.{Random, Try}
import java.io._

import breeze.linalg._
import breeze.stats._

import com.github.kenthorvath.telomere.Model.{CancerIncidenceAdjustment, Options}

object Simulator {

  @tailrec
  def iterate(startYear: Int, stopYear: Int, population: List[Human], modelOptions: Options): List[Human] = {

    if (startYear % 50 == 0)
      println(startYear)
    else
      print("")

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

      val nextGeneration: List[Human] = femalePopulation.par
        .flatMap(mother => Try(List(Child(birthYear = startYear, father = Random.shuffle(malePopulation).head,
          mother = mother, modelOptions = modelOptions))).getOrElse(Nil)).toList

      iterate(startYear + 1, stopYear, population = nextGeneration union population, modelOptions)
    }
  }

  def main(args: Array[String]) {


    val model = {
      val pacEffect = args(0).toBoolean
      val pacAgeCenter = args(1).toDouble
      val tlDependentCancer = args(2).toBoolean

      val cancerIncidenceScalingFactor = args(3).toDouble
      val cancerIncidenceAgeTLAdjustment = CancerIncidenceAdjustment(increasedIncidence = cancerIncidenceScalingFactor)

      val maternalInheritance = args(4).toFloat
      val allCauseMortalityForAge = Model.mortality
      val fecundityForAge = Model.fecundity
      val initialPopulationTL = args(5).toInt //(7000 to 12000 by 1000) ++ (9100 to 9900 by 100)

      Model.Options(pacEffect = pacEffect, pacAgeCenter = pacAgeCenter, maternalInheritance = maternalInheritance, tlDependentCancer = tlDependentCancer, cancerIncidenceAdjustment = cancerIncidenceAgeTLAdjustment, allCauseMortalityForAge = allCauseMortalityForAge, fecundityForAge = fecundityForAge, initialPopulationTL = initialPopulationTL)
    }


    val pw = new PrintWriter(new File(args(8)))
    // PrintWriter
    // Write CSV header
    pw.write(s"trialNumber,year,avgNewbornTL,stddevNewbornTL,Q1NewbornTL,Q2NewbornTL,Q3NewbornTL,birthRate,populationCount,avgNewbornLifeExpectancy,deathRate,${Model.csvHeader}\n")

    println(Model.csvHeader)
    println(model)
    //Initialize Random number generator for reproducibility
    val results = (1 to args(7).toInt).flatMap(trialNumber => {
      val randomSeed: Int = 0xdf2c9fb9 + trialNumber // Taken from truncated first commit hash, if curious
      Random.setSeed(randomSeed)
      println(s"Trial $trialNumber")

      val runLength = args(6).toInt

      val seedPopulation: List[Human] = {
        for {
          i <- 1 to 100
          year <- -100 to 0
        }
          yield Child(father = MaleFounder(modelOptions = model),
            mother = FemaleFounder(modelOptions = model),
            birthYear = year, modelOptions = model)
      }.toList.filter(_.isAliveAtYear(0))

      val initialPopulation: List[Human] = Random.shuffle(seedPopulation).take(1000)

      val resultPopulation: List[Human] = iterate(startYear = 1, stopYear = runLength + 1, population = initialPopulation, modelOptions = model)

      val trialResult = (1 to runLength).map(year => {
        val newbornTLByYear = resultPopulation.filter(_.birthYear == year).map(_.birthTL.toDouble).toArray

        val result: Vector[Option[Any]] = Vector(
          Some(trialNumber),
          Some(year), {
            // mean newborn TL
            Try(Some(mean(newbornTLByYear))).getOrElse(None)
          }, {
            // std newborn TL
            Try(Some(stddev(newbornTLByYear))).getOrElse(None)
          }, {
            // Q1 newborn TL
            Try(Some(DescriptiveStats.percentile(newbornTLByYear, 0.25))).getOrElse(None)
          }, {
            // Q2 newborn TL
            Try(Some(DescriptiveStats.percentile(newbornTLByYear, 0.5))).getOrElse(None)
          }, {
            // Q3 newborn TL
            Try(Some(DescriptiveStats.percentile(newbornTLByYear, 0.75))).getOrElse(None)
          },
          Some(resultPopulation.count(_.birthYear == year)),
          Some(resultPopulation.count(_.isAliveAtYear(year))), {
            // average newborn death age
            val deathAges = resultPopulation.filter(_.birthYear == year).map(x => x.deathYear - x.birthYear).map(_.toDouble).toArray
            Try(Some(mean(deathAges))).getOrElse(None)
          }, {
            val populationSizeLastYear = resultPopulation.count(_.isAliveAtYear(year - 1))
            val populationSizeThisYear = resultPopulation.count(_.isAliveAtYear(year))
            val birthsThisYear = resultPopulation.count(_.birthYear == year)
            val deathsThisYear = populationSizeLastYear - (populationSizeThisYear - birthsThisYear)
            Some(deathsThisYear)
          },
          Some(model.toString)
        )
        result.toArray
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

    pw.close()
    System.exit(0)
  }

}
