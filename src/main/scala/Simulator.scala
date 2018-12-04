/**
  * Simulator
  * ---------
  *
  * Parameters: <pacAgeCenter> <crossOverYear> <brinkEffect> <cancerIncidenceAdjustment> <maternalInheritance> <initialPopulationTL> <runLength> <numberOfTrials> <outputFileName>
  *
  * - pacAgeCenter (None or a Double): PAC in years above which TL is added, below which subtracted
  * - crossOverYear (None or an Int): Year that PAC model is switched on during simulation
  * - brinkEffect (Boolean): Whether or not to include a brink effect in the model
  * - cancerIncidenceAdjustment (Double): Scaling factor for cancer incidence (0.0 -> cancer incidence)
  * - maternalInheritance (Double): Weight of inherited maternal birth TL (range -> 0.0 to 1.0)
  * - initialPopulationTL (Int): Given in base pairs
  * - runLength (Int): Given in years
  * - numberOfTrials (Int): Must be greater than 1
  * - outputFileName (String): path to write CSV output
  *
  * Author: Kent Horvath, MD PhD
  * Date: October 6, 2016
  * License: MIT
  */

import java.io._

import breeze.stats.DescriptiveStats.percentile
import breeze.stats.{mean, stddev}
import com.github.kenthorvath.telomere.Model.{CancerIncidenceAdjustment, Options}
import com.github.kenthorvath.telomere._

import scala.annotation.tailrec
import scala.util.{Random, Try}

object Simulator {

  @tailrec
  def iterate(currentYear: Int, stopYear: Int, population: List[Human], modelOptions: List[(Int, Options)]): List[Human] = {


    if (currentYear >= stopYear)
      population
    else {
      val femalePopulation: List[Human] = population
        .filter(_.sex == Female)
        .filter(_ isAliveAtYear currentYear)
        .filter(_ hasChildAtYear currentYear)
      val malePopulation: List[Human] = population
        .filter(_.sex == Male)
        .filter(_ isAliveAtYear currentYear)
        .filter(_ isCapableOfMatingForYear currentYear)

      // Find the first option effective on or after this year
      val currentModel = modelOptions
        .sortBy({ case (effectiveYear, _) => effectiveYear })(Ordering.fromLessThan(_ > _))
        .find({ case (effectiveYear, _) => currentYear >= effectiveYear })
        .map({ case (_, options) => options })
        .get

      if (currentYear % 50 == 0) {
        println(s"currentYear=$currentYear, executionPlan=$modelOptions, pacEffect=${currentModel.pacAgeCenter}")
      }
      else ()

      // Note: Using `par` here greatly speeds up the simulation, at the expense of deterministic output
      // because the sequence in which elements are processed is a function of the state of the hardware and available
      // resources during runtime.
      // TODO: Use a clever functional programming pattern to make this deterministic under parallelism.
      val nextGeneration: List[Human] = femalePopulation.par
        .flatMap(mother => Try(List(Child(birthYear = currentYear, father = Random.shuffle(malePopulation).head,
          mother = mother, modelOptions = currentModel))).getOrElse(Nil)).toList

      iterate(currentYear + 1, stopYear, population = nextGeneration union population, modelOptions)
    }
  }

  def main(args: Array[String]) {

    def maybe[T](f: => T): Option[T] = Try(Some(f)).getOrElse(None)

    val model = {
      val pacAgeCenter: Option[Double] = maybe(args(0).toDouble)
      val brinkEffect = args(2).toBoolean

      val cancerIncidenceScalingFactor = args(3).toDouble
      val cancerIncidenceAgeTLAdjustment = CancerIncidenceAdjustment(increasedIncidence = cancerIncidenceScalingFactor)

      val maternalInheritance = args(4).toFloat
      val allCauseMortalityForAge = Model.mortality
      val fecundityForAge = Model.fecundity
      val initialPopulationTL = args(5).toInt //(7000 to 12000 by 1000) ++ (9100 to 9900 by 100)

      Model.Options(pacAgeCenter = pacAgeCenter,
        maternalInheritance = maternalInheritance,
        brinkEffect = brinkEffect,
        cancerIncidenceAdjustment = cancerIncidenceAgeTLAdjustment,
        allCauseMortalityForAge = allCauseMortalityForAge,
        fecundityForAge = fecundityForAge,
        initialPopulationTL = initialPopulationTL)
    }

    val crossOverYear: Option[Int] = maybe(args(1).toInt)

    val modelOptionsExecutionPlan = crossOverYear.map { year =>
      // Before the crossover, PAC effect is always None
      val preCrossOverModel = model.copy(pacAgeCenter = None)
      val postCrossOverModel = model
      List((0, preCrossOverModel), (year, postCrossOverModel))
    }
      .getOrElse(List((0, model)))

    println(s"Execution plan is: $modelOptionsExecutionPlan")

    val pw = new PrintWriter(new File(args(8)))
    // PrintWriter
    // Write CSV header
    pw.write(s"trialNumber,year,avgNewbornTL,stdDevNewbornTL,Q1NewbornTL,Q2NewbornTL,Q3NewbornTL,birthRate,populationCount,avgNewbornLifeExpectancy,deathRate,${Model.csvHeader}\n")

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
          _ <- 1 to 100
          year <- -100 to 0
        }
          yield {
            val pacFreeModel = model.copy(pacAgeCenter = None)
            Child(father = MaleFounder(modelOptions = pacFreeModel),
              mother = FemaleFounder(modelOptions = pacFreeModel),
              birthYear = year, modelOptions = pacFreeModel)
          }
      }.toList.filter(_.isAliveAtYear(0))


      val preCrossOverInitialPopulation: List[Human] = Random.shuffle(seedPopulation).take(1000)

      val resultPopulation: List[Human] = iterate(currentYear = 1,
        stopYear = runLength + 1,
        population = preCrossOverInitialPopulation,
        modelOptions = modelOptionsExecutionPlan)

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
            Try(Some(percentile(newbornTLByYear, 0.25))).getOrElse(None)
          }, {
            // Q2 newborn TL
            Try(Some(percentile(newbornTLByYear, 0.5))).getOrElse(None)
          }, {
            // Q3 newborn TL
            Try(Some(percentile(newbornTLByYear, 0.75))).getOrElse(None)
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
          // TODO: rethink how this works when there are three or more models in the execution plan
          // TODO: for now, just report the postCrossOverModel (i.e. model), as it's simpler for analysis purposes.
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
