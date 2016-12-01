/**
  * Created by kent on 10/6/16.
  */

object JobGenerator {
  def main(args: Array[String]): Unit = {

    if (args.length < 2) {
      throw new Exception("usage: <runLength> <trialCount>")
    }
    else {
      ()
    }

    val models = for {
      pacEffect <- List(true, false)
      pacAgeCenter <- ((15 to 55 by 5) ++ (30 to 40 by 1) ++ (34.0 to 35.0 by 0.1)).toSet
      sexEffect <- List(false)
      tlDependentCancer <- List(true, false)
      cancerIncidenceAdjustment <- (0 to 4) map (math.pow(2, _))
      maternalInheritance <- List(0.575)
      initialPopulationTL <- (7000 to 8000 by 1000) ++ (9000 to 11000 by 250) ++ (12000 to 13000 by 1000)
    } yield s"$pacEffect $pacAgeCenter $sexEffect $tlDependentCancer $cancerIncidenceAdjustment $maternalInheritance $initialPopulationTL ${args(0)} ${args(1)}"

    object Counter {
      var x: Int = 0

      def next: Int = {
        x += 1
        x
      }
    }

    for (modelParameters <- models) yield {
      val counter: Int = Counter.next
      println(s"#!/bin/sh")
      println(s"scala -cp ../target/scala-2.11/pac_effect_2.11-1.0.jar Simulator $modelParameters ../sbatch-output/model-${counter}.csv &&")
      println(s"aws s3 cp ../sbatch-output/model-${counter}.csv")
      println(s"rm ../sbatch-output/model-${counter}.csv")
    }

  }
}

