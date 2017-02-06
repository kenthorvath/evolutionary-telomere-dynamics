/**
  * Created by kent on 10/6/16.
  */

object JobGenerator {
  def main(args: Array[String]): Unit = {

    if (args.length < 3) {
      throw new Exception("usage: <runLength> <trialCount> <bucket-name>")
    }
    else {
      ()
    }

    val models = for {
      brinkEffect <- List(true)
      pacAgeCenter: String <- List("None") ++ ((15 to 55 by 5).map(_.toDouble) ++ (30 to 40 by 1).map(_.toDouble) ++ (34.0 to 35.0 by 0.1)).map(_.toString).toSet
      cancerIncidenceAdjustment <- List(0) ++ ((0 to 2) map (math.pow(2, _)))
      maternalInheritance <- List(0.575)
      initialPopulationTL <- (7000 to 8000 by 1000) ++ (9000 to 11000 by 250) ++ (12000 to 13000 by 1000)
    } yield s"$pacAgeCenter $brinkEffect $cancerIncidenceAdjustment $maternalInheritance $initialPopulationTL ${args(0)} ${args(1)}"

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
      println(s"scala -cp ../target/scala-2.11/pac-effect-assembly-1.0.jar Simulator $modelParameters ../sbatch-output/model-$counter.csv &&")
      println(s"aws s3 cp ../sbatch-output/model-$counter.csv s3://telomere-dynamics/${args(2)}/")
      println(s"rm -f ../sbatch-output/model-$counter.csv")
    }

  }
}

