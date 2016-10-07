/**
  * Created by kent on 10/6/16.
  */

object JobGenerator {
  def main(args: Array[String]): Unit = {

    val models = for {
      pacEffect <- List(true, false)
      pacAgeCenter <- 25 to 35
      sexEffect <- List(false)
      tlDependentCancer <- List(true, false)
      cancerIncidenceAgeTLAdjustment <- List(true, false)
      maternalInheritance <- List(0.575)
      initialPopulationTL <- (7000 to 12000 by 100)
      runLength <- List(500)
      trialCount <- List(5)
    } yield s"$pacEffect $pacAgeCenter $sexEffect $tlDependentCancer $cancerIncidenceAgeTLAdjustment $maternalInheritance $initialPopulationTL $runLength $trialCount"

    object Counter {
      var x: Int = 0

      def next: Int = {
        x += 1
        x
      }
    }

    for (modelParameters <- models) yield {
      println(s"scala target/scala-2.11/pac_effect_2.11-1.0.jar $modelParameters model-${Counter.next}.csv")
    }

  }
}

