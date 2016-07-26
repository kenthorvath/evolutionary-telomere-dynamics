/**
  * Created by kent on 7/24/16.
  */

import com.kenthorvath.telomere._

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}

object HumanSpecification extends Properties("Human") {

  property("diesDuringOrAfterBirth") = forAll { (birthYear: Int) =>
    (birthYear > 0 && birthYear <= 1e6) ==> {
      val a = Child(birthYear = birthYear, father = Adam, mother = Eve)
      a.deathYear >= a.birthYear
    }
  }

  property("onlyMaleOrFemale") = forAll { (birthYear: Int) =>
    (birthYear > 0 && birthYear <= 1e6) ==> {
      val a = Child(birthYear = birthYear, father = Adam, mother = Eve)
      a.sex == Male || a.sex == Female
    }
  }

  property("LTLDecreasesWithAge") = forAll { (birthYear: Int) =>
    (birthYear > 0 && birthYear < 1e6) ==> {
      val a = Child(birthYear = birthYear, father = Adam, mother = Eve)
      (a.birthYear to a.deathYear).forall(year => a.LTLForYear(year + 1) < a.birthTL)
    }
  }

  property("probabilityRangesBetweenZeroAndOne") = forAll { (birthYear: Int) =>
    (birthYear > 0 && birthYear <= 1e6) ==> {
      val a = Child(birthYear = birthYear, father = Adam, mother = Eve)
      (0 until a.deathAge).forall(age => {
        val pCancer = a.baseProbabilityOfCancer(age)
        val pDeath = a.baseProbabilityOfDeath(age)
        val pPregnancy = a.baseProbabilityOfPregnancy(age)

        0 <= pCancer && pCancer <= 1 &&
          0 <= pDeath && pDeath <= 1 &&
          0 <= pPregnancy && pPregnancy <= 1
      })
    }
  }

  property("pregnanciesAreNonConsecutive") = forAll { (birthYear: Int) =>
    (birthYear > 0 && birthYear < 1e6) ==> {
      val a = Child(birthYear = birthYear, father = Adam, mother = Eve)
      (a.birthYear to a.deathYear).forall(year => a.LTLForYear(year + 1) < a.birthTL)
    }
  }

}