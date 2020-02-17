package calc10free


import cats.Eq
import cats.tests.CatsSuite

import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.Discipline
import cats.implicits._
import cats.laws.discipline.MonadTests
import org.scalacheck.ScalacheckShapeless._
import org.typelevel.discipline.scalatest.FunSuiteDiscipline


// see: https://typelevel.org/cats/typeclasses/lawtesting.html
// cats documentation doesn't mention than Discipline doesn't implement the checkAll() method by default

class VersionedResultLawTests extends CatsSuite with FunSuiteDiscipline {
  // so that we don't need to define our own instances of Arbitrary

  implicit def eqTree[A: Eq]: Eq[VersionedResult[A]] = Eq.fromUniversalEquals

  // functor tests
  //  val tests = cats.laws.discipline.FunctorTests[VersionedResult].functor[Int, Int, String].all

  checkAll("VersionedResult.FunctorLaws", MonadTests[VersionedResult].monad[Int, Int, String])

  //  override def checkAll(name: String, ruleSet: Laws#RuleSet)(implicit config: VersionedResultLawTests.this.PropertyCheckConfiguration, prettifier: Prettifier, pos: Position): Unit = ???
}
