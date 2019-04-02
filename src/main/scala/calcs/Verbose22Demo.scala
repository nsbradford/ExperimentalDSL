package calcs

import scala.reflect.runtime.universe._
//import scala.reflect.ClassTag
import scala.util.{Success, Try}
//import shapeless.tag.@@ // TODO add to build.sbt
import Verbose22Imports._

object ImplicitRepositories {
  implicit object DoubleHasRepository extends HasRepository[Double] {
    override def persist(t: VersionedDataUnpersisted[Double]): Try[Unit] = ???
    override def hydrate(version: _root_.calcs.Verbose22Imports.CalcVersionAssigned): Try[Double] = ???
    override def hydrateLatestValid(): Try[VersionedData[Double]] = Try{
      VersionedData[Double](0.5, CalcVersionAssigned("Double-Hydrator", 3423))
    }
  }
  implicit object IntHasRepository extends HasRepository[Int] {
    override def persist(t: VersionedDataUnpersisted[Int]): Try[Unit] = ???
    override def hydrate(version: CalcVersionAssigned): Try[Int] = ???
    override def hydrateLatestValid(): Try[VersionedData[Int]] = Try{
      VersionedData[Int](4, CalcVersionAssigned("Int-Hydrator", 3943))
    }
  }
  implicit object StringHasRepository extends HasRepository[String] {
    override def persist(t: VersionedDataUnpersisted[String]): Try[Unit] = Try {
      println(s"DataRepository > PERSIST > Version${t.version} had data ${t.data}")
    }
    override def hydrate(version: CalcVersionAssigned): Try[String] = ???
    override def hydrateLatestValid(): Try[VersionedData[String]] = ???
  }
}

class MockCalcRepository extends CalcRepository {

  override def requisitionNewRunId(calcName: String): Try[CalcVersionAssigned] = {
    calcIdCounter = calcIdCounter + 1
    Success(CalcVersionAssigned(calcName, calcIdCounter))
  }
  private var calcIdCounter = 0

  override def logInput[T : TypeTag](inputRecord: InputRecord[T]): Try[Unit] = Try {
    println(s"CalcRepository > log INPUT > $inputRecord of type: ${typeOf[T]}")
  }

  override def logOutput[T : TypeTag](outputRecord: OutputRecord[T]): Try[Unit] = ???
}

object Verbose22Demo extends App {

  import ImplicitRepositories._
  implicit val calcRepository: CalcRepository = new MockCalcRepository

  // first define calculator
  val demoCalc2 = Calc("Demo Calc2", (i: Int, d: Double) => (i * d).toString)

  // note that this for-comprehension is strictly over Try, not even bothering with IO/Future yet
  val finalVersionedResult: Try[VersionedData[String]] =
    for {
      versionedInt <- HasRepository[Int].hydrateLatestValid() // TODO want to run these two async
      versionedDouble <- HasRepository[Double].hydrateLatestValid()
      resultAgg: Agg2[Int, Double, String] = demoCalc2(versionedInt, versionedDouble)
      finalResult: VersionedData[String] <- Agg.persistAndReturn(resultAgg)
    } yield finalResult
}
