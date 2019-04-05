package calcs

import SharedModel._
import scala.util.{Failure, Success, Try}
import Verbose22Imports._

object ImplicitRepositories {

  // Scalac apparently has weird semantics with implicit objects so use implicit vals instea
  implicit val DoubleHasRepository: HasRepository[Double] = new HasRepository[Double] {
    override def dataConceptName = "Demo Double"
    override def persist(t: VersionedDataUnpersisted[Double]): Try[Unit] = Try {
      println(s"DataRepository > PERSIST > $dataConceptName >Version${t.version} with output data {${t.data}}")
    }
    override def hydrate(version: CalcVersionAssigned): Try[Double] = ???
    override def hydrateLatestValid(): Try[VersionedData[Double]] = Try{
      VersionedData[Double](0.5, CalcVersionAssigned(CalcName("Double-Hydrator"), 3423))
    }
  }
  implicit val IntHasRepository: HasRepository[Int] = new HasRepository[Int] {
    override def dataConceptName = "Demo Int"
    override def persist(t: VersionedDataUnpersisted[Int]): Try[Unit] = ???
    override def hydrate(version: CalcVersionAssigned): Try[Int] = ???
    override def hydrateLatestValid(): Try[VersionedData[Int]] = Try{
      VersionedData[Int](4, CalcVersionAssigned(CalcName("Int-Hydrator"), 3943))
    }
  }
  implicit val StringHasRepository: HasRepository[String] = new HasRepository[String] {
    override def dataConceptName = "Demo String"
    override def persist(t: VersionedDataUnpersisted[String]): Try[Unit] = Try {
      println(s"DataRepository > PERSIST > $dataConceptName >Version${t.version} with output data {${t.data}}")
    }
    override def hydrate(version: CalcVersionAssigned): Try[String] = ???
    override def hydrateLatestValid(): Try[VersionedData[String]] = ???
  }
}

class MockCalcRepository extends CalcRepository {

  override def requisitionNewRunId(calcName: CalcName): Try[CalcVersionAssigned] = {
    calcIdCounter = calcIdCounter + 1
    Success(CalcVersionAssigned(calcName, calcIdCounter))
  }
  private var calcIdCounter = 0

  override def logInput[T : HasRepository](inputRecord: InputRecord[T]): Try[Unit] = Try {
    println(s"CalcRepository > log INPUT > $inputRecord of type: {${HasRepository[T].fullName}}")
  }

  override def logOutput[T : HasRepository](outputRecord: OutputRecord[T]): Try[Unit] = Try {
    println(s"CalcRepository > log OUTPUT > $outputRecord of type: {${HasRepository[T].fullName}}")
  }
}

object Verbose22Demo extends App {

  import ImplicitRepositories._
  implicit val calcRepository: CalcRepository = new MockCalcRepository

  val demoCalc1 = Calc("DemoCalc1-Doublex100", (d: Double) => d * 100d)
  val demoCalc2 = Calc("DemoCalc2-CombineIntDouble", (i: Int, d: Double) => (i * d).toString)


  // note that this for-comprehension is strictly over Try, not even bothering with IO/Future yet
  val finalVersionedResult: Try[VersionedData[String]] =
    for {
      versionedDouble <- HasRepository[Double].hydrateLatestValid()

      intermediateResultAgg: RawResult1[Double, Double] = demoCalc1(versionedDouble)
      intermediateResult: VersionedData[Double] <- RawResult.persistAndReturn(intermediateResultAgg) // implicit HasRepository for R
      _ <- intermediateResultAgg.logInputs(intermediateResult.version) // implicit HasRepository for T1, T2

      versionedInt <- HasRepository[Int].hydrateLatestValid() // TODO want to run this async using applicative

      // TODO will bundle these 3 operations together, but here explicitly broken
      resultAgg: RawResult2[Int, Double, String] = demoCalc2(versionedInt, intermediateResult)
      finalResult: VersionedData[String] <- RawResult.persistAndReturn(resultAgg) // implicit HasRepository for R
      _ <- resultAgg.logInputs(finalResult.version) // implicit HasRepository for T1, T2

    } yield finalResult

  finalVersionedResult match {
    case Success(s) => println(s)
    case Failure(e) => throw e
  }
}
