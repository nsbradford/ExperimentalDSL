package calcs.calcasfunction.prototype1.consolidated22

import calcs.calcasfunction.prototype1.SharedModel._

import scala.util.{Failure, Success, Try}


object DemoConsolidated extends App {
  import Consolidated22._

  // Could have multiple persistence contexts for local dev, multiple Prod targets, etc.
  import DemoPersistenceContext._

  // Business logic is cleanly separated
  val bizLogic1 = (d: Double) => d * 100d
  val bizLogic2 = (i: Int, d: Double) => (i * d).toString

  // Building calcs requires injecting implicit CalcRepository and HasRepository[_]s
  val demoCalc1: Calc1[Double, Double] = Calc1("DemoCalc1-Doublex100", bizLogic1)
  val demoCalc2: Calc2[Int, Double, String] = Calc2("DemoCalc2-CombineIntDouble", bizLogic2)

  /**
    *   Hydrate[Double] => Calc1[Double] =>
    *                                       Calc2[String]
    *                       Hydrate[Int] =>
    */
  val finalVersionedResult: Try[VersionedData[String]] =
    for {
      versionedDouble: VersionedData[Double]    <- HasRepository[Double].hydrateLatestValid()
      intermediateResult: VersionedData[Double] <- demoCalc1(versionedDouble)
      versionedInt: VersionedData[Int]          <- HasRepository[Int].hydrateLatestValid() // TODO run async w/ applicative
      finalResult: VersionedData[String]        <- demoCalc2(versionedInt, intermediateResult)
    } yield finalResult

  finalVersionedResult match {
    case Success(s) => println(s)
    case Failure(e) => throw e
  }
}


object DemoPersistenceContext {

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

  class MockCalcRepository extends CalcRepository {

    override def requisitionNewRunId(calcName: CalcName): Try[CalcVersionAssigned] = Try {
      calcIdCounter = calcIdCounter + 1
      CalcVersionAssigned(calcName, calcIdCounter)
    }
    private var calcIdCounter = 0

    override def logInput[T : HasRepository](inputRecord: InputRecord[T]): Try[Unit] = Try {
      println(s"CalcRepository > log INPUT > $inputRecord of type: {${HasRepository[T].fullName}}")
    }

    override def logOutput[T : HasRepository](outputRecord: OutputRecord[T]): Try[Unit] = Try {
      println(s"CalcRepository > log OUTPUT > $outputRecord of type: {${HasRepository[T].fullName}}")
    }
  }
  implicit val calcRepository: CalcRepository = new MockCalcRepository
}
