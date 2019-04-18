package calcs.curried

import calcs.SharedModel._

import scala.util.{Failure, Success, Try}

/**
  * TODO big issues:
  *   Because all outputs are persisted, this tries to persist the Calc.
  *   Because new Calc1s are created, they attempt to get new version IDs
  * Potential solution:
  *   "AnonymousCalc" which passes in the parent version, and doesn't try to persist output Calc
  */
object CurriedFn {

  object Curried {
    def build[T1, R](name: String,
                    f: T1 => R)
                    (implicit calcRepository: CalcRepository,
                     evR: HasRepository[R],
                     ev1: HasRepository[T1]): Calc[T1, R] = {
      Calc(name, f)
    }

    def build[T1, T2, R](name: String,
                        f: (T1, T2) => R)
                        (implicit calcRepository: CalcRepository,
                         evR: HasRepository[R],
                         ev1: HasRepository[T1],
                         ev2: HasRepository[T2]): Calc[T1, Calc[T2, R]] = {
      Calc(name, (t1: T1) => build(name, f.curried(t1)))
    }

//    def buildRecursively[T, R](f: T => R): Calc
  }

  object Calc {

    implicit def CalcTriviallyHasRepository[T: HasRepository, R : HasRepository](implicit ev: HasRepository[R]): HasRepository[Calc[T, R]] =
      new HasRepository[Calc[T, R]] {
        override def dataConceptName: String = "N/A calcs are not serializable"
        override protected def persist(t: VersionedDataUnpersisted[Calc[T, R]]): Try[Unit] = Success()
        override def hydrate(version: CalcVersionAssigned): Try[Calc[T, R]] = Failure(??? ) // TODO can't hydrate
        override def hydrateLatestValid(): Try[VersionedData[Calc[T, R]]] = Failure(???) // TODO can't hydrate
      }

  }

  case class Calc[T1, R] private (name: String,
                                  f: (T1) => R)
                                 (implicit calcRepository: CalcRepository,
                                  evR: HasRepository[R],
                                  ev1: HasRepository[T1])
    extends Function1[VersionedData[T1], Try[VersionedData[R]]]
//      with CalcLike[R]
  {
    final def fullyQualifiedName: CalcName = CalcName(s"$name" /*: ${this.getClass.getName}"*/) // or maybe use typetags

    final def apply(vT1: VersionedData[T1]): Try[VersionedData[R]] =
    {
      for {
        calcVersionAssigned <- calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)
        _ <- this.logSingleInput(calcVersionAssigned, inputCalc = vT1.version)
        pureResult = f(vT1.data)
        persistedResult: VersionedData[R] <- evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }


    private def logSingleInput(calcVersionAssigned: CalcVersionAssigned, inputCalc: CalcVersionAssigned): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = inputCalc))(ev1)
    }

    private def logOutput(calcVersionAssigned: CalcVersionAssigned): Try[Unit] = {
      calcRepository.logOutput(OutputRecord[R](calcVersion = calcVersionAssigned))(evR)
    }
  }




}

object DemoCurriedFn extends App {
  import CurriedFn._

  // Could have multiple persistence contexts for local dev, multiple Prod targets, etc.
  import DemoPersistenceContext._

  // Business logic is cleanly separated
  val bizLogic1 = (d: Double) => d * 100d
  val bizLogic2 = (i: Int, d: Double) => (i * d).toString

  // Building calcs requires injecting implicit CalcRepository and HasRepository[_]s
  val demoCalc1: Calc[Double, Double] = Curried.build("DemoCalc1-Doublex100", bizLogic1)
  val demoCalc2: Calc[Int, Calc[Double, String]] = Curried.build("DemoCalc2-CombineIntDouble", bizLogic2)

  /**
    *   Hydrate[Double] => Calc1[Double] =>
    *                                       Calc2[String]
    *                       Hydrate[Int] =>
    */
  val finalVersionedResult: Try[VersionedData[String]] =
    for {
      versionedDouble: VersionedData[Double]    <- HasRepository[Double].hydrateLatestValid()
      intermediateResult: VersionedData[Double] <- demoCalc1(versionedDouble)
      versionedInt: VersionedData[Int]          <- HasRepository[Int].hydrateLatestValid()

      // TODO combine these two steps and link somehow...
      reified: VersionedData[Calc[Double, String]] <- demoCalc2(versionedInt)
      finalResult: VersionedData[String]        <- reified.data(intermediateResult)

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