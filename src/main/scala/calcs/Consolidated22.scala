package calcs

import scala.util.{Failure, Success, Try}
import SharedModel._

object GenerateConsolidatedText extends App {

  (1 to 22).foreach{ i =>
    val argTypeParams = (1 to i).map(x => s"T$x")
    val argTypeParamStr = argTypeParams.mkString(", ")

    val constructorArgs = {
      val normalArgs = Seq("val name: String", "val calcRepository: CalcRepository", s"val f: ($argTypeParamStr) => R").mkString(",\n\t\t")
      val inputHasRepositoryEv = (1 to i).map(x => s"ev$x: HasRepository[T$x]")
      val implicitArgs = (s"implicit evR: HasRepository[R]" +: inputHasRepositoryEv).mkString(",\n\t\t")
      s"(\n\t\t$normalArgs)\n\t\t($implicitArgs)"
    }

    val functionExtension = {
      val argTypesVersioned = argTypeParams.map(t => s"VersionedData[$t]").mkString(", ")
      s"Function$i[$argTypesVersioned, Try[VersionedData[R]]]"
    }

    val forLoopSteps = {
      val calcVersionAssigned = "calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)"
      val logOutput = "_ <- this.logOutput(calcVersionAssigned)(evR)"
      val logInputs = (1 to i).map(x => s"_ <- this.logSingleInput(calcVersionAssigned)(vT$x)(ev$x)")
      val pureFunctionArgs = argTypeParams.map(t => s"v$t.data").mkString(", ")
      val pureResult = s"pureResult = f($pureFunctionArgs)"
      val persistedResult = "persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))"
      (
        Seq(calcVersionAssigned, logOutput) ++
          logInputs ++
          Seq(pureResult, persistedResult)
        ).mkString("\n\t\t\t")
    }

    val classBody = {
      val applyArgs = argTypeParams.map(t => s"v$t: VersionedData[$t]").mkString(", ")
      val applyBody = s"\n\t\tfor {\n\t\t\t$forLoopSteps\n\t\t} yield persistedResult"
      s"\n\n\tfinal def apply($applyArgs): Try[VersionedData[R]] = {$applyBody\n\t}"
    }

    val typeParams = argTypeParamStr + ", R"
    val calc = s"class Calc$i[$typeParams]$constructorArgs \n\textends $functionExtension\n\t\twith Calc[R] {$classBody\n}\n"
    println(calc)

    //  class Calc1[T1, R](val name: String,
    //                     val f: (T1) => R,
    //                     val calcRepository: CalcRepository)
    //                     (implicit evR: HasRepository[R],
    //                     ev1: HasRepository[T1])
    //    extends Function1[VersionedData[T1], Try[VersionedData[R]]]
    //      with Calc[R] {
    //
    //    final def apply(v1: VersionedData[T1]): Try[VersionedData[R]] = {
    //      for {
    //        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
    //        _ <- this.logOutput(calcVersionAssigned)(evR)
    //        _ <- this.logSingleInput(calcVersionAssigned)(v1)(ev1)
    //        pureResult = f(v1.data)
    //        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
    //      } yield persistedResult
    //    }
    //  }
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


object DemoConsolidated extends App {
  import Consolidated22._
  import DemoPersistenceContext._

  val bizLogic1 = (d: Double) => d * 100d
  val bizLogic2 = (i: Int, d: Double) => (i * d).toString

  val demoCalc1: Calc1[Double, Double] = new Calc1("DemoCalc1-Doublex100", calcRepository, bizLogic1)
  val demoCalc2: Calc2[Int, Double, String] = new Calc2("DemoCalc2-CombineIntDouble", calcRepository, bizLogic2)

  /**
    * A super-simple pipeline:
    *
    *   Hydrate[Double] => Calc1[Double] =>
    *                                       Calc2[String]
    *                       Hydrate[Int] =>
    *
    */
  val finalVersionedResult: Try[VersionedData[String]] =
    for {
      versionedDouble: VersionedData[Double]    <- HasRepository[Double].hydrateLatestValid()
      intermediateResult: VersionedData[Double] <- demoCalc1(versionedDouble)
      versionedInt                              <- HasRepository[Int].hydrateLatestValid() // TODO run async w/ applicative
      finalResult: VersionedData[String]        <- demoCalc2(versionedInt, intermediateResult)
    } yield finalResult

  finalVersionedResult match {
    case Success(s) => println(s)
    case Failure(e) => throw e
  }
}


object Consolidated22 {

  sealed trait Calc[R]{
    def name: String
    final def fullyQualifiedName: CalcName = CalcName(s"$name: ${this.getClass.getName}") // or maybe use typetags

    protected def calcRepository: CalcRepository

    final protected def logSingleInput[T](calcVersionAssigned: CalcVersionAssigned)
                                         (v: VersionedData[T])
                                         (implicit ev: HasRepository[T]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T](calcVersion = calcVersionAssigned, inputCalc = v.version))(ev)
    }

    final protected def logOutput(calcVersionAssigned: CalcVersionAssigned)
                                       (implicit ev: HasRepository[R]): Try[Unit] = {
      calcRepository.logOutput(OutputRecord[R](calcVersion = calcVersionAssigned))(ev)
    }
  }

  // TODO replace Try[] with F[] for effect type (e.g. IO in prod, Id in test)
  // TODO would be nice to abstract over function arity here
  // TODO would at least be nice to abstract over persistence code - could use parMapN w/ HList

  //==============================================================================================================
  // STOP this is generated code
  //==============================================================================================================


  class Calc1[T1, R](
                      val name: String,
                      val calcRepository: CalcRepository,
                      val f: (T1) => R)
                    (implicit evR: HasRepository[R],
                     ev1: HasRepository[T1])
    extends Function1[VersionedData[T1], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        pureResult = f(vT1.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc2[T1, T2, R](
                          val name: String,
                          val calcRepository: CalcRepository,
                          val f: (T1, T2) => R)
                        (implicit evR: HasRepository[R],
                         ev1: HasRepository[T1],
                         ev2: HasRepository[T2])
    extends Function2[VersionedData[T1], VersionedData[T2], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        pureResult = f(vT1.data, vT2.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc3[T1, T2, T3, R](
                              val name: String,
                              val calcRepository: CalcRepository,
                              val f: (T1, T2, T3) => R)
                            (implicit evR: HasRepository[R],
                             ev1: HasRepository[T1],
                             ev2: HasRepository[T2],
                             ev3: HasRepository[T3])
    extends Function3[VersionedData[T1], VersionedData[T2], VersionedData[T3], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        pureResult = f(vT1.data, vT2.data, vT3.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc4[T1, T2, T3, T4, R](
                                  val name: String,
                                  val calcRepository: CalcRepository,
                                  val f: (T1, T2, T3, T4) => R)
                                (implicit evR: HasRepository[R],
                                 ev1: HasRepository[T1],
                                 ev2: HasRepository[T2],
                                 ev3: HasRepository[T3],
                                 ev4: HasRepository[T4])
    extends Function4[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc5[T1, T2, T3, T4, T5, R](
                                      val name: String,
                                      val calcRepository: CalcRepository,
                                      val f: (T1, T2, T3, T4, T5) => R)
                                    (implicit evR: HasRepository[R],
                                     ev1: HasRepository[T1],
                                     ev2: HasRepository[T2],
                                     ev3: HasRepository[T3],
                                     ev4: HasRepository[T4],
                                     ev5: HasRepository[T5])
    extends Function5[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc6[T1, T2, T3, T4, T5, T6, R](
                                          val name: String,
                                          val calcRepository: CalcRepository,
                                          val f: (T1, T2, T3, T4, T5, T6) => R)
                                        (implicit evR: HasRepository[R],
                                         ev1: HasRepository[T1],
                                         ev2: HasRepository[T2],
                                         ev3: HasRepository[T3],
                                         ev4: HasRepository[T4],
                                         ev5: HasRepository[T5],
                                         ev6: HasRepository[T6])
    extends Function6[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc7[T1, T2, T3, T4, T5, T6, T7, R](
                                              val name: String,
                                              val calcRepository: CalcRepository,
                                              val f: (T1, T2, T3, T4, T5, T6, T7) => R)
                                            (implicit evR: HasRepository[R],
                                             ev1: HasRepository[T1],
                                             ev2: HasRepository[T2],
                                             ev3: HasRepository[T3],
                                             ev4: HasRepository[T4],
                                             ev5: HasRepository[T5],
                                             ev6: HasRepository[T6],
                                             ev7: HasRepository[T7])
    extends Function7[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc8[T1, T2, T3, T4, T5, T6, T7, T8, R](
                                                  val name: String,
                                                  val calcRepository: CalcRepository,
                                                  val f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)
                                                (implicit evR: HasRepository[R],
                                                 ev1: HasRepository[T1],
                                                 ev2: HasRepository[T2],
                                                 ev3: HasRepository[T3],
                                                 ev4: HasRepository[T4],
                                                 ev5: HasRepository[T5],
                                                 ev6: HasRepository[T6],
                                                 ev7: HasRepository[T7],
                                                 ev8: HasRepository[T8])
    extends Function8[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](
                                                      val name: String,
                                                      val calcRepository: CalcRepository,
                                                      val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)
                                                    (implicit evR: HasRepository[R],
                                                     ev1: HasRepository[T1],
                                                     ev2: HasRepository[T2],
                                                     ev3: HasRepository[T3],
                                                     ev4: HasRepository[T4],
                                                     ev5: HasRepository[T5],
                                                     ev6: HasRepository[T6],
                                                     ev7: HasRepository[T7],
                                                     ev8: HasRepository[T8],
                                                     ev9: HasRepository[T9])
    extends Function9[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](
                                                            val name: String,
                                                            val calcRepository: CalcRepository,
                                                            val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)
                                                          (implicit evR: HasRepository[R],
                                                           ev1: HasRepository[T1],
                                                           ev2: HasRepository[T2],
                                                           ev3: HasRepository[T3],
                                                           ev4: HasRepository[T4],
                                                           ev5: HasRepository[T5],
                                                           ev6: HasRepository[T6],
                                                           ev7: HasRepository[T7],
                                                           ev8: HasRepository[T8],
                                                           ev9: HasRepository[T9],
                                                           ev10: HasRepository[T10])
    extends Function10[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](
                                                                 val name: String,
                                                                 val calcRepository: CalcRepository,
                                                                 val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R)
                                                               (implicit evR: HasRepository[R],
                                                                ev1: HasRepository[T1],
                                                                ev2: HasRepository[T2],
                                                                ev3: HasRepository[T3],
                                                                ev4: HasRepository[T4],
                                                                ev5: HasRepository[T5],
                                                                ev6: HasRepository[T6],
                                                                ev7: HasRepository[T7],
                                                                ev8: HasRepository[T8],
                                                                ev9: HasRepository[T9],
                                                                ev10: HasRepository[T10],
                                                                ev11: HasRepository[T11])
    extends Function11[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10], vT11: VersionedData[T11]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        _ <- this.logSingleInput(calcVersionAssigned)(vT11)(ev11)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data, vT11.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](
                                                                      val name: String,
                                                                      val calcRepository: CalcRepository,
                                                                      val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R)
                                                                    (implicit evR: HasRepository[R],
                                                                     ev1: HasRepository[T1],
                                                                     ev2: HasRepository[T2],
                                                                     ev3: HasRepository[T3],
                                                                     ev4: HasRepository[T4],
                                                                     ev5: HasRepository[T5],
                                                                     ev6: HasRepository[T6],
                                                                     ev7: HasRepository[T7],
                                                                     ev8: HasRepository[T8],
                                                                     ev9: HasRepository[T9],
                                                                     ev10: HasRepository[T10],
                                                                     ev11: HasRepository[T11],
                                                                     ev12: HasRepository[T12])
    extends Function12[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10], vT11: VersionedData[T11], vT12: VersionedData[T12]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        _ <- this.logSingleInput(calcVersionAssigned)(vT11)(ev11)
        _ <- this.logSingleInput(calcVersionAssigned)(vT12)(ev12)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data, vT11.data, vT12.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](
                                                                           val name: String,
                                                                           val calcRepository: CalcRepository,
                                                                           val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R)
                                                                         (implicit evR: HasRepository[R],
                                                                          ev1: HasRepository[T1],
                                                                          ev2: HasRepository[T2],
                                                                          ev3: HasRepository[T3],
                                                                          ev4: HasRepository[T4],
                                                                          ev5: HasRepository[T5],
                                                                          ev6: HasRepository[T6],
                                                                          ev7: HasRepository[T7],
                                                                          ev8: HasRepository[T8],
                                                                          ev9: HasRepository[T9],
                                                                          ev10: HasRepository[T10],
                                                                          ev11: HasRepository[T11],
                                                                          ev12: HasRepository[T12],
                                                                          ev13: HasRepository[T13])
    extends Function13[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10], vT11: VersionedData[T11], vT12: VersionedData[T12], vT13: VersionedData[T13]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        _ <- this.logSingleInput(calcVersionAssigned)(vT11)(ev11)
        _ <- this.logSingleInput(calcVersionAssigned)(vT12)(ev12)
        _ <- this.logSingleInput(calcVersionAssigned)(vT13)(ev13)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data, vT11.data, vT12.data, vT13.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](
                                                                                val name: String,
                                                                                val calcRepository: CalcRepository,
                                                                                val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R)
                                                                              (implicit evR: HasRepository[R],
                                                                               ev1: HasRepository[T1],
                                                                               ev2: HasRepository[T2],
                                                                               ev3: HasRepository[T3],
                                                                               ev4: HasRepository[T4],
                                                                               ev5: HasRepository[T5],
                                                                               ev6: HasRepository[T6],
                                                                               ev7: HasRepository[T7],
                                                                               ev8: HasRepository[T8],
                                                                               ev9: HasRepository[T9],
                                                                               ev10: HasRepository[T10],
                                                                               ev11: HasRepository[T11],
                                                                               ev12: HasRepository[T12],
                                                                               ev13: HasRepository[T13],
                                                                               ev14: HasRepository[T14])
    extends Function14[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10], vT11: VersionedData[T11], vT12: VersionedData[T12], vT13: VersionedData[T13], vT14: VersionedData[T14]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        _ <- this.logSingleInput(calcVersionAssigned)(vT11)(ev11)
        _ <- this.logSingleInput(calcVersionAssigned)(vT12)(ev12)
        _ <- this.logSingleInput(calcVersionAssigned)(vT13)(ev13)
        _ <- this.logSingleInput(calcVersionAssigned)(vT14)(ev14)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data, vT11.data, vT12.data, vT13.data, vT14.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](
                                                                                     val name: String,
                                                                                     val calcRepository: CalcRepository,
                                                                                     val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R)
                                                                                   (implicit evR: HasRepository[R],
                                                                                    ev1: HasRepository[T1],
                                                                                    ev2: HasRepository[T2],
                                                                                    ev3: HasRepository[T3],
                                                                                    ev4: HasRepository[T4],
                                                                                    ev5: HasRepository[T5],
                                                                                    ev6: HasRepository[T6],
                                                                                    ev7: HasRepository[T7],
                                                                                    ev8: HasRepository[T8],
                                                                                    ev9: HasRepository[T9],
                                                                                    ev10: HasRepository[T10],
                                                                                    ev11: HasRepository[T11],
                                                                                    ev12: HasRepository[T12],
                                                                                    ev13: HasRepository[T13],
                                                                                    ev14: HasRepository[T14],
                                                                                    ev15: HasRepository[T15])
    extends Function15[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10], vT11: VersionedData[T11], vT12: VersionedData[T12], vT13: VersionedData[T13], vT14: VersionedData[T14], vT15: VersionedData[T15]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        _ <- this.logSingleInput(calcVersionAssigned)(vT11)(ev11)
        _ <- this.logSingleInput(calcVersionAssigned)(vT12)(ev12)
        _ <- this.logSingleInput(calcVersionAssigned)(vT13)(ev13)
        _ <- this.logSingleInput(calcVersionAssigned)(vT14)(ev14)
        _ <- this.logSingleInput(calcVersionAssigned)(vT15)(ev15)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data, vT11.data, vT12.data, vT13.data, vT14.data, vT15.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](
                                                                                          val name: String,
                                                                                          val calcRepository: CalcRepository,
                                                                                          val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R)
                                                                                        (implicit evR: HasRepository[R],
                                                                                         ev1: HasRepository[T1],
                                                                                         ev2: HasRepository[T2],
                                                                                         ev3: HasRepository[T3],
                                                                                         ev4: HasRepository[T4],
                                                                                         ev5: HasRepository[T5],
                                                                                         ev6: HasRepository[T6],
                                                                                         ev7: HasRepository[T7],
                                                                                         ev8: HasRepository[T8],
                                                                                         ev9: HasRepository[T9],
                                                                                         ev10: HasRepository[T10],
                                                                                         ev11: HasRepository[T11],
                                                                                         ev12: HasRepository[T12],
                                                                                         ev13: HasRepository[T13],
                                                                                         ev14: HasRepository[T14],
                                                                                         ev15: HasRepository[T15],
                                                                                         ev16: HasRepository[T16])
    extends Function16[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10], vT11: VersionedData[T11], vT12: VersionedData[T12], vT13: VersionedData[T13], vT14: VersionedData[T14], vT15: VersionedData[T15], vT16: VersionedData[T16]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        _ <- this.logSingleInput(calcVersionAssigned)(vT11)(ev11)
        _ <- this.logSingleInput(calcVersionAssigned)(vT12)(ev12)
        _ <- this.logSingleInput(calcVersionAssigned)(vT13)(ev13)
        _ <- this.logSingleInput(calcVersionAssigned)(vT14)(ev14)
        _ <- this.logSingleInput(calcVersionAssigned)(vT15)(ev15)
        _ <- this.logSingleInput(calcVersionAssigned)(vT16)(ev16)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data, vT11.data, vT12.data, vT13.data, vT14.data, vT15.data, vT16.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](
                                                                                               val name: String,
                                                                                               val calcRepository: CalcRepository,
                                                                                               val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R)
                                                                                             (implicit evR: HasRepository[R],
                                                                                              ev1: HasRepository[T1],
                                                                                              ev2: HasRepository[T2],
                                                                                              ev3: HasRepository[T3],
                                                                                              ev4: HasRepository[T4],
                                                                                              ev5: HasRepository[T5],
                                                                                              ev6: HasRepository[T6],
                                                                                              ev7: HasRepository[T7],
                                                                                              ev8: HasRepository[T8],
                                                                                              ev9: HasRepository[T9],
                                                                                              ev10: HasRepository[T10],
                                                                                              ev11: HasRepository[T11],
                                                                                              ev12: HasRepository[T12],
                                                                                              ev13: HasRepository[T13],
                                                                                              ev14: HasRepository[T14],
                                                                                              ev15: HasRepository[T15],
                                                                                              ev16: HasRepository[T16],
                                                                                              ev17: HasRepository[T17])
    extends Function17[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], VersionedData[T17], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10], vT11: VersionedData[T11], vT12: VersionedData[T12], vT13: VersionedData[T13], vT14: VersionedData[T14], vT15: VersionedData[T15], vT16: VersionedData[T16], vT17: VersionedData[T17]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        _ <- this.logSingleInput(calcVersionAssigned)(vT11)(ev11)
        _ <- this.logSingleInput(calcVersionAssigned)(vT12)(ev12)
        _ <- this.logSingleInput(calcVersionAssigned)(vT13)(ev13)
        _ <- this.logSingleInput(calcVersionAssigned)(vT14)(ev14)
        _ <- this.logSingleInput(calcVersionAssigned)(vT15)(ev15)
        _ <- this.logSingleInput(calcVersionAssigned)(vT16)(ev16)
        _ <- this.logSingleInput(calcVersionAssigned)(vT17)(ev17)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data, vT11.data, vT12.data, vT13.data, vT14.data, vT15.data, vT16.data, vT17.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](
                                                                                                    val name: String,
                                                                                                    val calcRepository: CalcRepository,
                                                                                                    val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R)
                                                                                                  (implicit evR: HasRepository[R],
                                                                                                   ev1: HasRepository[T1],
                                                                                                   ev2: HasRepository[T2],
                                                                                                   ev3: HasRepository[T3],
                                                                                                   ev4: HasRepository[T4],
                                                                                                   ev5: HasRepository[T5],
                                                                                                   ev6: HasRepository[T6],
                                                                                                   ev7: HasRepository[T7],
                                                                                                   ev8: HasRepository[T8],
                                                                                                   ev9: HasRepository[T9],
                                                                                                   ev10: HasRepository[T10],
                                                                                                   ev11: HasRepository[T11],
                                                                                                   ev12: HasRepository[T12],
                                                                                                   ev13: HasRepository[T13],
                                                                                                   ev14: HasRepository[T14],
                                                                                                   ev15: HasRepository[T15],
                                                                                                   ev16: HasRepository[T16],
                                                                                                   ev17: HasRepository[T17],
                                                                                                   ev18: HasRepository[T18])
    extends Function18[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], VersionedData[T17], VersionedData[T18], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10], vT11: VersionedData[T11], vT12: VersionedData[T12], vT13: VersionedData[T13], vT14: VersionedData[T14], vT15: VersionedData[T15], vT16: VersionedData[T16], vT17: VersionedData[T17], vT18: VersionedData[T18]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        _ <- this.logSingleInput(calcVersionAssigned)(vT11)(ev11)
        _ <- this.logSingleInput(calcVersionAssigned)(vT12)(ev12)
        _ <- this.logSingleInput(calcVersionAssigned)(vT13)(ev13)
        _ <- this.logSingleInput(calcVersionAssigned)(vT14)(ev14)
        _ <- this.logSingleInput(calcVersionAssigned)(vT15)(ev15)
        _ <- this.logSingleInput(calcVersionAssigned)(vT16)(ev16)
        _ <- this.logSingleInput(calcVersionAssigned)(vT17)(ev17)
        _ <- this.logSingleInput(calcVersionAssigned)(vT18)(ev18)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data, vT11.data, vT12.data, vT13.data, vT14.data, vT15.data, vT16.data, vT17.data, vT18.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](
                                                                                                         val name: String,
                                                                                                         val calcRepository: CalcRepository,
                                                                                                         val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R)
                                                                                                       (implicit evR: HasRepository[R],
                                                                                                        ev1: HasRepository[T1],
                                                                                                        ev2: HasRepository[T2],
                                                                                                        ev3: HasRepository[T3],
                                                                                                        ev4: HasRepository[T4],
                                                                                                        ev5: HasRepository[T5],
                                                                                                        ev6: HasRepository[T6],
                                                                                                        ev7: HasRepository[T7],
                                                                                                        ev8: HasRepository[T8],
                                                                                                        ev9: HasRepository[T9],
                                                                                                        ev10: HasRepository[T10],
                                                                                                        ev11: HasRepository[T11],
                                                                                                        ev12: HasRepository[T12],
                                                                                                        ev13: HasRepository[T13],
                                                                                                        ev14: HasRepository[T14],
                                                                                                        ev15: HasRepository[T15],
                                                                                                        ev16: HasRepository[T16],
                                                                                                        ev17: HasRepository[T17],
                                                                                                        ev18: HasRepository[T18],
                                                                                                        ev19: HasRepository[T19])
    extends Function19[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], VersionedData[T17], VersionedData[T18], VersionedData[T19], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10], vT11: VersionedData[T11], vT12: VersionedData[T12], vT13: VersionedData[T13], vT14: VersionedData[T14], vT15: VersionedData[T15], vT16: VersionedData[T16], vT17: VersionedData[T17], vT18: VersionedData[T18], vT19: VersionedData[T19]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        _ <- this.logSingleInput(calcVersionAssigned)(vT11)(ev11)
        _ <- this.logSingleInput(calcVersionAssigned)(vT12)(ev12)
        _ <- this.logSingleInput(calcVersionAssigned)(vT13)(ev13)
        _ <- this.logSingleInput(calcVersionAssigned)(vT14)(ev14)
        _ <- this.logSingleInput(calcVersionAssigned)(vT15)(ev15)
        _ <- this.logSingleInput(calcVersionAssigned)(vT16)(ev16)
        _ <- this.logSingleInput(calcVersionAssigned)(vT17)(ev17)
        _ <- this.logSingleInput(calcVersionAssigned)(vT18)(ev18)
        _ <- this.logSingleInput(calcVersionAssigned)(vT19)(ev19)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data, vT11.data, vT12.data, vT13.data, vT14.data, vT15.data, vT16.data, vT17.data, vT18.data, vT19.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](
                                                                                                              val name: String,
                                                                                                              val calcRepository: CalcRepository,
                                                                                                              val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R)
                                                                                                            (implicit evR: HasRepository[R],
                                                                                                             ev1: HasRepository[T1],
                                                                                                             ev2: HasRepository[T2],
                                                                                                             ev3: HasRepository[T3],
                                                                                                             ev4: HasRepository[T4],
                                                                                                             ev5: HasRepository[T5],
                                                                                                             ev6: HasRepository[T6],
                                                                                                             ev7: HasRepository[T7],
                                                                                                             ev8: HasRepository[T8],
                                                                                                             ev9: HasRepository[T9],
                                                                                                             ev10: HasRepository[T10],
                                                                                                             ev11: HasRepository[T11],
                                                                                                             ev12: HasRepository[T12],
                                                                                                             ev13: HasRepository[T13],
                                                                                                             ev14: HasRepository[T14],
                                                                                                             ev15: HasRepository[T15],
                                                                                                             ev16: HasRepository[T16],
                                                                                                             ev17: HasRepository[T17],
                                                                                                             ev18: HasRepository[T18],
                                                                                                             ev19: HasRepository[T19],
                                                                                                             ev20: HasRepository[T20])
    extends Function20[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], VersionedData[T17], VersionedData[T18], VersionedData[T19], VersionedData[T20], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10], vT11: VersionedData[T11], vT12: VersionedData[T12], vT13: VersionedData[T13], vT14: VersionedData[T14], vT15: VersionedData[T15], vT16: VersionedData[T16], vT17: VersionedData[T17], vT18: VersionedData[T18], vT19: VersionedData[T19], vT20: VersionedData[T20]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        _ <- this.logSingleInput(calcVersionAssigned)(vT11)(ev11)
        _ <- this.logSingleInput(calcVersionAssigned)(vT12)(ev12)
        _ <- this.logSingleInput(calcVersionAssigned)(vT13)(ev13)
        _ <- this.logSingleInput(calcVersionAssigned)(vT14)(ev14)
        _ <- this.logSingleInput(calcVersionAssigned)(vT15)(ev15)
        _ <- this.logSingleInput(calcVersionAssigned)(vT16)(ev16)
        _ <- this.logSingleInput(calcVersionAssigned)(vT17)(ev17)
        _ <- this.logSingleInput(calcVersionAssigned)(vT18)(ev18)
        _ <- this.logSingleInput(calcVersionAssigned)(vT19)(ev19)
        _ <- this.logSingleInput(calcVersionAssigned)(vT20)(ev20)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data, vT11.data, vT12.data, vT13.data, vT14.data, vT15.data, vT16.data, vT17.data, vT18.data, vT19.data, vT20.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](
                                                                                                                   val name: String,
                                                                                                                   val calcRepository: CalcRepository,
                                                                                                                   val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R)
                                                                                                                 (implicit evR: HasRepository[R],
                                                                                                                  ev1: HasRepository[T1],
                                                                                                                  ev2: HasRepository[T2],
                                                                                                                  ev3: HasRepository[T3],
                                                                                                                  ev4: HasRepository[T4],
                                                                                                                  ev5: HasRepository[T5],
                                                                                                                  ev6: HasRepository[T6],
                                                                                                                  ev7: HasRepository[T7],
                                                                                                                  ev8: HasRepository[T8],
                                                                                                                  ev9: HasRepository[T9],
                                                                                                                  ev10: HasRepository[T10],
                                                                                                                  ev11: HasRepository[T11],
                                                                                                                  ev12: HasRepository[T12],
                                                                                                                  ev13: HasRepository[T13],
                                                                                                                  ev14: HasRepository[T14],
                                                                                                                  ev15: HasRepository[T15],
                                                                                                                  ev16: HasRepository[T16],
                                                                                                                  ev17: HasRepository[T17],
                                                                                                                  ev18: HasRepository[T18],
                                                                                                                  ev19: HasRepository[T19],
                                                                                                                  ev20: HasRepository[T20],
                                                                                                                  ev21: HasRepository[T21])
    extends Function21[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], VersionedData[T17], VersionedData[T18], VersionedData[T19], VersionedData[T20], VersionedData[T21], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10], vT11: VersionedData[T11], vT12: VersionedData[T12], vT13: VersionedData[T13], vT14: VersionedData[T14], vT15: VersionedData[T15], vT16: VersionedData[T16], vT17: VersionedData[T17], vT18: VersionedData[T18], vT19: VersionedData[T19], vT20: VersionedData[T20], vT21: VersionedData[T21]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        _ <- this.logSingleInput(calcVersionAssigned)(vT11)(ev11)
        _ <- this.logSingleInput(calcVersionAssigned)(vT12)(ev12)
        _ <- this.logSingleInput(calcVersionAssigned)(vT13)(ev13)
        _ <- this.logSingleInput(calcVersionAssigned)(vT14)(ev14)
        _ <- this.logSingleInput(calcVersionAssigned)(vT15)(ev15)
        _ <- this.logSingleInput(calcVersionAssigned)(vT16)(ev16)
        _ <- this.logSingleInput(calcVersionAssigned)(vT17)(ev17)
        _ <- this.logSingleInput(calcVersionAssigned)(vT18)(ev18)
        _ <- this.logSingleInput(calcVersionAssigned)(vT19)(ev19)
        _ <- this.logSingleInput(calcVersionAssigned)(vT20)(ev20)
        _ <- this.logSingleInput(calcVersionAssigned)(vT21)(ev21)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data, vT11.data, vT12.data, vT13.data, vT14.data, vT15.data, vT16.data, vT17.data, vT18.data, vT19.data, vT20.data, vT21.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }

  class Calc22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](
                                                                                                                        val name: String,
                                                                                                                        val calcRepository: CalcRepository,
                                                                                                                        val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R)
                                                                                                                      (implicit evR: HasRepository[R],
                                                                                                                       ev1: HasRepository[T1],
                                                                                                                       ev2: HasRepository[T2],
                                                                                                                       ev3: HasRepository[T3],
                                                                                                                       ev4: HasRepository[T4],
                                                                                                                       ev5: HasRepository[T5],
                                                                                                                       ev6: HasRepository[T6],
                                                                                                                       ev7: HasRepository[T7],
                                                                                                                       ev8: HasRepository[T8],
                                                                                                                       ev9: HasRepository[T9],
                                                                                                                       ev10: HasRepository[T10],
                                                                                                                       ev11: HasRepository[T11],
                                                                                                                       ev12: HasRepository[T12],
                                                                                                                       ev13: HasRepository[T13],
                                                                                                                       ev14: HasRepository[T14],
                                                                                                                       ev15: HasRepository[T15],
                                                                                                                       ev16: HasRepository[T16],
                                                                                                                       ev17: HasRepository[T17],
                                                                                                                       ev18: HasRepository[T18],
                                                                                                                       ev19: HasRepository[T19],
                                                                                                                       ev20: HasRepository[T20],
                                                                                                                       ev21: HasRepository[T21],
                                                                                                                       ev22: HasRepository[T22])
    extends Function22[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], VersionedData[T17], VersionedData[T18], VersionedData[T19], VersionedData[T20], VersionedData[T21], VersionedData[T22], Try[VersionedData[R]]]
      with Calc[R] {

    final def apply(vT1: VersionedData[T1], vT2: VersionedData[T2], vT3: VersionedData[T3], vT4: VersionedData[T4], vT5: VersionedData[T5], vT6: VersionedData[T6], vT7: VersionedData[T7], vT8: VersionedData[T8], vT9: VersionedData[T9], vT10: VersionedData[T10], vT11: VersionedData[T11], vT12: VersionedData[T12], vT13: VersionedData[T13], vT14: VersionedData[T14], vT15: VersionedData[T15], vT16: VersionedData[T16], vT17: VersionedData[T17], vT18: VersionedData[T18], vT19: VersionedData[T19], vT20: VersionedData[T20], vT21: VersionedData[T21], vT22: VersionedData[T22]): Try[VersionedData[R]] = {
      for {
        calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
        _ <- this.logOutput(calcVersionAssigned)(evR)
        _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
        _ <- this.logSingleInput(calcVersionAssigned)(vT2)(ev2)
        _ <- this.logSingleInput(calcVersionAssigned)(vT3)(ev3)
        _ <- this.logSingleInput(calcVersionAssigned)(vT4)(ev4)
        _ <- this.logSingleInput(calcVersionAssigned)(vT5)(ev5)
        _ <- this.logSingleInput(calcVersionAssigned)(vT6)(ev6)
        _ <- this.logSingleInput(calcVersionAssigned)(vT7)(ev7)
        _ <- this.logSingleInput(calcVersionAssigned)(vT8)(ev8)
        _ <- this.logSingleInput(calcVersionAssigned)(vT9)(ev9)
        _ <- this.logSingleInput(calcVersionAssigned)(vT10)(ev10)
        _ <- this.logSingleInput(calcVersionAssigned)(vT11)(ev11)
        _ <- this.logSingleInput(calcVersionAssigned)(vT12)(ev12)
        _ <- this.logSingleInput(calcVersionAssigned)(vT13)(ev13)
        _ <- this.logSingleInput(calcVersionAssigned)(vT14)(ev14)
        _ <- this.logSingleInput(calcVersionAssigned)(vT15)(ev15)
        _ <- this.logSingleInput(calcVersionAssigned)(vT16)(ev16)
        _ <- this.logSingleInput(calcVersionAssigned)(vT17)(ev17)
        _ <- this.logSingleInput(calcVersionAssigned)(vT18)(ev18)
        _ <- this.logSingleInput(calcVersionAssigned)(vT19)(ev19)
        _ <- this.logSingleInput(calcVersionAssigned)(vT20)(ev20)
        _ <- this.logSingleInput(calcVersionAssigned)(vT21)(ev21)
        _ <- this.logSingleInput(calcVersionAssigned)(vT22)(ev22)
        pureResult = f(vT1.data, vT2.data, vT3.data, vT4.data, vT5.data, vT6.data, vT7.data, vT8.data, vT9.data, vT10.data, vT11.data, vT12.data, vT13.data, vT14.data, vT15.data, vT16.data, vT17.data, vT18.data, vT19.data, vT20.data, vT21.data, vT22.data)
        persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))
      } yield persistedResult
    }
  }


}