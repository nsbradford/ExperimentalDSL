package calcs

import scala.util.Try


object GenerateConsolidatedText extends App {

  (1 to 3).foreach{ i =>
    val argTypeParams = (1 to i).map(x => s"T$x")
    val argTypeParamStr = argTypeParams.mkString(", ")
    val typeParams = argTypeParamStr + ", R"

    val argTypesVersioned = argTypeParams.map(t => s"VersionedData[$t]").mkString(", ")
    val applyArgs = argTypeParams.map(t => s"v$t: VersionedData[$t]").mkString(", ")
//
//    val calcVersions = (1 to i).map(x => s"\t\t\tv$x.version,\n").mkString("")
//    val unverisoned = (1 to i).map(x => s"v$x.data").mkString(", ")
//    val versionedDataInputs = (1 to i).map(x => s"v$x: VersionedData[T$x]").mkString(", ")
//    val versionedDataTypeParams = (1 to i).map(x => s"VersionedData[T$x]").mkString(", ")
//    val fBody = s"\n\t\tRawResult$i(\n$calcVersions\t\t\tUnversionedData(f($unverisoned), CalcUnversioned(CalcName(this.name)))\n\t\t)\n"
//
//    val calc = s"trait Calc$i[$typeParams] extends Function$i[$versionedDataTypeParams, RawResult$i[$typeParams, R]] with Calc[R] {"
//    val calc2 = s"\tdef f: ($typeParams) => R\n\tdef apply($versionedDataInputs): RawResult$i[$typeParams, R] = {$fBody\t}"

    val normalArgs = Seq("val name: String", "val calcRepository: CalcRepository", s"val f: ($argTypeParamStr) => R").mkString(",\n\t\t")
    val inputHasRepositoryEv = (1 to i).map(x => s"ev$x: HasRepository[T$x]")
    val implicitArgs = (s"implicit evR: HasRepository[R]" +: inputHasRepositoryEv).mkString(",\n\t\t")
    val constructorArgs = s"(\n\t\t$normalArgs)\n\t\t($implicitArgs)"

    val functionExtension = s"Function$i[$argTypesVersioned, VersionedData[Try[R]]]"

    val calcVersionAssigned = "calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)"
    val logOutput = "_ <- this.logOutput(calcVersionAssigned)(evR)"
    val logInputs = (1 to i).map(x => s"_ <- this.logSingleInput(calcVersionAssigned)(vT$x)(ev$x)")
    val pureFunctionArgs = argTypeParams.map(t => s"v$t.data").mkString(", ")
    val pureResult = s"pureResult = f($pureFunctionArgs)"
    val persistedResult = "persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))"

    val forLoopSteps = (
        Seq(calcVersionAssigned, logOutput) ++
        logInputs ++
        Seq(pureResult, persistedResult)
      ).mkString("\n\t\t\t")

    val applyBody = s"\n\t\tfor {\n\t\t\t$forLoopSteps\n\t\t} yield persistedResult"
    val classBody = s"\n\n\tfinal def apply($applyArgs): Try[VersionedData[R]] = {$applyBody\n\t}"


    val calc = s"class Calc$i[$typeParams]$constructorArgs \n\textends $functionExtension\n\t\twith Calc[R] {$classBody\n}\n"
    println(calc)

  }
}

object DemoConsolidated extends App {
  import Consolidated22._

}


object Consolidated22 extends SharedModel {

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