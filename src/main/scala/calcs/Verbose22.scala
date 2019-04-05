package calcs

import scala.reflect.ClassTag
import scala.util.Try
import scala.reflect.runtime.universe._
import cats.effect.IO



object Verbose22Imports
  extends Verbose22


object GenerateText extends App {

  (1 to 22).foreach{ i =>

    // : TypeTag : ClassTag
    val typeParamsVariant = (1 to i).map(x => s"T$x").mkString(", ") + ", +R"
    val typeParmsTuple = (1 to i).map(x => s"T$x").mkString(", ")

    val aggParams = (1 to i).map(x => s"v$x: CalcVersionAssigned").mkString(", ") + ", r: UnversionedData[R]"
    val agg = s"case class Agg$i[$typeParamsVariant]($aggParams) extends RawResult[R]"
    val logInputsArgs = (1 to i).map(x => s"ev$x: HasRepository[T$x]").mkString(", ")
    val logInputsInner = (1 to i).map(x => s"calcRepository.logInput(InputRecord[T$x](calcVersion = calcVersionAssigned, inputCalc = this.v$x))").mkString("\n\t\t")
    val aggInner = s"{\n\tdef logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, $logInputsArgs): Try[Unit] = {\n\t\t$logInputsInner \n\t}\n}"


    val calcVersions = (1 to i).map(x => s"\t\t\tv$x.version,\n").mkString("")
    val unverisoned = (1 to i).map(x => s"v$x.data").mkString(", ")
    val versionedDataInputs = (1 to i).map(x => s"v$x: VersionedData[T$x]").mkString(", ")
    val versionedDataTypeParams = (1 to i).map(x => s"VersionedData[T$x]").mkString(", ")
    val fBody = s"\n\t\tRawResult$i(\n$calcVersions\t\t\tUnversionedData(f($unverisoned), CalcUnversioned(CalcName(this.name)))\n\t\t)\n"

    val calc = s"trait Calc$i[$typeParamsVariant] extends Function$i[$versionedDataTypeParams, RawResult$i[$typeParmsTuple, R]] with Calc[R] {"
    val calc2 = s"\tdef f: ($typeParmsTuple) => R\n\tdef apply($versionedDataInputs): RawResult$i[$typeParmsTuple, R] = {$fBody\t}"

    println(agg)
    println(aggInner)
    println(calc)
    println(calc2)
    println("\n}\n")
  }
}


/**
  * Initial design - have a Calc for each Function1-22
  */
trait Verbose22 extends SharedModel {

  /**
    * Calculators have an internal Aggregator of
    */
  sealed trait RawResult[+R]{ // TODO these are only used in teh RawResult companion object
    def r: UnversionedData[R]
    final def source: CalcUnversioned = r.version
  }

  object RawResult {
    /**
      * calcs can't be Covariant in R because HasRepository can't be covariant
      */
    final def persistAndReturn[R](agg: RawResult[R])
                                 (implicit ev: HasRepository[R],
                                  calcRepository: CalcRepository): Try[VersionedData[R]] = {
      for {
        newCalcId <- calcRepository.requisitionNewRunId(agg.source.calcName)
        newlyVersionedData = VersionedDataUnpersisted(data = agg.r.data, version = newCalcId)
        vdata <- ev.persistWrap(newlyVersionedData)
      } yield vdata
    }


  }

  sealed trait Calc[+R]{
    def name: String
    final def fullyQualifiedName: String = s"$name: ${this.getClass.getName}" // or maybe use typetags
  }
  object Calc{
    // TODO this should become auto-generated as well

    def apply[T1, R](minorName: String, func: (T1) => R) = new Calc1[T1, R]{
      override def f: (T1) => R = func
      override def name: String = minorName
    }

    def apply[T1, T2, R](minorName: String, func: (T1, T2) => R) = new Calc2[T1, T2, R]{
      override def f: (T1, T2) => R = func
      override def name: String = minorName
    }
  }


  //==============================================================================================================
  // STOP this is generated code
  //==============================================================================================================
  case class RawResult1[T1, +R](v1: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
    }
  }
  trait Calc1[T1, +R] extends Function1[VersionedData[T1], RawResult1[T1, R]] with Calc[R] {
    def f: (T1) => R
    def apply(v1: VersionedData[T1]): RawResult1[T1, R] = {
      RawResult1(
        v1.version,
        UnversionedData(f(v1.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult2[T1, T2, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
    }
  }
  trait Calc2[T1, T2, +R] extends Function2[VersionedData[T1], VersionedData[T2], RawResult2[T1, T2, R]] with Calc[R] {
    def f: (T1, T2) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2]): RawResult2[T1, T2, R] = {
      RawResult2(
        v1.version,
        v2.version,
        UnversionedData(f(v1.data, v2.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult3[T1, T2, T3, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
    }
  }
  trait Calc3[T1, T2, T3, +R] extends Function3[VersionedData[T1], VersionedData[T2], VersionedData[T3], RawResult3[T1, T2, T3, R]] with Calc[R] {
    def f: (T1, T2, T3) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3]): RawResult3[T1, T2, T3, R] = {
      RawResult3(
        v1.version,
        v2.version,
        v3.version,
        UnversionedData(f(v1.data, v2.data, v3.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult4[T1, T2, T3, T4, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
    }
  }
  trait Calc4[T1, T2, T3, T4, +R] extends Function4[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], RawResult4[T1, T2, T3, T4, R]] with Calc[R] {
    def f: (T1, T2, T3, T4) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4]): RawResult4[T1, T2, T3, T4, R] = {
      RawResult4(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult5[T1, T2, T3, T4, T5, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
    }
  }
  trait Calc5[T1, T2, T3, T4, T5, +R] extends Function5[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], RawResult5[T1, T2, T3, T4, T5, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5]): RawResult5[T1, T2, T3, T4, T5, R] = {
      RawResult5(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult6[T1, T2, T3, T4, T5, T6, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
    }
  }
  trait Calc6[T1, T2, T3, T4, T5, T6, +R] extends Function6[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], RawResult6[T1, T2, T3, T4, T5, T6, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6]): RawResult6[T1, T2, T3, T4, T5, T6, R] = {
      RawResult6(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult7[T1, T2, T3, T4, T5, T6, T7, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
    }
  }
  trait Calc7[T1, T2, T3, T4, T5, T6, T7, +R] extends Function7[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], RawResult7[T1, T2, T3, T4, T5, T6, T7, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7]): RawResult7[T1, T2, T3, T4, T5, T6, T7, R] = {
      RawResult7(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult8[T1, T2, T3, T4, T5, T6, T7, T8, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
    }
  }
  trait Calc8[T1, T2, T3, T4, T5, T6, T7, T8, +R] extends Function8[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], RawResult8[T1, T2, T3, T4, T5, T6, T7, T8, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8]): RawResult8[T1, T2, T3, T4, T5, T6, T7, T8, R] = {
      RawResult8(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
    }
  }
  trait Calc9[T1, T2, T3, T4, T5, T6, T7, T8, T9, +R] extends Function9[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], RawResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9]): RawResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] = {
      RawResult9(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
    }
  }
  trait Calc10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, +R] extends Function10[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], RawResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10]): RawResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] = {
      RawResult10(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, v11: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10], ev11: HasRepository[T11]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
      calcRepository.logInput(InputRecord[T11](calcVersion = calcVersionAssigned, inputCalc = this.v11))
    }
  }
  trait Calc11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, +R] extends Function11[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], RawResult11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10], v11: VersionedData[T11]): RawResult11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] = {
      RawResult11(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        v11.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data, v11.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, v11: CalcVersionAssigned, v12: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10], ev11: HasRepository[T11], ev12: HasRepository[T12]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
      calcRepository.logInput(InputRecord[T11](calcVersion = calcVersionAssigned, inputCalc = this.v11))
      calcRepository.logInput(InputRecord[T12](calcVersion = calcVersionAssigned, inputCalc = this.v12))
    }
  }
  trait Calc12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, +R] extends Function12[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], RawResult12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10], v11: VersionedData[T11], v12: VersionedData[T12]): RawResult12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] = {
      RawResult12(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        v11.version,
        v12.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data, v11.data, v12.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, v11: CalcVersionAssigned, v12: CalcVersionAssigned, v13: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10], ev11: HasRepository[T11], ev12: HasRepository[T12], ev13: HasRepository[T13]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
      calcRepository.logInput(InputRecord[T11](calcVersion = calcVersionAssigned, inputCalc = this.v11))
      calcRepository.logInput(InputRecord[T12](calcVersion = calcVersionAssigned, inputCalc = this.v12))
      calcRepository.logInput(InputRecord[T13](calcVersion = calcVersionAssigned, inputCalc = this.v13))
    }
  }
  trait Calc13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, +R] extends Function13[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], RawResult13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10], v11: VersionedData[T11], v12: VersionedData[T12], v13: VersionedData[T13]): RawResult13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] = {
      RawResult13(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        v11.version,
        v12.version,
        v13.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data, v11.data, v12.data, v13.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, v11: CalcVersionAssigned, v12: CalcVersionAssigned, v13: CalcVersionAssigned, v14: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10], ev11: HasRepository[T11], ev12: HasRepository[T12], ev13: HasRepository[T13], ev14: HasRepository[T14]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
      calcRepository.logInput(InputRecord[T11](calcVersion = calcVersionAssigned, inputCalc = this.v11))
      calcRepository.logInput(InputRecord[T12](calcVersion = calcVersionAssigned, inputCalc = this.v12))
      calcRepository.logInput(InputRecord[T13](calcVersion = calcVersionAssigned, inputCalc = this.v13))
      calcRepository.logInput(InputRecord[T14](calcVersion = calcVersionAssigned, inputCalc = this.v14))
    }
  }
  trait Calc14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, +R] extends Function14[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], RawResult14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10], v11: VersionedData[T11], v12: VersionedData[T12], v13: VersionedData[T13], v14: VersionedData[T14]): RawResult14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] = {
      RawResult14(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        v11.version,
        v12.version,
        v13.version,
        v14.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data, v11.data, v12.data, v13.data, v14.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, v11: CalcVersionAssigned, v12: CalcVersionAssigned, v13: CalcVersionAssigned, v14: CalcVersionAssigned, v15: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10], ev11: HasRepository[T11], ev12: HasRepository[T12], ev13: HasRepository[T13], ev14: HasRepository[T14], ev15: HasRepository[T15]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
      calcRepository.logInput(InputRecord[T11](calcVersion = calcVersionAssigned, inputCalc = this.v11))
      calcRepository.logInput(InputRecord[T12](calcVersion = calcVersionAssigned, inputCalc = this.v12))
      calcRepository.logInput(InputRecord[T13](calcVersion = calcVersionAssigned, inputCalc = this.v13))
      calcRepository.logInput(InputRecord[T14](calcVersion = calcVersionAssigned, inputCalc = this.v14))
      calcRepository.logInput(InputRecord[T15](calcVersion = calcVersionAssigned, inputCalc = this.v15))
    }
  }
  trait Calc15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, +R] extends Function15[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], RawResult15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10], v11: VersionedData[T11], v12: VersionedData[T12], v13: VersionedData[T13], v14: VersionedData[T14], v15: VersionedData[T15]): RawResult15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] = {
      RawResult15(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        v11.version,
        v12.version,
        v13.version,
        v14.version,
        v15.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data, v11.data, v12.data, v13.data, v14.data, v15.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, v11: CalcVersionAssigned, v12: CalcVersionAssigned, v13: CalcVersionAssigned, v14: CalcVersionAssigned, v15: CalcVersionAssigned, v16: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10], ev11: HasRepository[T11], ev12: HasRepository[T12], ev13: HasRepository[T13], ev14: HasRepository[T14], ev15: HasRepository[T15], ev16: HasRepository[T16]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
      calcRepository.logInput(InputRecord[T11](calcVersion = calcVersionAssigned, inputCalc = this.v11))
      calcRepository.logInput(InputRecord[T12](calcVersion = calcVersionAssigned, inputCalc = this.v12))
      calcRepository.logInput(InputRecord[T13](calcVersion = calcVersionAssigned, inputCalc = this.v13))
      calcRepository.logInput(InputRecord[T14](calcVersion = calcVersionAssigned, inputCalc = this.v14))
      calcRepository.logInput(InputRecord[T15](calcVersion = calcVersionAssigned, inputCalc = this.v15))
      calcRepository.logInput(InputRecord[T16](calcVersion = calcVersionAssigned, inputCalc = this.v16))
    }
  }
  trait Calc16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, +R] extends Function16[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], RawResult16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10], v11: VersionedData[T11], v12: VersionedData[T12], v13: VersionedData[T13], v14: VersionedData[T14], v15: VersionedData[T15], v16: VersionedData[T16]): RawResult16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] = {
      RawResult16(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        v11.version,
        v12.version,
        v13.version,
        v14.version,
        v15.version,
        v16.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data, v11.data, v12.data, v13.data, v14.data, v15.data, v16.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, v11: CalcVersionAssigned, v12: CalcVersionAssigned, v13: CalcVersionAssigned, v14: CalcVersionAssigned, v15: CalcVersionAssigned, v16: CalcVersionAssigned, v17: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10], ev11: HasRepository[T11], ev12: HasRepository[T12], ev13: HasRepository[T13], ev14: HasRepository[T14], ev15: HasRepository[T15], ev16: HasRepository[T16], ev17: HasRepository[T17]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
      calcRepository.logInput(InputRecord[T11](calcVersion = calcVersionAssigned, inputCalc = this.v11))
      calcRepository.logInput(InputRecord[T12](calcVersion = calcVersionAssigned, inputCalc = this.v12))
      calcRepository.logInput(InputRecord[T13](calcVersion = calcVersionAssigned, inputCalc = this.v13))
      calcRepository.logInput(InputRecord[T14](calcVersion = calcVersionAssigned, inputCalc = this.v14))
      calcRepository.logInput(InputRecord[T15](calcVersion = calcVersionAssigned, inputCalc = this.v15))
      calcRepository.logInput(InputRecord[T16](calcVersion = calcVersionAssigned, inputCalc = this.v16))
      calcRepository.logInput(InputRecord[T17](calcVersion = calcVersionAssigned, inputCalc = this.v17))
    }
  }
  trait Calc17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, +R] extends Function17[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], VersionedData[T17], RawResult17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10], v11: VersionedData[T11], v12: VersionedData[T12], v13: VersionedData[T13], v14: VersionedData[T14], v15: VersionedData[T15], v16: VersionedData[T16], v17: VersionedData[T17]): RawResult17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] = {
      RawResult17(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        v11.version,
        v12.version,
        v13.version,
        v14.version,
        v15.version,
        v16.version,
        v17.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data, v11.data, v12.data, v13.data, v14.data, v15.data, v16.data, v17.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, v11: CalcVersionAssigned, v12: CalcVersionAssigned, v13: CalcVersionAssigned, v14: CalcVersionAssigned, v15: CalcVersionAssigned, v16: CalcVersionAssigned, v17: CalcVersionAssigned, v18: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10], ev11: HasRepository[T11], ev12: HasRepository[T12], ev13: HasRepository[T13], ev14: HasRepository[T14], ev15: HasRepository[T15], ev16: HasRepository[T16], ev17: HasRepository[T17], ev18: HasRepository[T18]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
      calcRepository.logInput(InputRecord[T11](calcVersion = calcVersionAssigned, inputCalc = this.v11))
      calcRepository.logInput(InputRecord[T12](calcVersion = calcVersionAssigned, inputCalc = this.v12))
      calcRepository.logInput(InputRecord[T13](calcVersion = calcVersionAssigned, inputCalc = this.v13))
      calcRepository.logInput(InputRecord[T14](calcVersion = calcVersionAssigned, inputCalc = this.v14))
      calcRepository.logInput(InputRecord[T15](calcVersion = calcVersionAssigned, inputCalc = this.v15))
      calcRepository.logInput(InputRecord[T16](calcVersion = calcVersionAssigned, inputCalc = this.v16))
      calcRepository.logInput(InputRecord[T17](calcVersion = calcVersionAssigned, inputCalc = this.v17))
      calcRepository.logInput(InputRecord[T18](calcVersion = calcVersionAssigned, inputCalc = this.v18))
    }
  }
  trait Calc18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, +R] extends Function18[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], VersionedData[T17], VersionedData[T18], RawResult18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10], v11: VersionedData[T11], v12: VersionedData[T12], v13: VersionedData[T13], v14: VersionedData[T14], v15: VersionedData[T15], v16: VersionedData[T16], v17: VersionedData[T17], v18: VersionedData[T18]): RawResult18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] = {
      RawResult18(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        v11.version,
        v12.version,
        v13.version,
        v14.version,
        v15.version,
        v16.version,
        v17.version,
        v18.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data, v11.data, v12.data, v13.data, v14.data, v15.data, v16.data, v17.data, v18.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, v11: CalcVersionAssigned, v12: CalcVersionAssigned, v13: CalcVersionAssigned, v14: CalcVersionAssigned, v15: CalcVersionAssigned, v16: CalcVersionAssigned, v17: CalcVersionAssigned, v18: CalcVersionAssigned, v19: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10], ev11: HasRepository[T11], ev12: HasRepository[T12], ev13: HasRepository[T13], ev14: HasRepository[T14], ev15: HasRepository[T15], ev16: HasRepository[T16], ev17: HasRepository[T17], ev18: HasRepository[T18], ev19: HasRepository[T19]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
      calcRepository.logInput(InputRecord[T11](calcVersion = calcVersionAssigned, inputCalc = this.v11))
      calcRepository.logInput(InputRecord[T12](calcVersion = calcVersionAssigned, inputCalc = this.v12))
      calcRepository.logInput(InputRecord[T13](calcVersion = calcVersionAssigned, inputCalc = this.v13))
      calcRepository.logInput(InputRecord[T14](calcVersion = calcVersionAssigned, inputCalc = this.v14))
      calcRepository.logInput(InputRecord[T15](calcVersion = calcVersionAssigned, inputCalc = this.v15))
      calcRepository.logInput(InputRecord[T16](calcVersion = calcVersionAssigned, inputCalc = this.v16))
      calcRepository.logInput(InputRecord[T17](calcVersion = calcVersionAssigned, inputCalc = this.v17))
      calcRepository.logInput(InputRecord[T18](calcVersion = calcVersionAssigned, inputCalc = this.v18))
      calcRepository.logInput(InputRecord[T19](calcVersion = calcVersionAssigned, inputCalc = this.v19))
    }
  }
  trait Calc19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, +R] extends Function19[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], VersionedData[T17], VersionedData[T18], VersionedData[T19], RawResult19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10], v11: VersionedData[T11], v12: VersionedData[T12], v13: VersionedData[T13], v14: VersionedData[T14], v15: VersionedData[T15], v16: VersionedData[T16], v17: VersionedData[T17], v18: VersionedData[T18], v19: VersionedData[T19]): RawResult19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] = {
      RawResult19(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        v11.version,
        v12.version,
        v13.version,
        v14.version,
        v15.version,
        v16.version,
        v17.version,
        v18.version,
        v19.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data, v11.data, v12.data, v13.data, v14.data, v15.data, v16.data, v17.data, v18.data, v19.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, v11: CalcVersionAssigned, v12: CalcVersionAssigned, v13: CalcVersionAssigned, v14: CalcVersionAssigned, v15: CalcVersionAssigned, v16: CalcVersionAssigned, v17: CalcVersionAssigned, v18: CalcVersionAssigned, v19: CalcVersionAssigned, v20: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10], ev11: HasRepository[T11], ev12: HasRepository[T12], ev13: HasRepository[T13], ev14: HasRepository[T14], ev15: HasRepository[T15], ev16: HasRepository[T16], ev17: HasRepository[T17], ev18: HasRepository[T18], ev19: HasRepository[T19], ev20: HasRepository[T20]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
      calcRepository.logInput(InputRecord[T11](calcVersion = calcVersionAssigned, inputCalc = this.v11))
      calcRepository.logInput(InputRecord[T12](calcVersion = calcVersionAssigned, inputCalc = this.v12))
      calcRepository.logInput(InputRecord[T13](calcVersion = calcVersionAssigned, inputCalc = this.v13))
      calcRepository.logInput(InputRecord[T14](calcVersion = calcVersionAssigned, inputCalc = this.v14))
      calcRepository.logInput(InputRecord[T15](calcVersion = calcVersionAssigned, inputCalc = this.v15))
      calcRepository.logInput(InputRecord[T16](calcVersion = calcVersionAssigned, inputCalc = this.v16))
      calcRepository.logInput(InputRecord[T17](calcVersion = calcVersionAssigned, inputCalc = this.v17))
      calcRepository.logInput(InputRecord[T18](calcVersion = calcVersionAssigned, inputCalc = this.v18))
      calcRepository.logInput(InputRecord[T19](calcVersion = calcVersionAssigned, inputCalc = this.v19))
      calcRepository.logInput(InputRecord[T20](calcVersion = calcVersionAssigned, inputCalc = this.v20))
    }
  }
  trait Calc20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, +R] extends Function20[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], VersionedData[T17], VersionedData[T18], VersionedData[T19], VersionedData[T20], RawResult20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10], v11: VersionedData[T11], v12: VersionedData[T12], v13: VersionedData[T13], v14: VersionedData[T14], v15: VersionedData[T15], v16: VersionedData[T16], v17: VersionedData[T17], v18: VersionedData[T18], v19: VersionedData[T19], v20: VersionedData[T20]): RawResult20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] = {
      RawResult20(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        v11.version,
        v12.version,
        v13.version,
        v14.version,
        v15.version,
        v16.version,
        v17.version,
        v18.version,
        v19.version,
        v20.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data, v11.data, v12.data, v13.data, v14.data, v15.data, v16.data, v17.data, v18.data, v19.data, v20.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, v11: CalcVersionAssigned, v12: CalcVersionAssigned, v13: CalcVersionAssigned, v14: CalcVersionAssigned, v15: CalcVersionAssigned, v16: CalcVersionAssigned, v17: CalcVersionAssigned, v18: CalcVersionAssigned, v19: CalcVersionAssigned, v20: CalcVersionAssigned, v21: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10], ev11: HasRepository[T11], ev12: HasRepository[T12], ev13: HasRepository[T13], ev14: HasRepository[T14], ev15: HasRepository[T15], ev16: HasRepository[T16], ev17: HasRepository[T17], ev18: HasRepository[T18], ev19: HasRepository[T19], ev20: HasRepository[T20], ev21: HasRepository[T21]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
      calcRepository.logInput(InputRecord[T11](calcVersion = calcVersionAssigned, inputCalc = this.v11))
      calcRepository.logInput(InputRecord[T12](calcVersion = calcVersionAssigned, inputCalc = this.v12))
      calcRepository.logInput(InputRecord[T13](calcVersion = calcVersionAssigned, inputCalc = this.v13))
      calcRepository.logInput(InputRecord[T14](calcVersion = calcVersionAssigned, inputCalc = this.v14))
      calcRepository.logInput(InputRecord[T15](calcVersion = calcVersionAssigned, inputCalc = this.v15))
      calcRepository.logInput(InputRecord[T16](calcVersion = calcVersionAssigned, inputCalc = this.v16))
      calcRepository.logInput(InputRecord[T17](calcVersion = calcVersionAssigned, inputCalc = this.v17))
      calcRepository.logInput(InputRecord[T18](calcVersion = calcVersionAssigned, inputCalc = this.v18))
      calcRepository.logInput(InputRecord[T19](calcVersion = calcVersionAssigned, inputCalc = this.v19))
      calcRepository.logInput(InputRecord[T20](calcVersion = calcVersionAssigned, inputCalc = this.v20))
      calcRepository.logInput(InputRecord[T21](calcVersion = calcVersionAssigned, inputCalc = this.v21))
    }
  }
  trait Calc21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, +R] extends Function21[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], VersionedData[T17], VersionedData[T18], VersionedData[T19], VersionedData[T20], VersionedData[T21], RawResult21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10], v11: VersionedData[T11], v12: VersionedData[T12], v13: VersionedData[T13], v14: VersionedData[T14], v15: VersionedData[T15], v16: VersionedData[T16], v17: VersionedData[T17], v18: VersionedData[T18], v19: VersionedData[T19], v20: VersionedData[T20], v21: VersionedData[T21]): RawResult21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] = {
      RawResult21(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        v11.version,
        v12.version,
        v13.version,
        v14.version,
        v15.version,
        v16.version,
        v17.version,
        v18.version,
        v19.version,
        v20.version,
        v21.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data, v11.data, v12.data, v13.data, v14.data, v15.data, v16.data, v17.data, v18.data, v19.data, v20.data, v21.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

  case class RawResult22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, +R](v1: CalcVersionAssigned, v2: CalcVersionAssigned, v3: CalcVersionAssigned, v4: CalcVersionAssigned, v5: CalcVersionAssigned, v6: CalcVersionAssigned, v7: CalcVersionAssigned, v8: CalcVersionAssigned, v9: CalcVersionAssigned, v10: CalcVersionAssigned, v11: CalcVersionAssigned, v12: CalcVersionAssigned, v13: CalcVersionAssigned, v14: CalcVersionAssigned, v15: CalcVersionAssigned, v16: CalcVersionAssigned, v17: CalcVersionAssigned, v18: CalcVersionAssigned, v19: CalcVersionAssigned, v20: CalcVersionAssigned, v21: CalcVersionAssigned, v22: CalcVersionAssigned, r: UnversionedData[R]) extends RawResult[R]
  {
    def logInputs(calcVersionAssigned: CalcVersionAssigned)(implicit calcRepository: CalcRepository, ev1: HasRepository[T1], ev2: HasRepository[T2], ev3: HasRepository[T3], ev4: HasRepository[T4], ev5: HasRepository[T5], ev6: HasRepository[T6], ev7: HasRepository[T7], ev8: HasRepository[T8], ev9: HasRepository[T9], ev10: HasRepository[T10], ev11: HasRepository[T11], ev12: HasRepository[T12], ev13: HasRepository[T13], ev14: HasRepository[T14], ev15: HasRepository[T15], ev16: HasRepository[T16], ev17: HasRepository[T17], ev18: HasRepository[T18], ev19: HasRepository[T19], ev20: HasRepository[T20], ev21: HasRepository[T21], ev22: HasRepository[T22]): Try[Unit] = {
      calcRepository.logInput(InputRecord[T1](calcVersion = calcVersionAssigned, inputCalc = this.v1))
      calcRepository.logInput(InputRecord[T2](calcVersion = calcVersionAssigned, inputCalc = this.v2))
      calcRepository.logInput(InputRecord[T3](calcVersion = calcVersionAssigned, inputCalc = this.v3))
      calcRepository.logInput(InputRecord[T4](calcVersion = calcVersionAssigned, inputCalc = this.v4))
      calcRepository.logInput(InputRecord[T5](calcVersion = calcVersionAssigned, inputCalc = this.v5))
      calcRepository.logInput(InputRecord[T6](calcVersion = calcVersionAssigned, inputCalc = this.v6))
      calcRepository.logInput(InputRecord[T7](calcVersion = calcVersionAssigned, inputCalc = this.v7))
      calcRepository.logInput(InputRecord[T8](calcVersion = calcVersionAssigned, inputCalc = this.v8))
      calcRepository.logInput(InputRecord[T9](calcVersion = calcVersionAssigned, inputCalc = this.v9))
      calcRepository.logInput(InputRecord[T10](calcVersion = calcVersionAssigned, inputCalc = this.v10))
      calcRepository.logInput(InputRecord[T11](calcVersion = calcVersionAssigned, inputCalc = this.v11))
      calcRepository.logInput(InputRecord[T12](calcVersion = calcVersionAssigned, inputCalc = this.v12))
      calcRepository.logInput(InputRecord[T13](calcVersion = calcVersionAssigned, inputCalc = this.v13))
      calcRepository.logInput(InputRecord[T14](calcVersion = calcVersionAssigned, inputCalc = this.v14))
      calcRepository.logInput(InputRecord[T15](calcVersion = calcVersionAssigned, inputCalc = this.v15))
      calcRepository.logInput(InputRecord[T16](calcVersion = calcVersionAssigned, inputCalc = this.v16))
      calcRepository.logInput(InputRecord[T17](calcVersion = calcVersionAssigned, inputCalc = this.v17))
      calcRepository.logInput(InputRecord[T18](calcVersion = calcVersionAssigned, inputCalc = this.v18))
      calcRepository.logInput(InputRecord[T19](calcVersion = calcVersionAssigned, inputCalc = this.v19))
      calcRepository.logInput(InputRecord[T20](calcVersion = calcVersionAssigned, inputCalc = this.v20))
      calcRepository.logInput(InputRecord[T21](calcVersion = calcVersionAssigned, inputCalc = this.v21))
      calcRepository.logInput(InputRecord[T22](calcVersion = calcVersionAssigned, inputCalc = this.v22))
    }
  }
  trait Calc22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, +R] extends Function22[VersionedData[T1], VersionedData[T2], VersionedData[T3], VersionedData[T4], VersionedData[T5], VersionedData[T6], VersionedData[T7], VersionedData[T8], VersionedData[T9], VersionedData[T10], VersionedData[T11], VersionedData[T12], VersionedData[T13], VersionedData[T14], VersionedData[T15], VersionedData[T16], VersionedData[T17], VersionedData[T18], VersionedData[T19], VersionedData[T20], VersionedData[T21], VersionedData[T22], RawResult22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]] with Calc[R] {
    def f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R
    def apply(v1: VersionedData[T1], v2: VersionedData[T2], v3: VersionedData[T3], v4: VersionedData[T4], v5: VersionedData[T5], v6: VersionedData[T6], v7: VersionedData[T7], v8: VersionedData[T8], v9: VersionedData[T9], v10: VersionedData[T10], v11: VersionedData[T11], v12: VersionedData[T12], v13: VersionedData[T13], v14: VersionedData[T14], v15: VersionedData[T15], v16: VersionedData[T16], v17: VersionedData[T17], v18: VersionedData[T18], v19: VersionedData[T19], v20: VersionedData[T20], v21: VersionedData[T21], v22: VersionedData[T22]): RawResult22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R] = {
      RawResult22(
        v1.version,
        v2.version,
        v3.version,
        v4.version,
        v5.version,
        v6.version,
        v7.version,
        v8.version,
        v9.version,
        v10.version,
        v11.version,
        v12.version,
        v13.version,
        v14.version,
        v15.version,
        v16.version,
        v17.version,
        v18.version,
        v19.version,
        v20.version,
        v21.version,
        v22.version,
        UnversionedData(f(v1.data, v2.data, v3.data, v4.data, v5.data, v6.data, v7.data, v8.data, v9.data, v10.data, v11.data, v12.data, v13.data, v14.data, v15.data, v16.data, v17.data, v18.data, v19.data, v20.data, v21.data, v22.data), CalcUnversioned(CalcName(this.name)))
      )
    }

  }

}

object Verbose22Agg {

  //  import shapeless

  /**
    * May be useful:
    * Type class witnessing that every element of `L` is a subtype of `B`.
    * BasicConstraint, LUBConstraint
    *
    * https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/hlistconstraints.scala
    */
  //  case class Agg[Inputs, Outputs, Hierarchy](inputs: Int)
  //                                            (implicit val c: LUBConstraint[L, Seq[_]])

  //  case class AggOutput[+T](a: Agg, output: VersionedData[T])
  //   implicit object AggIsMonoid extends Monoid[Agg]

  //  object Calc {
  //    def persist(): Unit = ???
  //  }
  //  trait Calc1[-T1, +R] extends Function1[VersionedData[T1], AggOutput[R]]
  //  trait Calc2[-T1, -T2, +R] extends Function2[VersionedData[T1], VersionedData[T2], AggOutput[R]]
}

