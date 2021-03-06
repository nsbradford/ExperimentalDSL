package calcdsl.interpeters

import calcdsl.Calc._
import calcdsl._
import cats.Id
import shared.ParMemo


class SyncInterpreter extends CalcInterpreter {
  override type Result[A] = Id[A]

  final override def execute[A](fa: PureCalc[A]): Result[A] =
    fa.f()

  final override def execute[A, B](fa: FlatMapCalc[A, B]): Result[B] = {
    fmapCache(fa).asInstanceOf[Result[B]]
  }

  final override def execute[A](fa: OutputCalc[A]): Result[A] = {
    outputCache(fa).asInstanceOf[Result[A]]
  }

  private def executeFlatMap[A, B](fa: FlatMapCalc[A, B]): Result[B] = {
    val resultA: A = fa.in.runWith(this)
    val resultB: B = fa.f(resultA).runWith(this)
    resultB
  }

  private def executeOutput[A](fa: OutputCalc[A]): Result[A] = {
    fa.in.runWith(this)
//    fa.repository.write() // TODO doesn't work well with non-versioned use case
  }

  private val fmapCache: FlatMapCalc[_, _] => Result[_] = ParMemo{ x => executeFlatMap(x) }
  private val outputCache: OutputCalc[_] => Result[_] = ParMemo{ x => executeOutput(x) }
}




/**
  * Created by nicholasbradford on 1/12/20.
  */
class SyncDiagInterpreter(versionManager: VersionManager,
                          overrides: Map[RepositoryName, Version] = Map())
  extends CalcInterpreter {

  override type Result[A] = VersionedResult[A]

  final override def execute[A](fa: PureCalc[A]): IntermediateVersionedResult[A] =
    IntermediateVersionedResult(fa.f(), Set())

  final override def execute[A, B](fa: FlatMapCalc[A, B]): IntermediateVersionedResult[B] = {
    fmapCache(fa).asInstanceOf[IntermediateVersionedResult[B]]
  }

  final override def execute[A](fa: OutputCalc[A]): Versioned[A] = {
    outputCache(fa).asInstanceOf[Versioned[A]]
  }

  private def executeFlatMap[A, B](fa: FlatMapCalc[A, B]): IntermediateVersionedResult[B] = {
    val resultA: VersionedResult[A] = fa.in.runWith(this)
    val resultB : VersionedResult[B] = fa.f(resultA.get).runWith(this)
    IntermediateVersionedResult(resultB.get, resultA.versions ++ resultB.versions)
  }

  private def executeOutput[A](fa: OutputCalc[A]): Versioned[A] = loadOverrideOrElse(fa){
    val resultA: VersionedResult[A] = fa.in.runWith(this)
    val newVersion = versionManager.requisitionNewVersion(fa.repository.name)
    versionManager.logInputs(newVersion, resultA.versions)
    val result = Versioned(resultA.get, newVersion)
    fa.repository.write(result)
    result
  }

  private def loadOverrideOrElse[R](calc: OutputCalc[R])
                                   (execution: => Versioned[R]): Versioned[R] = {
    overrides.get(calc.repository.name) match {
      case None => execution
      case Some(versionToOverrideWith) => calc.repository.read(versionToOverrideWith).get
    }
  }

  private val fmapCache: FlatMapCalc[_, _] => IntermediateVersionedResult[_] = ParMemo{ x => executeFlatMap(x) }
  private val outputCache: OutputCalc[_] => Versioned[_] = ParMemo{ x => executeOutput(x) }


}
