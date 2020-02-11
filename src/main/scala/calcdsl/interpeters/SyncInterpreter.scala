package calcdsl.interpeters

import calcdsl.Calc._
import calcdsl._
import shared.ParMemo


/**
  * Created by nicholasbradford on 1/12/20.
  */
class SyncInterpreter(versionManager: VersionManager) extends CalcInterpreter {

  override type Result[A] = VersionedResult[A]

  override def execute[A](fa: PureCalc[A]): IntermediateVersionedResult[A] =
    IntermediateVersionedResult(fa.f(), Set())

  override def execute[A, B](fa: FlatMapCalc[A, B]): IntermediateVersionedResult[B] = {
    fmapCache(fa).asInstanceOf[IntermediateVersionedResult[B]]
  }

  override def execute[A](fa: OutputCalc[A]): Versioned[A] = {
    outputCache(fa).asInstanceOf[Versioned[A]]
  }

  private def executeFlatMap[A, B](fa: FlatMapCalc[A, B]): IntermediateVersionedResult[B] = {
    val resultA: VersionedResult[A] = fa.in.runWith(this)
    val resultB : VersionedResult[B] = fa.f(resultA.get).runWith(this)
    IntermediateVersionedResult(resultB.get, resultA.versions ++ resultB.versions)
  }

  private def executeOutput[A](fa: OutputCalc[A]): Versioned[A] = {
    val resultA: VersionedResult[A] = fa.in.runWith(this)
    val newVersion = versionManager.requisitionNewVersion(fa.repository.name)
    versionManager.logInputs(newVersion, resultA.versions)
    val result = Versioned(resultA.get, newVersion)
    fa.repository.write(result)
    result
  }


  private val outputCache: OutputCalc[_] => Versioned[_] = ParMemo{ x => executeOutput(x) }
  private val fmapCache: FlatMapCalc[_, _] => IntermediateVersionedResult[_] = ParMemo{ x => executeFlatMap(x) }
}
