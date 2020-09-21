package archive.calcs.calcasdata.Versioned.kernel

import scala.language.higherKinds
import scala.util.Try


trait VersionedMetadata {

  // TODO ideally is a single DataVersion; is currently a Set to support VersionedAPI
  def versions: Set[DataVersion]
}



/**
  * TODO the holy grail is to be able to have this handle 100% of versioning and persistence,
  *   while having the DSL know nothing about versions.
  */
trait VExecutor extends Executor {

//  type Result[T] <: VersionedMetadata // TODO probably won't work
  type Result[T] = Try[T]

  val calcIdRepository: CalcIdRepository
  val metadataRepository: MetadataWriter

  override def run[T](pure: Pure[T]): Result[T] = ???

  override def run[T](lazyV: LazyV[T]): Result[T] = ???

  override def run[A, T](map: Map[A, T]): Result[T] = ???

  override def run[A, T](fMap: FMap[A, T]): Result[T] = ???

  override def run[A, B, T](combine: Combine[A, B, T]): Result[T] = ???

  override def run[T](procedure: Procedure[T]): Result[T] = {
    for {
//      calcRun <- this.calcIdRepository.requisitionNewRunId(procedure.name)
      result <- procedure.run(this)

    //      allInputVersions = versionedDataInput.toList.toSet[Versioned[Any]].flatMap(_.versions)
    //      _ <- metadataRepository.logAllInputs(calcRun, allInputVersions)

    } yield result
  }
}