package calcs.calcasfunction.smartdomain

import scala.util.{Success, Try}
import scala.util.Try
import scala.language.higherKinds
//import cats.effect.IO

// directly from package calcs.calcasfunction.prototype2

/**
  * A producer of data.
  */
case class CalcName(name: String)
object CalcName{
}

/**
  * A unit of data production.
  */
case class CalcRun(calcName: CalcName, calcRunId: Long)
object CalcRun {
  private val anonymous: CalcName = CalcName("Anonymous")
  private val unknownRunId: Long = -1
  val unknown: CalcRun = CalcRun(anonymous, unknownRunId)
  def apply(name: String, calcRunId: Long): CalcRun = CalcRun(CalcName(name), calcRunId)
}


// =========================================================================================================
// Versions
// =========================================================================================================

/**
  * Carry contextual metadata along with the data.
  */
sealed trait Contextful[+T]{
  def get: T
  //  def version: VersionId
  //  def storageId: TypeStorageId
}

/**
  * Standard metadata format.
  */
sealed trait DataWithCalculatorMetadata[+T] extends Contextful[T]

case class Versioned[+T](get: T, version: CalcRun)
  extends DataWithCalculatorMetadata[T]
case class Unversioned[+T](get: T) extends DataWithCalculatorMetadata[T]


/**
  * Type to encode the unsafe middle ground where data has been produced,
  * but not yet persisted to storage.
  *
  * @see [[calcs.calcasfunction.prototype2.Versioned]]
  */
case class VersionedUnpersisted[+T](get: T, version: CalcRun/*, storageId: StorageTypeId*/)
  extends DataWithCalculatorMetadata[T]
{
  def toPersisted: Versioned[T] = Versioned(get, version)
}


// =========================================================================================================
// METADATA RECORDS
// =========================================================================================================


/**
  * Metadata records:
  *   Input: CalcVersion used VersionedData
  *   Output: CalcVersion produced [Type]
  *   Equivalence/Hierarchy: CalcVersion output [Type] == other CalcVersion of same type
  */
case class RunRecord(calcVersion: CalcRun) // add date, isSuccessful, isValid
case class InputRecord(calcVersion: CalcRun, inputCalc: CalcRun, dataType: StorageTypeRepresentation)
case class OutputRecord(calcVersion: CalcRun)
//case class HierarchyRecord(upper: CalcRun, inner: CalcRun)


/**
  * Later, abstract over monad M[_] instead of tying to Try.
  */

trait Hydrator[T]{
  def hydrate(version: CalcRun): Try[T]
  def hydrateLatestValid(): Try[Versioned[T]]
}

trait Persister[T]{
  def dbTypeRepr: StorageTypeRepresentation

  protected def persist(t: VersionedUnpersisted[T]): Try[Unit]

  final def persistWrap(t: VersionedUnpersisted[T]): Try[Versioned[T]] = {
    for (_ <- this.persist(t)) yield t.toPersisted
  }
}


object Persister {
  def apply[T](implicit ev: Persister[T]): Persister[T] = ev
  def create[T](repr: String, f: (VersionedUnpersisted[T]) => Try[Unit]): Persister[T] = {
    new Persister[T]{
      protected def persist(t: VersionedUnpersisted[T]): Try[Unit] = f(t)
      override val dbTypeRepr: StorageTypeRepresentation = StorageTypeRepresentation(repr)
    }
  }
}

trait CalcIdRepository{
  def requisitionNewRunId(calcName: CalcName): Try[CalcRun]
//  def logEquivalence(hierarchyRecord: HierarchyRecord): Try[Unit]
}

trait MetadataRepository {
  def logRun(runRecord: RunRecord): Try[Unit]
  def logInput(inputRecord: InputRecord): Try[Unit]
  def logOutput(outputRecord: OutputRecord): Try[Unit]

  def getLatestRun(calcName: CalcName): Try[CalcRun]
}


//object AnythingUnversionedCanBeAssignedAVersionImplicits {
//  implicit class AnythingUnversionedCanBeAssignedAVersion[T]
//  (t: T)
//  (implicit
//   metadataRepository: MetadataRepository,
//   persistable: Persistable[T]
//  ){
//    def versioned: Try[Versioned[T]] =
//      for {
//        calcRun <- metadataRepository.requisitionNewRunId(CalcRun.unknown.calcName)
//        _ <- metadataRepository.logInput(InputRecord[T](calcRun, inputCalc = CalcRun.unknown))(persistable)
//        _ <- metadataRepository.logOutput(OutputRecord[T](calcRun))(persistable)
//        persistedResult <- persistable.persistWrap(VersionedUnpersisted[T](t, calcRun))
//      } yield persistedResult
//  }
//}
//
//object Unversioned{
//  implicit class AnythingCanBeUnversionedData[T](t: T){
//    def unversioned: Unversioned[T] = Unversioned(t)
//  }
//}