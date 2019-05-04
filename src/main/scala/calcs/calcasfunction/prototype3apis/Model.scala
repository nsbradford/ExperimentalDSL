package calcs.calcasfunction.prototype3apis

import scala.util.{Success, Try}
import scala.util.Try
import scala.language.higherKinds
//import cats.effect.IO
import cats.Monad
import cats.effect.IO
import shapeless.ops.function.FnToProduct
import shapeless.syntax.std.function._
import shapeless.{::, Generic, HList, HNil}

import scala.language.higherKinds
import scala.util.Try

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
trait Versioned[+T]{
  def get: T
  def versions: Set[DataVersion]
}


/**
  * An API of type T with many methods, all of which are versioned.
  *
  * Warning: it's up to the user to enforce consistency.
  */
trait VersionedAPI[T <: VersionedAPI[T]] extends Versioned[T] {
  this: T =>
  override def get: T = this
}


/**
  * An API for a single piece of versioned data T.
  */
trait Repository[T] extends Versioned[T]{
  def dataVersion: DataVersion
  override final def versions: Set[DataVersion] = Set(dataVersion)
}


/**
  * An API for an eagerly evaluated piece of data T.
  */
case class VersionedData[T](get: T, dataVersion: DataVersion) extends Repository[T]

/**
  * Type to encode the unsafe middle ground where data has been produced,
  * but not yet persisted to storage.
  *
  * @see [[calcs.calcasfunction.prototype2fullarity.Versioned]]
  */
case class VersionedUnpersisted[T](get: T, dataVersion: DataVersion) extends Repository[T]
{
  def toPersisted: VersionedData[T] = VersionedData(get, dataVersion)
}


/**
  * Trivially meets with Versioned API by requesting not to persist any metadata.
  */
case class UnversionedData[+T](get: T) extends Versioned[T]{
  override lazy val versions = Set()
}


// =========================================================================================================
// METADATA RECORDS
// =========================================================================================================

// TODO use macro or classpath to eliminate collisions
case class StorageTypeRepresentation(name: String)


/**
  * Metadata records:
  *   Input: CalcVersion used VersionedData
  *   Output: CalcVersion produced [Type]
  *   Equivalence/Hierarchy: CalcVersion output [Type] == other CalcVersion of same type
  */
case class DataVersion(inputCalc: CalcRun, dataType: StorageTypeRepresentation)
case class RunRecord(calcVersion: CalcRun) // add date, isSuccessful, isValid
case class InputRecord(calcVersion: CalcRun, inputData: DataVersion)
case class OutputRecord(calcVersion: CalcRun, dataType: StorageTypeRepresentation)
//case class HierarchyRecord(upper: CalcRun, inner: CalcRun)


/**
  * Later, abstract over monad M[_] instead of tying to Try.
  */

trait Hydratable[T]{
  def hydrate(version: CalcRun): Try[T]
  def hydrateLatestValid(): Try[VersionedData[T]]
}

trait Persistable[T]{
  def storageTypeRepresentation: StorageTypeRepresentation
  def attachRepr(data: T, calcRun: CalcRun): VersionedData[T] = {
    VersionedData[T](data, DataVersion(calcRun, storageTypeRepresentation))
  }

  protected def persist(t: VersionedUnpersisted[T]): Try[Unit]

  final def persistWrap(t: VersionedUnpersisted[T]): Try[VersionedData[T]] = {
    for (_ <- this.persist(t)) yield t.toPersisted
  }
}


object Persistable {
  def apply[T](implicit ev: Persistable[T]): Persistable[T] = ev
  def create[T](repr: String, f: (VersionedUnpersisted[T]) => Try[Unit]): Persistable[T] = {
    new Persistable[T]{
      protected def persist(t: VersionedUnpersisted[T]): Try[Unit] = f(t)
      override val storageTypeRepresentation: StorageTypeRepresentation = StorageTypeRepresentation(repr)
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

  // TODO i think need Cats.traverse or Cats.Validated for this conversion to accumulate errors?
  final def logAllInputs(calcRun: CalcRun, inputs: Set[DataVersion]): Try[Unit] = {
    val loggingAttempts = inputs.map(i => logInput(InputRecord(calcRun, i)))
    Success()
  }
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
