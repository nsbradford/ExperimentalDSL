package calcs.calcasfunction.prototype2

import scala.util.Try
import scala.language.higherKinds
import cats.effect.IO

/**
  * Carry contextual metadata along with the data.
  *
  * @tparam T the datatype
  * @tparam VersionId the producer of this data T
  * @tparam TypeStorageId a way to identify type T in storage
  */
sealed trait Contextful[+T]{
  def get: T
//  def version: VersionId
//  def storageId: TypeStorageId
}

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

/**
  * Type information isn't enough - you need a contextual type id.
  * For example, if you have multiple semantic needs to store a primitive,
  * this allows you to differentiate between them.
  */
case class StorageTypeId(name: String)

/**
  * Standard metadata format.
  */
sealed trait DataWithCalculatorMetadata[+T] extends Contextful[T]

/**
  *
  * @param get
  * @param version
  * @param storageId
  * @tparam T
  */
case class Versioned[+T](get: T, version: CalcRun/*, storageId: StorageTypeId*/)
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

/**
  * Metadata records:
  *   Input: CalcVersion used VersionedData
  *   Output: CalcVersion produced [Type]
  *   Equivalence/Hierarchy: CalcVersion output [Type] == other CalcVersion of same type
  */
case class InputRecord[T](calcVersion: CalcRun, inputCalc: CalcRun)
case class OutputRecord[T](calcVersion: CalcRun)
  case class HierarchyRecord[+T](upper: CalcRun, inner: CalcRun)


/**
  * Later, abstract over monad M[_] instead of tying to IO.
  */


trait Hydratable[T]{
  def hydrate(version: CalcRun): IO[T]
  def hydrateLatestValid(): IO[Versioned[T]]
}

trait Persistable[T]{
//  final def fullName: String = s"$dataConceptName: ${this.getClass.getName}" // eliminates collisions
  def dbTypeRepr: String // TODO not sure if we should keep this as part of interface
  protected def persist(t: VersionedUnpersisted[T]): IO[Unit] // just returns the same data

  final def persistWrap(t: VersionedUnpersisted[T]): IO[Versioned[T]] = {
    for (_ <- this.persist(t)) yield t.toPersisted
  }
}

object Persistable {
  def apply[T](implicit ev: Persistable[T]): Persistable[T] = ev
  def create[T](repr: String, f: (VersionedUnpersisted[T]) => IO[Unit]): Persistable[T] = {
    new Persistable[T]{
      protected def persist(t: VersionedUnpersisted[T]): IO[Unit] = f(t)
      override val dbTypeRepr: String = repr
    }
  }
}

trait MetadataRepository {
  def requisitionNewRunId(calcName: CalcName): IO[CalcRun]
  def logInput[T: Persistable](inputRecord: InputRecord[T]): IO[Unit]
  def logOutput[T: Persistable](outputRecord: OutputRecord[T]): IO[Unit]
  def logEquivalence[T: Persistable](hierarchyRecord: HierarchyRecord[T]): IO[Unit]
}


object AnythingUnversionedCanBeAssignedAVersionImplicits {
  implicit class AnythingUnversionedCanBeAssignedAVersion[T]
  (t: T)
  (implicit
   metadataRepository: MetadataRepository,
   persistable: Persistable[T]
  ){
    def versioned: IO[Versioned[T]] =
      for {
        calcRun <- metadataRepository.requisitionNewRunId(CalcRun.unknown.calcName)
        _ <- metadataRepository.logInput(InputRecord[T](calcRun, inputCalc = CalcRun.unknown))(persistable)
        _ <- metadataRepository.logOutput(OutputRecord[T](calcRun))(persistable)
        persistedResult <- persistable.persistWrap(VersionedUnpersisted[T](t, calcRun))
      } yield persistedResult
  }
}

object Unversioned{
  implicit class AnythingCanBeUnversionedData[T](t: T){
    def unversioned: Unversioned[T] = Unversioned(t)
  }
}