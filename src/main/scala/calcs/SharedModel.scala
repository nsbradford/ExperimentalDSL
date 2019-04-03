package calcs

import scala.util.Try


trait SharedModel {

  /**
    * Just a single Try for now; in the future will wrap in Future or IO.
    */
  trait HasRepository[T]{
    final def fullName: String = s"$dataConceptName: ${this.getClass.getName}" // eliminates collisions
    def dataConceptName: String // TODO not sure if we should keep this as part of interface
    def persist(t: VersionedDataUnpersisted[T]): Try[Unit]
    def hydrate(version: CalcVersionAssigned): Try[T]
    def hydrateLatestValid(): Try[VersionedData[T]]
  }

  object HasRepository {
    def apply[T](implicit ev: HasRepository[T]): HasRepository[T] = ev
  }

  /**
    * Data is unversioned upon production, and requires connection to the outside world to assign.
    */
  case class CalcName(s: String)
  sealed trait CalcVersion {
    def calcName: CalcName
  }
  case class CalcVersionAssigned(calcName: CalcName, calcRunId: Long) extends CalcVersion
  case class CalcUnversioned(calcName: CalcName) extends CalcVersion // attach some extra way to prevent two of these from looking equal?

  /**
    * Data always has a version.
    */
  //  trait DataWithVersion
  case class VersionedData[+T](data: T, version: CalcVersionAssigned)
  case class UnversionedData[+T](data: T, version: CalcUnversioned)
  case class VersionedDataUnpersisted[+T](data: T, version: CalcVersionAssigned)

  /**
    * Metadata records:
    *   Input: CalcVersion used VersionedData
    *   Output: CalcVersion produced [Type]
    *   Equivalence/Hierarchy: CalcVersion output [Type] == other CalcVersion of same type
    */
  case class InputRecord[T](calcVersion: CalcVersionAssigned, inputCalc: CalcVersionAssigned)
  case class OutputRecord[T](calcVersion: CalcVersionAssigned)
  //  case class HierarchyRecord[+T](upperCalcVersion: CalcVersion, innerCalcVersion: CalcVersion)

  trait CalcRepository {
    def requisitionNewRunId(calcName: CalcName): Try[CalcVersionAssigned]
    def logInput[T : HasRepository](inputRecord: InputRecord[T]): Try[Unit]
    def logOutput[T : HasRepository](outputRecord: OutputRecord[T]): Try[Unit]
    //    def logEquivalence[T](hierarchyRecord: HierarchyRecord[T]): Unit
  }
}