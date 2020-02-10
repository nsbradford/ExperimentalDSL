package calcdsl

import org.joda.time.DateTime

/**
  * Created by nicholasbradford on 1/12/20.
  */
trait VersionManager {

  def asOfDate: DateTime

  // TODO all of these should use Future or other

  // write
  def requisitionNewVersion(repositoryName: RepositoryName): Version
  def logInputs(version: Version, inputs: Set[Version])


  // read
  def getLatest(repositoryName: RepositoryName): Option[CalcRun]
  def getInputs(version: Version): Set[Version]
  def getAllRunsOf(repositoryName: RepositoryName): Set[CalcRun]

}
