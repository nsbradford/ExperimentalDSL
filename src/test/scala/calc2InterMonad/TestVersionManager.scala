package calc2InterMonad

import org.joda.time.DateTime

import Syntax._


class TestVersionManager(override val asOfDate: DateTime,
                         preloaded: Seq[CalcRun] = Seq())
  extends VersionManager
{
  override def requisitionNewVersion(repositoryName: RepositoryName): Version = {
    runTable synchronized{
      val maxSoFar: Long = runTable.map(_.version.id).maxOpt.getOrElse(0l)
      val newVersion = Version(maxSoFar + 1)
      runTable = runTable :+ CalcRun(repositoryName, newVersion, asOfDate)
      newVersion
    }
  }

  override def logInputs(version: Version, inputs: Set[Version]): Unit = {

  }

  override def getLatest(repositoryName: RepositoryName): Option[CalcRun] = {
    runTable.filter(_.repositoryName == repositoryName).maxByOpt(_.asOf)
  }
  override def getInputs(version: Version): Set[Version] = inputTable(version)

  override def getAllRunsOf(repositoryName: RepositoryName): Set[CalcRun] = {
    runTable.filter(_.repositoryName == repositoryName).toSet
  }

  private var runTable: Seq[CalcRun] = Seq()
  private var inputTable: Map[Version, Set[Version]] = Map()
}
