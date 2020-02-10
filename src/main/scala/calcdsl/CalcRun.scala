package calcdsl

import org.joda.time.DateTime


case class CalcRun(repositoryName: RepositoryName, version: Version, asOf: DateTime)
case class InputRecord(version: Version, inputs: Set[Version])
