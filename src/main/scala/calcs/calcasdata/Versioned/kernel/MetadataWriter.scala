package calcs.calcasdata.Versioned.kernel

import scala.util.{Success, Try}



case class RunRecord(calcVersion: CalcRun) // add date, isSuccessful, isValid
case class InputRecord(calcVersion: CalcRun, inputData: DataVersion)
case class OutputRecord(calcVersion: CalcRun, dataType: Address)
//case class HierarchyRecord(upper: CalcRun, inner: CalcRun)

trait MetadataWriter {
  def logRun(runRecord: RunRecord): Try[Unit]
  def logInput(inputRecord: InputRecord): Try[Unit]
  def logOutput(outputRecord: OutputRecord): Try[Unit]
  //  def logEquivalence(hierarchyRecord: HierarchyRecord): Try[Unit]

  // TODO i think need Cats.traverse or Cats.Validated for this conversion to accumulate errors?
  // https://gist.github.com/cb372/458df4a98a2abb29b03f0d6f42298835
  final def logAllInputs(calcRun: CalcRun, inputs: Set[DataVersion]): Try[Unit] = {
    val loggingAttempts = inputs.map(i => logInput(InputRecord(calcRun, i)))
    Success()
  }

  // TODO create a separate MetadataReader
//  def getLatestRun(calcName: CalcName): Try[CalcRun]
}

