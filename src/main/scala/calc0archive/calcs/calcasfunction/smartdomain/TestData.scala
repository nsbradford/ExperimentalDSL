package archive.calcs.calcasfunction.smartdomain

import scala.util.Try


/**
  *
  */
object GlobalCalculatorRegistry {
  val BankAccountBalancesCalc = CalcName("BankAccountBalancesCalc")
}

class TestCalcIdRepository extends CalcIdRepository {

  override def requisitionNewRunId(calcName: CalcName): Try[CalcRun] = Try {
    calcIdCounter = calcIdCounter + 1
    CalcRun(calcName, calcIdCounter)
  }
  private var calcIdCounter = 0
}


/**
  *
  */
class TestMetadataRepository extends MetadataRepository {

  override def logInput(inputRecord: InputRecord): Try[Unit] = Try {
    //    println(s"CalcRepository > log INPUT > $inputRecord")
    inputTable = inputTable :+ inputRecord
  }

  override def logOutput(outputRecord: OutputRecord): Try[Unit] = Try {
    //    println(s"CalcRepository > log OUTPUT > $outputRecord")
    outputTable = outputTable :+ outputRecord
  }

  //  override def logEquivalence(hierarchyRecord: HierarchyRecord): Try[Unit] = Try {
  //    println(s"CalcRepository > log EQUIVALENCE > $hierarchyRecord")
  //  }

  override def logRun(runRecord: RunRecord): Try[Unit] = Try {
    //    println(s"CalcRepository > log OUTPUT > $outputRecord")
    runTable = runTable :+ runRecord
  }

  override def getLatestRun(calcName: CalcName): Try[CalcRun] = Try{
    runTable.filter(_.calcVersion.calcName == calcName).maxBy(_.calcVersion.calcRunId).calcVersion
  }

  var outputTable: scala.collection.mutable.Seq[OutputRecord] =
    scala.collection.mutable.Seq()
  var inputTable: scala.collection.mutable.Seq[InputRecord] =
    scala.collection.mutable.Seq()
  var runTable: scala.collection.mutable.Seq[RunRecord] =
    scala.collection.mutable.Seq()
}

