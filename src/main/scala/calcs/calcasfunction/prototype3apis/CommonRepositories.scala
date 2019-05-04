package calcs.calcasfunction.prototype3apis

import scala.util.Try


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

class TestMetadataRepository extends MetadataRepository {

  override def logInput(inputRecord: InputRecord): Try[Unit] = Try {
    println(s"CalcRepository > log INPUT > $inputRecord}")
  }

  override def logOutput(outputRecord: OutputRecord): Try[Unit] = Try {
    println(s"CalcRepository > log OUTPUT > $outputRecord")
  }

  //    override def logEquivalence[T: Persistable](hierarchyRecord: HierarchyRecord[T]): IO[Unit] = IO {
  //      println(s"CalcRepository > log EQUIVALENCE > $hierarchyRecord of type: {${Persistable[T].dbTypeRepr}}")
  //    }

  override def logRun(runRecord: RunRecord): Try[Unit] = Try {
    println(s"CalcRepository > log RUNRECORD > $runRecord")
    runTable = runTable :+ runRecord
  }

  override def getLatestRun(calcName: CalcName): Try[CalcRun] = Try{
    runTable.filter(_.calcVersion.calcName == calcName).maxBy(_.calcVersion.calcRunId).calcVersion
  }

  var outputTable: scala.collection.mutable.Seq[OutputRecord] = scala.collection.mutable.Seq()
  var inputTable: scala.collection.mutable.Seq[InputRecord] = scala.collection.mutable.Seq()
  var runTable: scala.collection.mutable.Seq[RunRecord] = scala.collection.mutable.Seq()
}


object CommonRepositories {

  implicit val calcIdRepository: CalcIdRepository = new TestCalcIdRepository
  implicit val metadataRepository: MetadataRepository = new TestMetadataRepository

  implicit val IntHasRepository: Persistable[Int] = Persistable.create[Int](
    "Int-Std",
    x => Try(println(s"Persisted Int: $x"))
  )
  implicit val StringHasRepository: Persistable[String] = Persistable.create[String](
    "String-Std",
    x => Try(println(s"Persisted String: $x"))
  )
}

