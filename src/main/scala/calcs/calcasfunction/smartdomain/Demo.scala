package calcs.calcasfunction.smartdomain

import scala.util.Try

// =========================================================================================================
// The Old World
// =========================================================================================================

trait BankAccountBalances {
  def accounts: Map[String, Double]
}
//trait B {
//  def b: String
//}
//trait C {
//  def c: String
//}
//trait D {
//  def d: String
//}

//trait X {
//  def y: Y
//}
//trait Y {
//  def y: String
//}

trait AImpl extends BankAccountBalances {
  override def accounts: Map[String, Double] = Map("BofA" -> 100d, "Chase" -> 50d)
}

//trait XImpl extends X {
//
//}

trait IBigBlobDomain
  extends BankAccountBalances
//    with B
//    with C
//    with D
//    with X

trait ProductionBigBlobDomain
  extends AImpl
//    with XImpl

object OldDemo extends App {
  val domain = new ProductionBigBlobDomain {}
  val netWorthMinusDebt: (Double, IBigBlobDomain) => Double =
    (debt, domain) => domain.accounts.values.sum - debt

}

// =========================================================================================================
// Versions
// =========================================================================================================


class TestCalcIdRepository extends CalcIdRepository {

  override def requisitionNewRunId(calcName: CalcName): Try[CalcRun] = Try {
    calcIdCounter = calcIdCounter + 1
    CalcRun(calcName, calcIdCounter)
  }
  private var calcIdCounter = 0
}

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

object GlobalCalculatorRegistry {
  val BankAccountBalancesCalc = CalcName("BankAccountBalancesCalc")
}


class BankAccountBalanceRepoImpl(val version: CalcRun,
                                 val metadataRepository: MetadataRepository)
  extends RepoContextImpl[BankAccountBalances]
{
  private val nameStr: String = "Demo-BankAccountBalanceRepoImpl"
  override def name: StorageTypeRepresentation = StorageTypeRepresentation(nameStr)
  override protected def innerGet: BankAccountBalances = new BankAccountBalances {
    override def accounts: Map[String, Double] = Map("BofA" -> 100d, "Chase" -> 50d)
  }
}

trait BinderDomain
  extends IBigBlobDomain
    with BundleImpl[BinderDomain]
{

  // TODO need some way to get bind() to also bind() all the inner Repos...

  // implementation of APIs by redirecting through the Repos
  override def accounts: Map[String, Double] = bankAccountRepo.get.accounts

  // TODO ugly and can fail - better to persist the "getLatest" or "getX" as an instruction...
  import GlobalCalculatorRegistry._
  private val latestBankAccountBalances: CalcRun =
    metadataRepository.getLatestRun(BankAccountBalancesCalc).get
  private val bankAccountRepo: Repo[BankAccountBalances] =
    new BankAccountBalanceRepoImpl(latestBankAccountBalances, metadataRepository)
}

class BinderDomainImpl(val metadataRepository: MetadataRepository)
  extends BinderDomain

object NewDemo extends App {
  val calcIdRepository = new TestCalcIdRepository
  val metadataRepository = new TestMetadataRepository


  val domain: BinderDomain = new BinderDomainImpl(metadataRepository)
  val netWorthMinusDebt: (Double, IBigBlobDomain) => Double =
    (debt, domain) => domain.accounts.values.sum - debt

  import GlobalCalculatorRegistry._

  for {
    calcRun <- this.calcIdRepository.requisitionNewRunId(BankAccountBalancesCalc)
    newDomain: IBigBlobDomain = domain.bind(calcRun) // cast necessary
    result <- netWorthMinusDebt(4000.0, newDomain)
//    releasedDomain = domain.unbind
  } yield ???

}

