package archive.calcs.calcasfunction.smartdomain

import scala.util.Try

// =========================================================================================================
// Repo[_]s
// =========================================================================================================

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

// =========================================================================================================
// Bundles
// =========================================================================================================

trait BinderDomain
  extends IBigBlobDomain
    with BundleImpl[BinderDomain]
{


  // implementation of APIs by redirecting through the Repos
  override def accounts: Map[String, Double] = bankAccountRepo.get.accounts

  // TODO ugly and can fail - better to persist the "getLatest" or "getX" as an instruction...
  import GlobalCalculatorRegistry._
  private lazy val latestBankAccountBalances: CalcRun =
    metadataRepository.getLatestRun(BankAccountBalancesCalc).get
  private lazy val bankAccountRepo: Repo[BankAccountBalances] =
    new BankAccountBalanceRepoImpl(latestBankAccountBalances, metadataRepository)
  // TODO need some way to get bind() to also bind() all the inner Repos...

}


class BinderDomainImpl(val metadataRepository: MetadataRepository)
  extends BinderDomain


// =========================================================================================================
// Demo
// =========================================================================================================

object DemoSmartDomain extends App {

  // Setup infra
  val calcIdRepository = new TestCalcIdRepository
  val metadataRepository = new TestMetadataRepository
  val domain: BinderDomain = new BinderDomainImpl(metadataRepository)

  // pure biz logic
  val netWorthMinusDebt: (Double, IBigBlobDomain) => Double =
    (debt, domain) => domain.accounts.values.sum - debt


  // ordinarily this would go inside a Calculator
  import GlobalCalculatorRegistry._
  val x =
    for {
      calcRun <- this.calcIdRepository.requisitionNewRunId(BankAccountBalancesCalc)
      newDomain = domain.bind(calcRun)
      result = netWorthMinusDebt(4000.0, newDomain)
  //    releasedDomain = domain.unbind
    } yield result // TODO also return ReleasedDomain i.e. State Monad


  // TODO this works, but would require you pass in the copy() method manually to bind()

  trait IFoo {
    def foo: Int
  }
  trait FooImpl extends IFoo {
    override def foo = inner(4)
    val inner = Map(4 -> 8, 5 -> 10)
  }
  case class BundleAPI(stack: Seq[String]) extends FooImpl

  val bundle = BundleAPI(Nil).copy(Seq("NewState"))
  val result = bundle.foo

  val bar = Seq(Seq("hey")).map(bundle.copy)

  val i = 2

  // TODO try to put a common copy() method in the superclass? Might require some reflection, which is a bad path...
  // https://stackoverflow.com/questions/7525142/how-to-programmatically-determine-if-the-class-is-a-case-class-or-a-simple-class
  // https://stackoverflow.com/questions/5827510/how-to-override-apply-in-a-case-class-companion?rq=1

  trait CanCopy[T] {
    this: T =>
    def copy(s: String): T
  }

  // TODO this sorta works if your Bundle is always a case class, can get working with a bit of boilerplate
  case class Leaf(inner: String) extends CanCopy[Leaf] {
    override def copy(s: String): Leaf = this.copy(s)
  }


  // TODO does copy() really share data magically under the hood?
  // Answer: No, it creates a whole new instance so you have to re-evaluate cached results.
  trait HasExpensiveCachedCalc[T]{
    this: T =>
    def copyWith(s: String): T
    def bulkLazy: Int = lazyStuff
    private lazy val lazyStuff: Int = {
      println("lazyStuff about to execute expensive computation...")
      5
    }
  }

  case class ConcreteCached(inner: String) extends HasExpensiveCachedCalc[ConcreteCached]{
    override def copyWith(s: String): ConcreteCached = this.copy(s)
  }

//  val cc = ConcreteCached("FirstCC")
//  println("first domain about to perform bulk lazy op:")
//  val firstResult = cc.bulkLazy
//  println("first domain about to re-perform bulk lazy op, should print nothing because it's been cached:")
//
//  val cc2 = cc.copy(inner = "SecondCC")
//  println("second domain about to re-perform bulk lazy op, should print nothing because it's been cached:")
//  val secondResult = cc2.bulkLazy


  // TODO ok so if copy() doesn't share data, if we just make the Domain point to separate objects does that work?
  class ExternalCalc(){
    lazy val lazyStuff: Int = {
      println("lazyStuff about to execute expensive computation...")
      5
    }
  }

  trait ExternalCachedCalc[T]{
    this: T =>
    val externalCalc: ExternalCalc
    def copyWith(s: String): T
    def bulkLazy: Int = externalCalc.lazyStuff

  }

  case class ExternalCached(inner: String, externalCalc: ExternalCalc)
    extends ExternalCachedCalc[ExternalCached]
  {
    override def copyWith(s: String): ExternalCached = this.copy(inner = s)
  }

  val ec = ConcreteCached("FirstCC")
  println("first domain about to perform bulk lazy op:")
  val EfirstResult = ec.bulkLazy
  println("first domain about to re-perform bulk lazy op, should print nothing because it's been cached:")

  val ec2 = ec.copy(inner = "SecondCC")
  println("second domain about to re-perform bulk lazy op, should print nothing because it's been cached:")
  val EsecondResult = ec2.bulkLazy



}

