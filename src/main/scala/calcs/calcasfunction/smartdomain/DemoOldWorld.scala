package calcs.calcasfunction.smartdomain



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

object DemoOldWorld extends App {
  val domain = new ProductionBigBlobDomain {}
  val netWorthMinusDebt: (Double, IBigBlobDomain) => Double =
    (debt, domain) => domain.accounts.values.sum - debt

}
