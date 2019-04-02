package calcs.archive;

//object SimpleState extends App {
//
//  case class MyTrades(trades: String) {
//    def next = MyTrades(trades + "again")
//  }
//
//  val nextLong: State[MyTrades, Int] = State(myTrades =>
//    (myTrades.next, myTrades.trades.length))
//
//
//  val chainedTrades: State[MyTrades, Int] = for {
//    _ <- nextLong
//    b <- nextLong
//    _ = println(b)
//    c <- nextLong
//  } yield c
//
//  println(chainedTrades.run(MyTrades("hey")).value)
//
//}
