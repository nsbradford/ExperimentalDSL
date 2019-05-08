package calcs.calcasdata.Versioned.demo

import calcs.calcasdata.Versioned.kernel._


object BasicDemo extends App {

  case class Add(a: V[Double], b: V[Double]) extends CProcedure[Double] {
    val name = "Add"

    def result = Combine(a, b, (x: Double, y: Double) => x + y)
  }

  case class MultiplyByTwo(v: V[Double]) extends CProcedure[Double] {
    val name = "MultiplyByTwo"

    def result = v.map(_ * 2)
  }

  def leverDoubleIfNotNegative(a: Double): V[Double] = {
    a match {
      case 0.0 => Pure(0.0)
      case x => MultiplyByTwo(Pure(x))
    }
  }

  def formattedEquity[A](a: V[A]): Map[A, String] = {
    a.map(_.toString + "B USD")
  }

  val debts = Pure(8d)
  val cash = Pure(10d)
  val assets: Procedure[Double] = Add(cash, debts)
  // named "Total Assets"
  val adjusted: FMap[Double, Double] = assets.fmap(leverDoubleIfNotNegative)
  val formatted: Map[Double, String] = formattedEquity(adjusted)

  val executor = new DemoExecutor
  println(formatted.run(executor))
}
