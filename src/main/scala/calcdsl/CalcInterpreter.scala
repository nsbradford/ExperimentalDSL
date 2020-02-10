package calcdsl


import scala.language.higherKinds

trait CalcInterpreter{
  type Result[A] //<: cats.Monad[A]

  def execute[A](fa: Calc.PureCalc[A]): Result[A]
//  def execute[A, B](fa: Calc.MergeCalc[A, B]): Result[(A, B)]
  def execute[A, B](fa: Calc.FlatMapCalc[A, B]): Result[B]
//  def execute[A](fa: ReadCalc[A]): Result[A]
  def execute[A](fa: OutputCalc[A]): Result[A]
}

object CalcInterpreter {
  type Aux[R[_]] = CalcInterpreter{ type Result[A] = R[A] }
}
