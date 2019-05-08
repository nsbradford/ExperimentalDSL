package calcs.calcasdata.Versioned


import scala.annotation.tailrec
import scala.language.higherKinds

// TODO(nick.bradford) instead of having function Option[String],
// create a Function[T] which is named,
// and a AssignedVariable[T] which is labelled.


sealed trait V[T] extends Ancestral with VersionedMetadata {
//  final def wrappedAs(s: String): Procedure[T] = Procedure(s, this)
//  final def named(s: String): NamedVariable[T] = NamedVariable(s, this)
  def run(implicit ex: Executor): ex.Result[T]
  def map[B](f: T => B): Map[T, B] = Map(this, f)
  def fmap[B](f: T => V[B]): FMap[T, B] = FMap(this, f)
//  def combine[B](f: T => B): Map[T, B] = Map(this, f)

}

case class Pure[T](raw: T) extends V[T]{
  def run(implicit ex: Executor): ex.Result[T] = ex.run(this)
}

case class LazyV[T](raw: () => T) extends V[T]{
  def run(implicit ex: Executor): ex.Result[T] = ex.run(this)
}

case class Map[A, T](in: V[A],
                     f: A => T) extends V[T]{
  def run(implicit ex: Executor): ex.Result[T] = ex.run(this)
}

case class FMap[A, T](in: V[A],
                      f: A => V[T]) extends V[T]{
  def run(implicit ex: Executor): ex.Result[T] = ex.run(this)
}

case class Combine[A, B, T](a: V[A],
                            b: V[B],
                            f: (A, B) => T) extends V[T]{
  def run(implicit ex: Executor): ex.Result[T] = ex.run(this)
}

/**
  * Both Procedures and NamedVariables are just V[T] + String name,
  *   the difference is that in diagnostics you know Procedures
  *   with the same name link to the same inner function,
  *   whereas Variables with the same name don't mean anything
  *   (are presumably in different namespaces - could enforce with macros).
  *
  * I.e. the rule is that all Functions should return Procedures,
  *   but inside the functions you can assign them names to make them appear
  *   named inside diagnostics.
  */
//case class Procedure[T](name: String, in: V[T]) extends V[T] {
//  override def run = in.run
//}
//case class NamedVariable[T](name: String, in: V[T]) extends V[T] {
//  override def run = in.run
//}

trait Procedure[T] extends V[T] {
  def name: String
  def run(implicit ex: Executor): ex.Result[T] = ex.run(this)
  def inputs: Seq[V[_]] // HLists? Type erasure?
  def result: V[T] // a little confusing that this is a V[T] and also produces something of the same type...
}

trait CProcedure[T] extends Procedure[T] with Product {
  def inputs: Seq[V[_]] = this.productIterator.collect{ case v: V[_] => v}.toSeq
}


class DemoExecutor extends Executor {
  override type Result[T] = T

  override def run[T](pure: Pure[T]): T = pure.raw

  override def run[A, T](map: Map[A, T]): T = {
    map.f(map.in.run(this))
  }

  override def run[A, T](fMap: FMap[A, T]): T = {
    fMap.f(fMap.in.run(this)).run(this)
  }

  override def run[A, B, T](combine: Combine[A, B, T]): T = {
    combine.f(combine.a.run(this), combine.b.run(this))
  }

  override def run[T](procedure: Procedure[T]): T = {
    println(s"Entering procedure ${procedure.name}")
    println(s"logging these inputs: ${procedure.inputs}")
    println(s"Executing...")
    val result = procedure.result.run(this)
    println(s"Publishing result... : $result")
    result
  }
}

object VExecutorDemo extends App {

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
  val assets: Procedure[Double] = Add(cash, debts) // named "Total Assets"
  val adjusted: FMap[Double, Double] = assets.fmap(leverDoubleIfNotNegative)
  val formatted: Map[Double, String] = formattedEquity(adjusted)

  val executor = new DemoExecutor
  println(formatted.run(executor))

}