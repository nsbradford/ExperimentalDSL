package calcs.calcasdata.Ver

import scala.annotation.tailrec
import scala.language.higherKinds

// TODO(nick.bradford) instead of having function Option[String],
// create a Function[T] which is named,
// and a AssignedVariable[T] which is labelled.

sealed trait V[T]{
  final def wrappedAs(s: String): Procedure[T] = Procedure(s, this)
  final def named(s: String): NamedVariable[T] = NamedVariable(s, this)
  def run: T
//  def run[X](context: X, state: (X, V[Any]) => X): (X, T)
//  def runContextless: (Unit, T) = run[Unit]((), (_, _) => ())
  //  def interpret[X[_] <: V[_]](f: X[T] => T): T
}

case class Pure[T](raw: T,
                   function: Option[String]=None) extends V[T]{
  def run = raw
//  override def run[X](context: X, state: (X, V[Any]) => X): (X, T) = {
//    val myState: X = state(context, in)
//    (myState, in)
//  }
}

case class Map[A, T](in: V[A],
                     f: A => T,
                     function: Option[String]=None) extends V[T]{
  def run = f(in.run)
//  override def run[X](context: X,
//                      state: (X, V[Any]) => X): (X, T) = {
//    val (innerState, result): (X, A) = in.run[X](context, state)
//    val myResult: T = f(result)
//    val myState: X = state(innerState, this)
//    (myState, myResult)
//  }
}

case class FMap[A, T](in: V[A],
                      f: A => V[T],
                      function: Option[String]=None) extends V[T]{
  def run = f(in.run).run
//  override def run[X](context: X, state: (X, V[Any]) => X): (X, T) = {
//    val (innerState, result): (X, A) = in.run[X](context, state)
//    val (flatState, flatResult): (X, T) = f(result).run(innerState, state)
//    val myState: X = state(flatState, flatResult)
//    (myState, flatResult)
//  }
}

case class Combine[A, B, T](a: V[A],
                            b: V[B],
                            f: (A, B) => T,
                            function: Option[String]=None) extends V[T]{
  def run = f(a.run, b.run)
//  override def run[X](context: X, state: (X, V[Any]) => X): (X, T) = {
//    val (innerStateA, resultA): (X, A) = a.run[X](context, state)
//    val (innerStateB, resultB): (X, B) = b.run[X](innerStateA, state)
//    val myResult: T = f(resultA, resultB)
//    val myState: X = state(innerStateB, myResult)
//    (myState, myResult)
//  }
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
case class Procedure[T](name: String, in: V[T]) extends V[T] {
  override def run = in.run
}
case class NamedVariable[T](name: String, in: V[T]) extends V[T] {
  override def run = in.run
}

/**
  *
  */
object VDemo extends App {

  def add(a: V[Double], b: V[Double]): Procedure[Double] = {
    Combine(a, b, (x: Double, y: Double) => x + y) wrappedAs "Add"
  }

  def formattedEquity[A](a: V[A]): Procedure[String] = {
    Map(a, (x: A) => x.toString) wrappedAs "formattedEquity"
  }

  def mapOption[A, B](a: V[Option[A]], f: A => B): Procedure[Option[B]] = {
    Map(a, (opta: Option[A]) => opta.map(f)) wrappedAs "Map Option"
  }

  def leverDoubleIfNotNegative(a: Double): Procedure[Double] = {
    a match {
      case 0.0 => Pure(0.0)
      case x => add(Pure(x), Pure(x))
    }
  } wrappedAs "leverDoubleIfNotNegative"

  def formatFunction(x: Option[String]): String = x match {
    case Some(name) => s"Context{$name} "
    case None => ""
  }

  val debts = Pure(8d)
  val cash = Pure(10d)
  val assets: NamedVariable[Double] = add(cash, debts) named "Total Assets"
  val adjusted: FMap[Double, Double] = FMap(assets, (d: Double) => leverDoubleIfNotNegative(d))
  val formatted: Procedure[String] = formattedEquity(adjusted)


  // TODO not sure how to overcome type erasure to create custom intepreters.
  // TODO not stack safe


//  def interpreter[A, B, C](x: V[A]): A = x match {
//    case Pure(in, _) => in
//    case Map(in, f, name) => ???
//      val inner = interpreter(in) // A
//      f(inner)
//    case x: FMap[A, T] =>
//      val inner: V[A] = interpreter(x.in)
//      interpreter(x.f(inner))
//    case x: Combine[]
//    case _ => ???
//  }

//  def interpretPure[T](x: Pure[T]): T = {
//    println(s"${formatFunction(x.function)} | Pure: ${x.in}")
//    x.in
//  }
//
//  def interpretMap[A, B](x: Map[A, B]): B = {
//    println(s"${formatFunction(x.function)} | Map: ${x.in}")
//    val inner = interpreter(x.in)
//    x.f(inner)
//  }

//  val context: List[String] = List()
//  val state: (List[String], Any) => List[String] = (xs, x) => x.toString +: xs
//  val (resultState, result) = formatted.run(context, state)


  println(formatted.run)
  val x = 2
}