package calcs.calcasdata.Ver

import scala.annotation.tailrec
import scala.language.higherKinds

/**
  * Created by nicholasbradford on 5/2/19.
  */
sealed trait V[A]{
  def function: Option[String]
  def wrappedAs(s: String): V[A]
//  def interpret[X[_] <: V[_]](f: X[A] => A): A
}
case class Pure[A](in: A,
                   function: Option[String]=None) extends V[A]{
  override def wrappedAs(s: String): Pure[A] = this.copy(function=Some(s))
}

case class Map[B, A](in: V[A],
                     f: A => B,
                     function: Option[String]=None) extends V[B]{
  override def wrappedAs(s: String): Map[B, A] = this.copy(function=Some(s))
}

case class FMap[A, B](in: V[A],
                      f: A => V[B],
                      function: Option[String]=None) extends V[B]{
  override def wrappedAs(s: String): FMap[A, B] = this.copy(function=Some(s))
}

case class Combine[A, B, C](a: V[A],
                            b: V[B],
                            f: (A, B) => C,
                            function: Option[String]=None) extends V[C]{
  override def wrappedAs(s: String): Combine[A, B, C] = this.copy(function=Some(s))
}

object VDemo extends App {

  def add(a: V[Double], b: V[Double]): Combine[Double, Double, Double] = {
    Combine(a, b, (x: Double, y: Double) => x + y) wrappedAs "Add"
  }

  def formattedEquity[A](a: V[A]): Map[A, String] = {
    Map(a, (x: A) => x.toString) wrappedAs "formattedEquity"
  }

  def mapOption[A, B](a: V[Option[A]], f: A => B): Map[Option[A], Option[B]] = {
    Map(a, (opta: Option[A]) => opta.map(f)) wrappedAs "Map Option"
  }

  def leverDoubleIfNotNegative(a: Double): V[Double] = {
    a match {
      case 0.0 => Pure(0.0)
      case x => add(Pure(x), Pure(x))
    }
  } wrappedAs "leverDoubleIfNotNegative"

  val debts = Pure(8d)
  val cash = Pure(10d)
  val assets = add(cash, debts)
  val adjusted = FMap(assets, (d: Double) => leverDoubleIfNotNegative(d))
  val formatted: Map[Double, String] = formattedEquity(adjusted)

  def formatFunction(x: Option[String]): String = x match {
    case Some(name) => s"Context{$name} "
    case None => ""
  }


  // TODO not sure how to overcome type erasure to create custom intepreters.
  // TODO not stack safe
  def interpreter[A, B, C](x: V[A]): A = x match {
    case Pure(in, _) => in
    case Map(in, f, name) =>
      val inner = interpreter(in) // A
      f(inner)
//    case x: FMap[A, T] =>
//      val inner: V[A] = interpreter(x.in)
//      interpreter(x.f(inner))
//    case x: Combine[]
    case _ => ???
  }

  def interpretPure[T](x: Pure[T]): T = {
    println(s"${formatFunction(x.function)} | Pure: ${x.in}")
    x.in
  }

  def interpretMap[A, B](x: Map[A, B]): B = {
    println(s"${formatFunction(x.function)} | Map: ${x.in}")
    val inner = interpreter(x.in)
    x.f(inner)
  }
}