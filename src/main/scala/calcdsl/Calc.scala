package calcdsl

import cats.Applicative

import scala.language.higherKinds
import scala.util.Try


final case class Version(id: Long)

/**
  * Created by nicholasbradford on 8/21/19.
  */
sealed trait Calc[A] {
  import Calc._
  def map[B](f: A => B): Calc[B] = FlatMapCalc(this, a => Calc.pure(f(a)))
  def mergeWith[B](fb: Calc[B]): Calc[(A, B)] = MergeCalc(this, fb)
  def fmap[B](f: A => Calc[B]): Calc[B] = FlatMapCalc(this, f)
  def output(repository: VersionedRepository[A]): Calc[A] = OutputCalc(this, repository)

  // TODO figure out how to fix this
//  def runWith(in: CalcInterpreter)
}

object Calc{
  final case class PureCalc[A](f: () => A) extends Calc[A]
  final case class MergeCalc[A, B](fa: Calc[A], fb: Calc[B]) extends Calc[(A, B)]
  final case class FlatMapCalc[A, B](in: Calc[A], f: A => Calc[B]) extends Calc[B]

  def apply[A](a: A): Calc[A] = Calc.pure(a)
  def pure[A](a: A): Calc[A] = PureCalc(() => a)

  implicit val calcApplicative: Applicative[Calc] = new Applicative[Calc] {
    override def pure[A](x: A): Calc[A] = Calc.pure(x)
    override def ap[A, B](ff: Calc[A => B])(fa: Calc[A]): Calc[B] = ff.mergeWith(fa).map{ case (f, a) => f(a) }
  }
  // TODO derive monad instance on top of applicative
}

final case class OutputCalc[A](in: Calc[A], repository: VersionedRepository[A]) extends Calc[A]
final case class Versioned[A](get: A, version: Version) extends Calc[A]

sealed abstract class VersionedResult[A](get: A, versions: Set[Version])
final case class IntermediateVersionedResult[A](get: A, inputs: Set[Version]) extends VersionedResult(get, inputs)
final case class FinalVersionedResult[A](data: Versioned[A]) extends VersionedResult(data.get, Set(data.version))

object CalcSyntax extends cats.syntax.ApplySyntax


trait CalcInterpreter{
  type Result[A] //<: cats.Monad[A]

  def execute[A](fa: Calc.PureCalc[A]): Result[A]
  def execute[A, B](fa: Calc.MergeCalc[A, B]): Result[(A, B)]
  def execute[A, B](fa: Calc.FlatMapCalc[A, B]): Result[B]
  def execute[A](fa: OutputCalc[A]): Result[A]
  def execute[A](fa: Versioned[A]): Result[A]
}

object CalcInterpreter {
  type Aux[R[_]] = CalcInterpreter{ type Result[A] = R[A] }
}

trait VersionedRepository[A] {
  def read(version: Version): Try[Versioned[A]]
  def write(fa: Versioned[A]): Try[Unit]
}




// async: futures short-circuit only if they're first in the for-comprehension
// https://alvinalexander.com/scala/how-exceptions-work-scala-futures-oncomplete-failure

object Interpreter extends App {

//  Random.setSeed(1232)
//
//  def requestNewVersion: Version = Version(Random.nextLong())
//
//  def interpret[A, R](calc: Calc[R]): Versioned[R] = calc match {
//    case v @ Versioned(_, _) =>
//      v
//    case v @ Map(inner: Calc[A], f: (A => R)) =>
//      val x: Calc[A] = inner
//      val result: Versioned[A] = interpret[Any, A](x)
//      Versioned(f(result.get), requestNewVersion)
//
//  }
//
//
//  val graph: Calc[String] =
//    Versioned(1232, requestNewVersion)
//      .map(_ * 2)
//      .map(_.toString)
//      .output(x => println(s"Persisted: $x"))
//
//  val result: Versioned[String] = interpret(graph)
//
//  println(result)

}