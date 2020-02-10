package calcdsl

import calcdsl.interpeters.SyncInterpreter
import cats.{Applicative, Functor, Monad}

import scala.concurrent.duration.FiniteDuration
import scala.language.higherKinds
import scala.util.Try


final case class Version(id: Long) extends AnyVal

/**
  * Created by nicholasbradford on 8/21/19.
  */
sealed trait Calc[A] {
  import Calc._
  def map[B](f: A => B): Calc[B] = FlatMapCalc[A, B](this, a => Calc.pure(f(a)))
//  def mergeWith[B](fb: Calc[B]): Calc[(A, B)] = MergeCalc(this, fb)
  def flatMap[B](f: A => Calc[B]): Calc[B] = FlatMapCalc(this, f)
  def output(repository: VersionedRepository[A]): Calc[A] = OutputCalc(this, repository)

  def runWith(in: CalcInterpreter): in.Result[A]
  def runSync(versionManager: VersionManager): VersionedResult[A] =
    this.runWith(new SyncInterpreter(versionManager))
}

object Calc{
  final case class PureCalc[A](f: () => A) extends Calc[A]{
    override def runWith(in: CalcInterpreter): in.Result[A] = in.execute(this)
  }
//  final case class MergeCalc[A, B](fa: Calc[A], fb: Calc[B]) extends Calc[(A, B)]
  final case class FlatMapCalc[A, B](in: Calc[A], f: A => Calc[B]) extends Calc[B] {
    override def runWith(in: CalcInterpreter): in.Result[B] = in.execute(this)
  }

  def apply[A](a: A): Calc[A] = Calc.pure(a)
  def pure[A](a: A): Calc[A] = PureCalc(() => a)

  implicit val calcFunctor: Functor[Calc] = new Functor[Calc] {
    override def map[A, B](fa: Calc[A])(f: (A) => B): Calc[B] = fa.map(f)
  }

  implicit val calcApplicative: Applicative[Calc] = new Applicative[Calc] {
    override def pure[A](x: A): Calc[A] = Calc.pure(x)
    override def ap[A, B](ff: Calc[A => B])(fa: Calc[A]): Calc[B] =
//      ff.mergeWith(fa).map{ case (f, a) => f(a) }
    ff.flatMap(fab => fa.map(fab))
  }
  // TODO derive monad instance on top of applicative
//  implicit val calcMonad: Monad[Calc] = new Monad[Calc]
}

sealed trait ReadMethod
case object Latest extends ReadMethod
case class LatestWithin(toLookBack: FiniteDuration) extends ReadMethod

//final case class ReadCalc[A](repository: VersionedRepository[A], method: ReadMethod)
final case class OutputCalc[A](in: Calc[A], repository: VersionedRepository[A]) extends Calc[A] {
  override def runWith(in: CalcInterpreter): in.Result[A] = in.execute(this)
}

sealed abstract class VersionedResult[A](val get: A, val versions: Set[Version])
final case class IntermediateVersionedResult[A](override val get: A, inputs: Set[Version])
  extends VersionedResult(get, inputs)
final case class Versioned[A](override val get: A, version: Version)
  extends VersionedResult(get, Set(version))
