package calc10free

import cats.free.Free
import cats.free.Free.liftF

case class RepositoryName(id: String)

sealed trait CalcA[A]
final case class Read[A](name: String) extends CalcA[A]
final case class Output[A](name: String, a: A) extends CalcA[Unit]

object Calc {
  type Calc[A] = Free[CalcA, A]

  def read[A](name: String): Calc[A] =
    liftF[CalcA, A](Read(name))

  def output[A](name: String, a: A): Calc[Unit] =
    liftF[CalcA, Unit](Output(name, a))
}

