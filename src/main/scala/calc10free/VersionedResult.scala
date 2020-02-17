package calc10free

import cats.Monad

import scala.annotation.tailrec


case class Version(id: Long) extends AnyVal

sealed abstract class VersionedResult[A](val get: A, val versions: Set[Version])

final case class IntermediateVersionedResult[A](override val get: A, inputs: Set[Version] = Set())
  extends VersionedResult(get, inputs)

final case class Versioned[A](override val get: A, version: Version)
  extends VersionedResult(get, Set(version))


object VersionedResult {

  implicit val versionedResultIsMonad: Monad[VersionedResult] = new Monad[VersionedResult] {
    override def flatMap[A, B](fa: VersionedResult[A])(f: (A) => VersionedResult[B]): VersionedResult[B] = {
      val inner: A = fa.get

      // TODO throws "java.lang.ClassCastException: ___ cannot be cast to scala.runtime.BoxedUnit"
      val result: VersionedResult[B] = f(inner)
      result
    }

    @tailrec
    override def tailRecM[A, B](a: A)(f: (A) => VersionedResult[Either[A, B]]): VersionedResult[B] = {
      val result: VersionedResult[Either[A, B]] = f(a)
      result match {
        case Versioned(Left(a1), _) => tailRecM(a1)(f)
        case IntermediateVersionedResult(Left(a1), _) => tailRecM(a1)(f)
        case Versioned(Right(b), v) => Versioned(b, v)
        case IntermediateVersionedResult(Right(b), v) => IntermediateVersionedResult(b, v)
      }
    }
    override def pure[A](x: A) = IntermediateVersionedResult(x)
  }

}
