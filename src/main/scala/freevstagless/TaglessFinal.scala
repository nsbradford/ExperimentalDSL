package freevstagless

import java.util.UUID

import cats.Monad
import cats.instances.future.catsStdInstancesForFuture
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}


// cats apparently has some fancy helpers for this in https://typelevel.org/cats-tagless/
//    among other things, it can apparently auto-convert to Free!
// also there's the `diesel` library for removing all the boilerplate


// operations in our mini eDSL
trait TaglessUserRepositoryAlg[F[_]] {
  def findUser(id: UUID): F[Option[User]]
  def updateUser(u: User): F[Unit]
}


// application code:
object TaglessBusinessLogic {
  def addPoints[F[_]: Monad](ur: TaglessUserRepositoryAlg[F])(userId: UUID, pointsToAdd: Int): F[Either[String, Unit]] = {
    ur.findUser(userId).flatMap {
      case None => implicitly[Monad[F]].pure(Left("User not found"))
      case Some(user) =>
        val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
        ur.updateUser(updated).map(_ => Right(()))
    }
  }
}

// all side effects are pushed to the interpreter
trait FutureInterpreter extends TaglessUserRepositoryAlg[Future] {
  override def findUser(id: UUID): Future[Option[User]] = {
    println("Tagless Interpreter: /* go and talk to a database */")
    Future.successful(None)
  }

  override def updateUser(u: User): Future[Unit] = {
    println("Tagless Interpreter: /* go and talk to a database */")
    Future.successful(())
  }
}


object RunTagless extends App {
  val interpreter = new FutureInterpreter {}
  val result: Future[Either[String, Unit]] = {
    println("Running program...")
    TaglessBusinessLogic.addPoints(interpreter)(UUID.randomUUID(), 10)
  }

  import scala.concurrent.duration._
  println(Await.result(result, 10.seconds))
}