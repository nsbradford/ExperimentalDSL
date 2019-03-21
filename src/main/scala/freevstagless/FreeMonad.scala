package freevstagless

import java.util.UUID

import cats.free.Free
import cats.instances.future.catsStdInstancesForFuture
import cats.~>

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}


// Defining DSL requires some boilerplate (some libraries can automate)

sealed trait FreeUserRepositoryAlg[T]
case class FindUser(id: UUID) extends FreeUserRepositoryAlg[Option[User]]
case class UpdateUser(u: User) extends FreeUserRepositoryAlg[Unit]

object UserRepositoryDSL {
  type UserRepository[T] = Free[FreeUserRepositoryAlg, T]

  def findUser(id: UUID): UserRepository[Option[User]] = Free.liftF(FindUser(id))
  def updateUser(u: User): UserRepository[Unit] = Free.liftF(UpdateUser(u))
}


// application code: only constructs a future computation instead of actually performing side effects
object FreeBusinessLogic {

  def addPoints(userId: UUID, pointsToAdd: Int):
  UserRepositoryDSL.UserRepository[Either[String, Unit]] = {

    UserRepositoryDSL.findUser(userId).flatMap {
      case None => Free.pure(Left("User not found"))
      case Some(user) =>
        val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
        UserRepositoryDSL.updateUser(updated).map(_ => Right(()))
    }
  }
}


object FreeInterpreter {

  // all side effects are pushed to the interpreter
  val futureInterpreter = new (FreeUserRepositoryAlg ~> Future) {
    override def apply[A](fa: FreeUserRepositoryAlg[A]): Future[A] = fa match {
      case FindUser(id) =>
        println("Free Interpreter: /* go and talk to a database */")
        Future.successful(Some(User(UUID.randomUUID(), "email@x.com", 0)))
      case UpdateUser(u) =>
        println("Free Interpreter: /* go and talk to a database */")
        Future.successful(())
    }
  }
}


object RunFree extends App {

  val program = {
    println("Building program...")
    val x = FreeBusinessLogic.addPoints(UUID.randomUUID(), 10)
    println("Built program!")
    x
  }

  val result: Future[Either[String, Unit]] = {
    println("Running program...")
    program.foldMap(FreeInterpreter.futureInterpreter)
  }

  import scala.concurrent.duration._
  println(Await.result(result, 10.seconds))
}

// ====================================================================================================================
// combining languages
//
import cats.data.EitherK // tutorial refers to cats.data.Coproduct
import cats.InjectK // tutorial refers to Inject instead of InjectK

sealed trait FreeEmailAlg[T]
case class SendEmail(email: String, subject: String, body: String) extends FreeEmailAlg[Unit]

object X{
  type UserAndEmailAlg[T] = EitherK[FreeUserRepositoryAlg, FreeEmailAlg, T]

  class Users[F[_]](implicit i: InjectK[FreeUserRepositoryAlg, F]) {
    def findUser(id: UUID): Free[F, Option[User]] = Free.inject(FindUser(id))
    def updateUser(u: User): Free[F, Unit] = Free.inject(UpdateUser(u))
  }
  object Users {
    implicit def users[F[_]](implicit i: InjectK[FreeUserRepositoryAlg, F]): Users[F] =
      new Users
  }

  class Emails[F[_]](implicit i: InjectK[FreeEmailAlg, F]) {
    def sendEmail(email: String, subject: String, body: String): Free[F, Unit] =
      Free.inject(SendEmail(email, subject, body))
  }
  object Emails {
    implicit def emails[F[_]](implicit i: InjectK[FreeEmailAlg, F]): Emails[F] =
      new Emails
  }

}

object MultipleFreeBusinessLogic {
  import X._

  def addPoints(userId: UUID, pointsToAdd: Int)(
    implicit ur: Users[UserAndEmailAlg], es: Emails[UserAndEmailAlg]): Free[UserAndEmailAlg, Either[String, Unit]] = {

    ur.findUser(userId).flatMap {
      case None => Free.pure(Left("User not found"))
      case Some(user) =>
        val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)

        for {
          _ <- ur.updateUser(updated)
          _ <- es.sendEmail(user.email, "Points added!", s"You now have ${updated.loyaltyPoints}")
        } yield Right(())
    }
  }
}

object MultipleFreeInterpreter {

  val futureUserInterpreter = new (FreeUserRepositoryAlg ~> Future) {
    override def apply[A](fa: FreeUserRepositoryAlg[A]): Future[A] = fa match {
      case FindUser(id) =>
        println("Multiple Free Interpreter: FindUser /* go and talk to a database */")
        Future.successful(Some(User(UUID.randomUUID(), "email@x.com", 0)))
      case UpdateUser(u) =>
        println("Multiple Free Interpreter: UpdateUser /* go and talk to a database */")
        Future.successful(())
    }
  }

  val futureEmailInterpreter = new (FreeEmailAlg ~> Future) {
    override def apply[A](fa: FreeEmailAlg[A]): Future[A] = fa match {
      case SendEmail(email, subject, body) =>
        println("Multiple Free Interpreter: /* use smtp */")
        Future.successful(())
    }
  }

  val futureUserOrEmailInterpreter = futureUserInterpreter or futureEmailInterpreter

}

object MultipleFreeApp extends App {

  val program = {
    println("Building program...")
    val x = MultipleFreeBusinessLogic.addPoints(UUID.randomUUID(), 10)
    println("Built program!")
    x
  }


  val result: Future[Either[String, Unit]] = {
    println("Running program...")
    program.foldMap(MultipleFreeInterpreter.futureUserOrEmailInterpreter)
  }

  import scala.concurrent.duration._
  println(Await.result(result, 10.seconds))
}
