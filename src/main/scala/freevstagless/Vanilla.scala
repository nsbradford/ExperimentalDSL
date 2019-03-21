package freevstagless

import java.util.UUID

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global



// basic use case

// just any basic case class will do here
case class User(id: UUID, email: String, loyaltyPoints: Int) {
  def serialize: String = id.toString + "," + loyaltyPoints + "," + email
}
object User {
  def parse(s: String): User = {
    val parts = s.split(",")
    User(UUID.fromString(parts(0)), parts(2), parts(1).toInt)
  }
}


// operations in our mini eDSL
trait UserRepository {
  def findUser(id: UUID): Future[Option[User]]
  def updateUser(u: User): Future[Unit]
}


// application code with side effects
object VanillaBusinessLogic {
  def addPoints(ur: UserRepository)(userId: UUID, pointsToAdd: Int): Future[Either[String, Unit]] = {
    ur.findUser(userId).flatMap {
      case None => Future.successful(Left("User not found"))
      case Some(user) =>
        val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
        ur.updateUser(updated).map(_ => Right(()))
    }
  }
}

// implementation for UserRepository eDSL
class UserRepositoryImpl extends UserRepository {
  override def findUser(id: UUID): Future[Option[User]] = {
    println("Vanilla UserRepository: /* go and talk to a database */")
    Future.successful(None)
  }
  override def updateUser(u: User) = {
    println("Vanilla UserRepository: /* go and talk to a database */")
    Future.successful(())
  }
}

object RunVanilla extends App {
  val userRepository = new UserRepositoryImpl
  val result: Future[Either[String, Unit]] = {
    println("Running program...")
    VanillaBusinessLogic.addPoints(userRepository)(UUID.randomUUID(), 10)
  }

  import scala.concurrent.duration._
  println(Await.result(result, 10.seconds))
}


// ====================================================================================================================
// combining languages

// a second eDSL
trait EmailService {
  def sendEmail(email: String, subject: String, body: String): Future[Unit]
}

// application code that has to mix both languages
class LoyaltyPointsCombined(ur: UserRepository, es: EmailService) {
  def addPoints(userId: UUID, pointsToAdd: Int): Future[Either[String, Unit]] = {
    ur.findUser(userId).flatMap {
      case None => Future.successful(Left("User not found"))
      case Some(user) =>
        val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
        for {
          _ <- ur.updateUser(updated)
          _ <- es.sendEmail(user.email, "Points added!",
            s"You now have ${updated.loyaltyPoints}")
        } yield Right(())
    }
  }
}


// compilation to lower-level instruction set

trait KVStore {
  def get(k: String): Future[Option[String]]
  def put(k: String, v: String): Future[Unit]
}

// takes the tutorial.UserRepository language and compiles it down to the lower-level KVStore language
class UserRepositoryUsingKVStore(kvStore: KVStore) extends UserRepository {
  override def findUser(id: UUID): Future[Option[User]] =
    kvStore.get(id.toString).map(serialized => serialized.map(User.parse))

  override def updateUser(u: User): Future[Unit] = {
    val serialized = u.serialize
    for {
      _ <- kvStore.put(u.id.toString, serialized)
      _ <- kvStore.put(u.email, serialized) // let's say we also maintain a by-email index
    } yield ()
  }
}
