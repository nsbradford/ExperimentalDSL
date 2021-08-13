package backendsummit

import cats._
import cats.data._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.Try


/**
  * Monad is the idea of "choice", path-dependency, or
  *   **chaining dependant operations within a context**
  *   (or, equivalently, the ability to squash nested contexts).
  */
trait MyMonad[Container[A]] extends MyFunctor[Container] {

  // If you can flatMap(), you have yourself a Monad!
  def flatMap[A, B](fa: Container[A])(f: A => Container[B]): Container[B]
  def pure[A](a: A): Container[A] // this just says "I can lift any value `A` into my Monadic context"

  // flatMap() is exactly equivalent to map() + flatten()!
  override def map[A, B](fa: Container[A])(f: A => B): Container[B] = flatMap(fa)(a => pure(f(a)))
  final def flatten[A](ffa: Container[Container[A]]): Container[A] = flatMap(ffa)(fa => fa)
}


/**
  * What if we wanted our application code to be separated from its context?
  *
  * I.e. what if we wanted one version of the backend to run Async, the other Sync
  */
object Monad_TaglessFinal extends App {

  abstract class DB[F[_] : Monad] {
    def get_username(): F[String]
    def query_db_for_id(username: String): F[Int]
    def query_db_for_friends(id: Int): F[List[String]]
  }

  // `Option` is sync, and we can use to represent operations which may fail
  class TestDB extends DB[Option]{
    override def get_username(): Option[String] = Some("Nick")
    override def query_db_for_id(username: String): Option[Int] = Some(1)
    override def query_db_for_friends(id: Int): Option[List[String]] = Some(List("Friend1", "Friend2"))
  }

  // `Future` represents an "async context"; the value may or may not appear in the future
  class ProductionDB extends DB[Future] {
    override def get_username(): Future[String] = Future("Async Nick")
    override def query_db_for_id(username: String): Future[Int] = Future(1)
    override def query_db_for_friends(id: Int): Future[List[String]] = Future(List("Async Friend1", "Async Friend2"))
  }

  // Here's some business logic, written independently of sync/async concerns!
  //   And, it's REALLY hard to mess up!
  def chain_contextless_sequence[F[_] : Monad](db: DB[F]): F[List[String]] = {
    for {
      username <- db.get_username()
      id <- db.query_db_for_id(username)
      friends <- db.query_db_for_friends(id)
    } yield friends
  }

  val prod_result: Future[List[String]] =    chain_contextless_sequence(new ProductionDB())
  val test_result: Option[List[String]] =    chain_contextless_sequence(new TestDB())

  println(prod_result)
  println(test_result)
}
