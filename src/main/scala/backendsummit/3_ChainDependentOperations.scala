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
  def pure[A](a: A): Container[A] // this just says "I can lift any value into my Monadic context"

  // flatMap() is exactly equivalent to map() + flatten()!
  override def map[A, B](fa: Container[A])(f: A => B): Container[B] = flatMap(fa)(a => pure(f(a)))
  final def flatten[A](ffa: Container[Container[A]]): Container[A] = flatMap(ffa)(fa => fa)
}


/**
  * What does this look like in practice?
  *   Well, turns out most programs are chains of dependent operations within a context!
  */
object Monad_Demos {

  object DB {
    def get_username(): Option[String] = ???
    def query_db_for_id(username: String): Option[Int] = ???
    def query_db_for_friends(id: Int): Option[List[String]] = ???
  }
  import DB._

  // what we want to do (verbose):
  val chain_callbacks_verbose: Option[List[String]] =
    get_username().flatMap{ username =>
      query_db_for_id(username).flatMap{ id =>
        query_db_for_friends(id)
      }
    }

  // also equivalent to:
  val chain_queries: Option[List[String]] =
    get_username()
      .flatMap(query_db_for_id)
      .flatMap(query_db_for_friends)

  // a "for-comprehension" is a generalization of python's list comprehension, which works for any monad!
  val chain_queries_readable: Option[List[String]] =
    for {
      username <- get_username()
      id <- query_db_for_id(username)
      friends <- query_db_for_friends(id)
    } yield friends

  // these "for-comprehensions" are ubiquitous in FP code;
  //  they are a generic way to write imperative-looking code within a "computational context"
}

/**
  * What if we wanted our application code to be separated from its context,
  *   to "abstract over different contexts/Monads"?
  *
  * E.g. what if we wanted one version of the backend to run Async, the other Sync
  */
object Monad_TaglessFinal extends App {

  abstract class DB[F[_] : Monad] {
    def get_username(): F[String]
    def query_db_for_id(username: String): F[Int]
    def query_db_for_friends(id: Int): F[List[String]]
  }

  // `Option` is sync, and we can use to represent operations which may fail
  class SyncTestDB extends DB[Option]{
    override def get_username(): Option[String] = Some("Nick")
    override def query_db_for_id(username: String): Option[Int] = Some(1)
    override def query_db_for_friends(id: Int): Option[List[String]] = Some(List("Friend1", "Friend2"))
  }

  // `Future` represents an "async context"; the value may or may not appear in the future
  class AsyncProductionDB extends DB[Future] {
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

  val sync_result: Option[List[String]] =  chain_contextless_sequence(new SyncTestDB())
  val async_result: Future[List[String]] = chain_contextless_sequence(new AsyncProductionDB())

  println(sync_result)
  println(async_result)
}


/**
  * DISCUSS: What do we think?
  */