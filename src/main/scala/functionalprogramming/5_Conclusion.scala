package functionalprogramming

import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


/**
  * THESIS:
  *   Monads and other FP concepts are useful (and ubiquitous) abstractions to recognize.
  *     I think of it as learning Chord Progressions for music, Archetypes/Hero's Journey for storytelling, etc.
  *   They're useful tools for thought - and if your language supports it, also can clarify your code.
  */

/**
  * Teasers for other topics
  *   - Modeling with Algebraic Data Types (Sums and Products)
  *   - Tagless Final DSL
  *   - Free Monads for embedded DSLs
  *   - Automatic type class derivation
  *   - Other cool type classes
  *     - Monoid for anything you can "sum" or "combine"
  *     - Foldable for folding
  *     - Traverse for inverting contexts (using Applicative + Foldable)
  */

/**
  * Monads are just the beginning of FP abstractions! Just to tease a couple more...
  */
object Teaser {
  import cats._
  import cats.implicits._

  // want to "sum" things together? "Monoid" is here for you!
  def combineAll[A: Monoid](as: List[A]): A = as.foldLeft(Monoid[A].empty)(Monoid[A].combine)
  val ints: Int = combineAll(List(1,1,1)) // result: 3
  val strs: String = combineAll(List("Hello ", "World ", "with ", "monoids!")) // result: "Hello World with monoids!"
  // TODO automatically derive with shapeless? https://www.scala-exercises.org/shapeless/auto_typeclass_derivation

  // want to invert contexts? Hello "Traverse" type class!
  val listOfFutures: List[Future[Int]] = List(Future(1), Future(2), Future(3))
  val futureList: Future[List[Int]] = listOfFutures.sequence
  // result: Future(List(1,2,3))
}


/**
  * What if we wanted our application code to be separated from its context?
  */
object Monad_TaglessFinal {
  import cats._
  import cats.implicits._

  class DB[F[_] : Monad] {
    def get_username(): F[String] = ???
    def query_db_for_id(username: String): F[Int] = ???
    def query_db_for_friends(id: Int): F[List[String]] = ???
  }

  class ProductionDB extends DB[Future] {
    override def get_username(): Future[String] = ???
    override def query_db_for_id(username: String): Future[Int] = ???
    override def query_db_for_friends(id: Int): Future[List[String]] = ???
  }

  class TestDB extends DB[Try]{
    override def get_username(): Try[String] = ???
    override def query_db_for_id(username: String): Try[Int] = ???
    override def query_db_for_friends(id: Int): Try[List[String]] = ???
  }

  def chain_contextless_sequence[F[_] : Monad](db: DB[F]): F[List[String]] = {
    for {
      username <- db.get_username()
      id <- db.query_db_for_id(username)
      friends <- db.query_db_for_friends(id)
    } yield friends
  }

  def prod_result: Future[List[String]] = chain_contextless_sequence(new ProductionDB())
  def test_result: Try[List[String]] =    chain_contextless_sequence(new TestDB())

}
