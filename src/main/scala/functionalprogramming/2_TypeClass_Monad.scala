package functionalprogramming

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.Try


/**
  * Core type classes: Functors, Applicatives, and Monads
  *
  * Now that we've seen a bit of what you can do with types, let's view some of the
  *   foundational concepts, expressed as type classes, which empower you in FP!
  *   (Yes, that'll include Monads - with no category theory, just practical examples!)
  */

/**
  * First meet some our favorite "Computational Contexts", which we'll call F[_] (a box with something inside)
  *
  * Option[T]: similar to Python's Optional; T may exist as `Some[T]`, or not exist and be `None`
  * List[T]: zero or more countable ordered Ts
  * Try[T]: may contain T, or an Exception
  * Future[T]: T may become available at some point (used for e.g. async calls)
  * Id[T]: a trivial container which is actually just a T
  *
  */

/**
  * DISCUSS: How do I perform the operation: List[A] => List[B]?
  *   Hint: it's builtin to Python...
  */
object Functor_Motivation {
  def foo(x: Int): String = (x * 2).toString

  // we use our classic `map` operation!
  val list: List[String] = List(1, 2, 3).map(foo)

  // turns out you can do this for a lot of containers/computational contexts
  val future: Future[String] = Future(1).map(foo)

  // is there any limit to the number of times you chain operations?
  def bar(x: Int): Int = x * 2
  val chained: List[Int] = List(1, 2, 3).map(bar).map(bar).map(bar).map(bar).map(bar) // no limits here!

}

// wait, can we formalize this concept into a type class?
trait MyFunctor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  // let's compare to a library definition...
  import cats.Functor
}

/**
  * Can we use this to write even more generic code?
  */
object Functor_UsingTheTypeClass extends App {

  import cats._
  import cats.implicits._

  def multiplyAnyFunctorBy2[F[_]: Functor](fa: F[Int]): F[String] = fa.map(Functor_Motivation.foo)

  println( multiplyAnyFunctorBy2(List(100, 100, 100)) )
  println( multiplyAnyFunctorBy2[Option](Some(100)) )
  println( multiplyAnyFunctorBy2(Try(100)) )
  println( multiplyAnyFunctorBy2(Future(100)) )
}

/**
  * DISCUSS: What can Functor NOT express?
  */
object Applicatives_Motivation {
  // Have you ever wanted to "merge" two contexts?
  // Try adding these two together using `map()`:
  val f1: Future[Int] = Future(1)
  val f2: Future[Int] = Future(2)
  val f3: Future[Future[Int]] = {
    f1.map(x1 =>
      f2.map(x2 => x1 + x2)
    )
    // Wait, Future[Future[Int]]? that's not what we want...
  }
}

// What we need is a new operation!
trait MyApplicative[F[_]] extends MyFunctor[F] { // (simplified)
  def pure[A](a: A): F[A] // Lift an ordinary value into our context. List() constructor for list
  def tupled[A, B](fa: F[A], fb: F[B]): F[(A, B)] // often called "product"
}

object Applicatives_Usage {
  import cats.implicits._

  val merged: Future[(Int, Int)] = (Future(1), Future(2)).tupled
  val result: Future[Int] = merged.map{ case (x1, x2) => x1 + x2 }

  // or do it like this...
  val resultFancy: Future[Int] = (Future(1), Future(2)).mapN(_ + _) // neat!
}

/**
  * DISCUSS: What can Applicative NOT express?
  */
object Monad_Motivation {

  // How about this function?
  object Part1 {
    def choose(input: Future[Boolean])(ifTrue: Future[String], ifFalse: Future[String]): Future[String] = ???
  }

  // What's a more general way to express this?
  object Part2 {
    def choose[B](input: Future[Boolean])(decision: Boolean => Future[B]): Future[B] = ???
  }

  // Even more general!
  object Part3 {
    def choose[A, B](input: Future[A])(decision: A => Future[B]): Future[B] = ???
  }

  // Maximum generality!
  object Part4 {
    def flatMap[F[_], A, B](input: F[A])(f: A => F[B]): F[B] = ???
  }
}

trait MyMonad[F[_]] extends MyApplicative[F] { // (simplified)
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // flatMap() is exactly equivalent to map() + flatten()!
  final def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  // Strictly more expressive power than map() or product()

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

  override def tupled[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    flatMap(fa){ a =>
      flatMap(fb){ b => pure(a, b) }
    }
  }

}


/**
  * What does this look like in practice?
  *   Well, turns out most programs are chains of dependent operations within a context!
  */
object Monad_Demos {
  def get_username(): Future[String] = ???
  def query_db_for_id(username: String): Future[Int] = ???
  def query_db_for_friends(id: Int): Future[List[String]] = ???

  // what we want to do (verbose):
  val chain_callbacks_verbose: Future[List[String]] =
    get_username().flatMap{ username =>
      query_db_for_id(username).flatMap{ id =>
        query_db_for_friends(id)
      }
    }

  // also equivalent to:
  val chain_queries: Future[List[String]] =
    get_username()
      .flatMap(query_db_for_id)
      .flatMap(query_db_for_friends)

  // a "for-comprehension" is a generalization of python's list comprehension,
  //   which works for any monad!
  val chain_queries_readable: Future[List[String]] =
    for {
      username <- get_username()
      id <- query_db_for_id(username)
      friends <- query_db_for_friends(id)
    } yield friends

  // these "for-comprehensions" are ubiquitous in FP code;
  //  they are a generic way to write imperative-looking code within a "computational context"
}


/**
  * RECAP
  *   - Functors: for static, linear operations
  *   - Applicatives for static, parallel operations
  *   - Monads for dynamic, dependent computations
  *
  * DISCUSS:
  *   How do you feel about Monads?
  *     Feel like you understand them?
  *     Wondering why everyone makes such a big deal about them?
  *     Wondering why everyone explains it by saying "a monad is just a monoid in the category of endofunctors"?
  *   Why/why not is it useful to have these three levels of separation (Functor, Applicative, Monad)?
  *
  */
