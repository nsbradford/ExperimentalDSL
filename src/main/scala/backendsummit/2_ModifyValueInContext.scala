package backendsummit

import scala.language.higherKinds
import cats._
import cats.data._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try


/**
  * Oh yeah, map()!
  */
object OptionsWithPowerUp {

  /**
    * Now, whenever we have an Option[A] and want to turn it into an Option[B],
    *   we know that we can use `map()` and there's **no way to mess it up!**
    */
  def convertOptionIntToStr(myOption: Option[Int]): Option[(Int, Int)] = {
    return myOption.map(int => (int, int)) // this is a Scala-style lambda function
  }

  convertOptionIntToStr(Some(1))  // Some((1, 1))
  convertOptionIntToStr(None)     // None

  /** This can come in handy ... */
  val xOption: Option[Int] = Some(1)
  val yOption: String =
    xOption                 // Some(1)
      .map(x => x * 2)      // Some(2)
      .map(x => x + 77)     // Some(79)
      .map(x => x.toString) // Some("79")
      .getOrElse("default") // "79"

}


/**
  * What is the function signature for `map()`?
  *
  * What is a Generic definition of something you can `map` over like this?
  *   Lists, Dicts, Futures, Trys, IOs, IDs...
  */
trait MyFunctor[Container[_]] {

  /** Any Container (or more appropriately, "Context")
    *   you can write this function for is a Functor!
    */
  def map[A, B](fa: Container[A])(f: A => B): Container[B]
}


/**
  * Now, whenever we have an Functor[A] and want to turn it into an Option[B],
  *   we know that we can use `map()` and there's **no way to mess it up!**
  *
  * The code "writes itself" because Functor *is* the idea of transforming any F[A] => F[B].
  *
  * And, we can write some very high-level code!
  */
object CodeAbstractedAcrossFunctors extends App {
  def anyFunctorMakeItATuple[F[_] : Functor, A](fa: F[A]): F[(A, A)] =
    return fa.map(a => (a, a))  // literally the only way to write this function and have it compile

  println( anyFunctorMakeItATuple(Option(1)) )    // Option((1,1))
  println( anyFunctorMakeItATuple(None) )         // None
  println( anyFunctorMakeItATuple(List(1,2,3)) )  // List((1,1), (2,2), (3,3))
  println( anyFunctorMakeItATuple(Future(1)) )    // Future((1,1))
  println( anyFunctorMakeItATuple(Try(1)) )       // Try((1,1))
}


/**
  * DISCUSS: thoughts?
  *
  * DISCUSS: What can Functor/Map NOT express?
  */
object WhatIsLeftToExpress_Flat {

  // This user id might not exist, or DB connection issue, etc.
  def getUserName(userId: Int): Option[String] = ???
  def validateUsername(name: String): Option[String] = ???

  // Uh-oh! We want Option[String], not Option[Option[String]]!
  // How do we get rid of the nesting?
  val userId: Int = 1
  val validatedUsername: Option[Option[String]] = getUserName(userId).map(validateUsername)
}


/**
  * How about a more generic example?
  */
object WhatIsLeftToExpress_Choice {

  // How about this function?
  object Part1 {
    def choose(input: Option[Boolean], ifTrue: Option[String], ifFalse: Option[String]): Option[String] = ???
  }

  // What's a more general way to express "choosing"?
  object Part2 {
    def choose[B](input: Option[Boolean], decision: Boolean => Option[B]): Option[B] = ???
  }

  // Even more general!
  object Part3 {
    def choose[A, B](input: Option[A], decision: A => Option[B]): Option[B] = ???
  }

  // Maximum generality! We've encoding the idea of "choosing", or dependency chaining, into the types!
  object Part4 {
    def flatMap[Container[_], A, B](input: Container[A], f: A => Container[B]): Container[B] = ???
  }
}
