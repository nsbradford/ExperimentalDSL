package applicatives

import cats.Applicative
import cats.data.Validated
import cats.data.ValidatedNec
import cats.implicits._
//import cats.data.Validated._
//import cats.instances._


object Ap {

  def compute(i: Int, j: Int): Int = i - j

  def heldOpt: Option[Int] = Some(1)
  def desiredOpt: Option[Int] = Some(3)

  // with monad/flatmap
  val tradeToMonad: Option[Int] = for {
    held <- heldOpt
    desired <- desiredOpt
  } yield desired - held

  // option applicative
  val resultOpt: Option[Int] = Applicative[Option].map2(heldOpt, desiredOpt)(compute)
  val resultOpt2: Option[Int] = (heldOpt, desiredOpt).mapN(compute)

  // with app. uses cats.Chain under the hood for accumulation (constant push/pop)

  // doesn't work for some reason - can't find applicative instance properly
//  def heldValid: ValidatedNec[String, Int] = Validated.valid(1)
//  def desiredValid: ValidatedNec[String, Int] = Validated.valid(3)
//  val result = Applicative[ValidatedNec].map2(heldValid, desiredValid)(compute)

  type MyValid[A] = ValidatedNec[String, A]
  def heldValid: MyValid[Int] = Validated.valid(1)
  def desiredValid: MyValid[Int] = Validated.valid(3)
  val result = Applicative[MyValid].map2(heldValid, desiredValid)(compute)
  //    val tradeToAp: Future[Int] = (heldValid, desiredValid).mapN(compute)

  // turns out we can do applicatives in parallel with parMapN?
  // https://typelevel.org/cats/typeclasses/parallel.html
//  val resultOpt3: Option[Int] = (heldOpt, desiredOpt).parMapN(compute)

}

object Ap2 extends App {
  import cats.Applicative
  import cats.data.ValidatedNel
  import cats.syntax.validated._

  type Error = String
  // use a NonEmptyList on the left side
  type ErrorOr[A] = ValidatedNel[Error, A]

  // using invalidNel is a shortcut for
  // NonEmptyList.of("empty first name").invalid[String]
  val errorOrFirstName = "empty first name".invalidNel[String]
  val errorOrLastName = "empty last name".invalidNel[String]
  val errorsOrFullName: ErrorOr[String] =
    Applicative[ErrorOr].map2(errorOrFirstName, errorOrLastName)(_ + " " + _)
//  val errorattempt = (errorOrFirstName, errorOrLastName).mapN(_ + " " + _)

  println(errorsOrFullName)

  val x = (List(1, 2, 3), List(4, 5, 6)).parMapN(_ + _)
}