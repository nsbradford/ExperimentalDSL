package archive.scratch

import scala.concurrent.Future
import scala.language.higherKinds

/**
  * Created by nicholasbradford on 8/21/19.
  */
object myscratch extends App {


  import cats.instances.option._

  import cats.Semigroupal

  val x = Semigroupal[Option]

//  import cats.instances.validated


  import cats._
  import cats.data._
  import cats.implicits._

//  def stringifyAnyInternal[F[_] : Functor](xs: F[Double]): F[String] = xs.map(_.toString)
//  println(stringifyAnyInternal(List(1d, 2d, 3d)))
//
//  List(1,2,3).foldMap()
}
