package archive.calcs.calcasfunction.archive.garbage

import cats.effect._
import cats.effect.concurrent.Ref
import cats.instances.list._
import cats.syntax.all._

/**
  * Created by nicholasbradford on 3/19/19.
  */
object RefAndIO {

}


import scala.concurrent.duration._


/**
  * *> is an alias for ProductR, which composes two functions while throwing away the result of the first.
  * We use this here because everything's just returning Unit.
  *
  * Checkout cats Apply.scala
  */
object sharedstate extends IOApp {

  def putStrLn(str: String): IO[Unit] = IO(println(str))

  def process1(myState: Ref[IO, List[String]]): IO[Unit] = {
    putStrLn("Starting process #1") *>
      IO.sleep(5.seconds) *>
      myState.update(_ ++ List("#1")) *>
      putStrLn("Done #1")
  }

  def process2(myState: Ref[IO, List[String]]): IO[Unit] = {
    putStrLn("Starting process #2") *>
      IO.sleep(3.seconds) *>
      myState.update(_ ++ List("#2")) *>
      putStrLn("Done #2")
  }

  def process3(myState: Ref[IO, List[String]]): IO[Unit] = {
    putStrLn("Starting process #3") *>
      IO.sleep(10.seconds) *>
      myState.update(_ ++ List("#3")) *>
      putStrLn("Done #3")
  }

//  def process4(myState: Ref[IO, List[String]]): IO[Int] = for {
//    n <- IO.pure(4)
//    _ <- myState
//  } yield n

  def masterProcess: IO[Unit] = ???
//    Ref.of[IO, List[String]](List.empty[String]).flatMap { myState =>
//      val ioa = List(process1(myState), process2(myState), process3(myState)).parSequence.void
//      ioa *> myState.get.flatMap(rs => putStrLn(rs.toString))
//    }

  override def run(args: List[String]): IO[ExitCode] =
    masterProcess.as(ExitCode.Success)

}