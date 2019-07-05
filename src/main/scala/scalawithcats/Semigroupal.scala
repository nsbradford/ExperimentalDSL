package scalawithcats

import cats.Semigroupal
import cats.instances.option._ // for Semigroupal Semigroupal.tuple3(Option(1), Option(2), Option(3))


/**
  * Created by nicholasbradford on 5/27/19.
  */
object DemoSemigroupal extends App {


  Semigroupal.tuple3(Option(1), Option(2), Option(3))
  // res3: Option[(Int, Int, Int)] = Some((1,2,3))

  Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
  // res5: Option[Int] = Some(6)

}
