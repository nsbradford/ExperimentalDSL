package calc10free



/**
  * Created by nicholasbradford on 2/16/20.
  */
object Computations {

  import calc10free.Calc._

  val program: Calc[Double] =
    for {
    //_ <- output[String]("log", "heyo i'm logging")
      _ <- output[Double]("held", -100d)
      _ <- output[Double]("desired", 15d)
      _ <- output[Double]("desired", 12d)
      _ <- output[Double]("held", 10d)
      held <- read[Double]("held")
      desired <- read[Double]("desired")
    } yield desired - held

  val singleString: Calc[List[String]] =
    for {
      _ <- output[List[String]]("held", List("asdf"))
    } yield List("asdfasdf")

}
