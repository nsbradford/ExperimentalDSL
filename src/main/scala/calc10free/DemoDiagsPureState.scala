package calc10free


/**
  * Created by nicholasbradford on 2/16/20.
  */
object DemoDiagsPureState {


  // TODO getting errors: ClassCastException: java.lang.String cannot be cast to scala.runtime.BoxedUnit

  //  import cats.data.State
  //  type InnerState = Map[String, Seq[Any]]
  //  type CalcState[A] = State[InnerState, A]
  //
  //  val pureCompiler: CalcA ~> CalcState = new (CalcA ~> CalcState) {
  //    def apply[A](fa: CalcA[A]): CalcState[A] =
  //      fa match {
  //        case Read(name) =>
  //          println("Read")
  //          State.inspect(s => s(name).asInstanceOf[A])
  //        case Output(name, a: A) =>
  //          println("Output")
  //          val x: CalcState[A] =
  //            for {
  //              already: Seq[A] <- State.inspect{ s: InnerState =>
  //                s.get(name).asInstanceOf[Option[Seq[A]]].getOrElse(Seq())
  //              }
  //              _ <- State.modify{ s: InnerState => s.updated(name, already :+ a) }
  //            } yield a
  //
  //          x
  //      }
  //  }
  //
  //  val pureResult: CalcState[_] = program.foldMap(pureCompiler)
  //  val x = pureResult.run(Map.empty).value
  //  val y = None

}
