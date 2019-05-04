package calcs.calcasfunction.prototype2fullarity

import cats.effect.IO

/**
  * Created by nicholasbradford on 4/30/19.
  */
class Repositories {

}



object CommonRepositories {

  implicit val metadataRepository: MetadataRepository = new MetadataRepository {

    override def requisitionNewRunId(calcName: CalcName): IO[CalcRun] = IO {
      calcIdCounter = calcIdCounter + 1
      CalcRun(calcName, calcIdCounter)
    }
    private var calcIdCounter = 0

    override def logInput[T: Persistable](inputRecord: InputRecord[T]): IO[Unit] = IO {
      println(s"CalcRepository > log INPUT > $inputRecord of type: {${Persistable[T].dbTypeRepr}}")
    }

    override def logOutput[T: Persistable](outputRecord: OutputRecord[T]): IO[Unit] = IO {
      println(s"CalcRepository > log OUTPUT > $outputRecord of type: {${Persistable[T].dbTypeRepr}}")
    }

    override def logEquivalence[T: Persistable](hierarchyRecord: HierarchyRecord[T]): IO[Unit] = IO {
      println(s"CalcRepository > log EQUIVALENCE > $hierarchyRecord of type: {${Persistable[T].dbTypeRepr}}")
    }

  }

  implicit val IntHasRepository: Persistable[Int] = Persistable.create[Int](
    "Int-Std",
    x => IO(println(s"Persisted Int: $x"))
  )
  implicit val StringHasRepository: Persistable[String] = Persistable.create[String](
    "String-Std",
    x => IO(println(s"Persisted String: $x"))
  )


}
