package calc10free

import cats.{Monad, ~>}

import scala.collection.mutable


// TODO in this design there's duplication between the interpreter and the monad implementation, cleanup
object DemoDiagsImpure extends App {

  type Address = String
  type VersionStack = mutable.Buffer[Version]
  type Database = mutable.Map[Address, mutable.Buffer[Versioned[Any]]]
  type Inputs = mutable.Map[Version, Set[Version]]

  case class ImpureState(db: Database = mutable.Map.empty,
                         versionStack: VersionStack = mutable.Buffer(),
                         inputs: Inputs = mutable.Map.empty)

  def diagsImpureCompiler: CalcA ~> VersionedResult = new (CalcA ~> VersionedResult) {
    //    var currentVersion: Long = 0
    //    var state: ImpureState = ImpureState()

    val kvs = mutable.Map.empty[String, Any]

    override def apply[A](fa: CalcA[A]): VersionedResult[A] = fa match {
      case Read(name) =>
        println(s"reading $name...")
        //        val latest: Versioned[A] = state.db(name).last.asInstanceOf[Versioned[A]]
        //        state.versionStack += latest.version
        val latest = IntermediateVersionedResult(kvs(name).asInstanceOf[A])
        println(s"read $name: $latest")
        latest

      case Output(address, a: Any) => // TODO could probably just use Any instead of A here, and cast at very bottom
        println(s"output $address: $a ...")
        //        currentVersion += 1
        //        val newVersion = Version(currentVersion)
        //        val versioned = Versioned[Any](a, newVersion)
        //        state.inputs += ((newVersion, state.versionStack.toSet))
        //        state.versionStack.clear()
        //        val newBuffer = state.db.get(address) match {
        //          case None =>
        //            val versionedAsBuffer: mutable.Buffer[Versioned[Any]] = mutable.Buffer(versioned)
        //            state.db += ((address, versionedAsBuffer))
        //          case Some(existing) =>
        //            val newBuffer = existing += versioned.asInstanceOf
        //            state.db.update(address, newBuffer)
        //        }
        //        versioned.asInstanceOf[Versioned[A]]

        kvs(address) = a
        println(s"output $address finished.")
        Versioned(a.asInstanceOf[A], Version(1))
    }
  }

  val diagsImpureResult = Computations.singleString.foldMap(diagsImpureCompiler)
  println(s"Result: $diagsImpureResult")



}