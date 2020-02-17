package calc10free

import cats.{Id, ~>}
import scala.collection.mutable

object DemoBasicImpure extends App {


  def impureCompiler: CalcA ~> Id = new (CalcA ~> Id) {
    // a very simple (and imprecise) key-value store
    val kvs = mutable.Map.empty[String, Any]

    override def apply[A](fa: CalcA[A]): Id[A] = fa match {
      case Read(name) =>
        println(s"read $name")
        kvs(name).asInstanceOf[A]
      case Output(name, a) =>
        println(s"output $name: $a")
        kvs(name) = a
        ()
    }
  }

  val impureResult = Computations.program.foldMap(impureCompiler)

}
