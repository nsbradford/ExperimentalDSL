package calcs.archive.garbage

/**
  * Created by nicholasbradford on 4/2/19.
  */
object ReflectUniqueCalcName extends App {

  import scala.reflect.runtime.universe._

  trait NamedCalc {
    def name: String
    final def fullyQualifiedName: String = s"$name: ${this.getClass.getName}"
  }

  class Calc2[-T1, -T2, +R](val name: String) extends Function2[T1, T2, R] with NamedCalc {
    override def apply(v1: T1, v2: T2): R = ???
  }

  def paramInfo[T](x: T)(implicit ev: TypeTag[T]): Unit = {
//    val targs = typeOf[T] match { case TypeRef(_, _, args) => args }
//    println(s"type of $x has type arguments $targs")
    println(ev.tpe)
  }

  paramInfo(new Calc2[Int, String, Double]("mycustomcalc"))
  println(new Calc2[Int, String, Double]("mycustomcalc").fullyQualifiedName)
}
