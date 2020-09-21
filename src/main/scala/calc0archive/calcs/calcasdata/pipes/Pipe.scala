package archive.calcs.calcasdata.pipes

/**
  * Starting with uninspired...
  */

sealed abstract class PipeLike {
  def name: String
}

sealed abstract class Pipe[T] extends PipeLike {
  def parent: PipeLike
  def run: T
  override def toString: String = parent.toString + "===>" + name
}

case object Plug extends PipeLike {
  def name = "<nothing>"
}

case class Source[T](name: String, in: () => T) extends Pipe[T] {
  override def parent = Plug
  override def run: T = {println(name); in()} // TODO(nick.bradford) has side-effect?
}
object PureSource {
  def apply[T](name: String, arg: => T): Source[T] = Source[T](name, () => arg)
}

case class Sink[T](name: String, parent: Pipe[T], writer: T => Unit) extends Pipe[T] {
  override def run: T = {
    println(name)
    val result = parent.run
    writer(result)
    result
  }
}

case class Map[I, O](name: String, parent: Pipe[I], f: I => O) extends Pipe[O]{
  override def run = {println(name); f(parent.run)}
}

case class FlatMap[I, O](name: String, parent: Pipe[I], f: I => Pipe[O]) extends Pipe[O] {
  override def run = {println(name); f(parent.run).run}
}

object Pipe {
  implicit class PipeSyntax[T](val pipe: Pipe[T]) extends AnyVal {
    def sink(persister: T => Unit): Sink[T] =  Sink(s"persisting", pipe, persister)
    def map[O](name: String)(f: T => O): Map[T, O] = Map(name, pipe, f)
    def flatMap[O](name: String)(f: T => Pipe[O]) = FlatMap("binding...", pipe, f)
  }
}


object PipeApp extends App {

  println("Building pipe, nothing should be executed!")

  def hydrateInt(): Int = {
    println("Hydrating Int...")
    1
  }

  val input1: Pipe[Int] = Source("dbConnect", hydrateInt)
  val input2: Pipe[Int] = PureSource("dbConnect", 2)
  val input3: Pipe[Int] = PureSource("dbConnect", 3)

  val simpleMapPipe: Pipe[Double] =
    input1
      .map("*4")(_ * 4)
      .map("/2")(_ / 2d) // change type
      .map("+100")(_ + 100d)
      .sink(println)

  println("Execute pipe...")
  val result: Double = simpleMapPipe.run
  println(s"Done executing pipe, finished with result: $result")
}