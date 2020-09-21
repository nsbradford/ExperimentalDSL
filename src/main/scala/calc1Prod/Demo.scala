package calcdsl



// async: futures short-circuit only if they're first in the for-comprehension
// https://alvinalexander.com/scala/how-exceptions-work-scala-futures-oncomplete-failure

object Interpreter extends App {

  //  Random.setSeed(1232)
  //
  //  def requestNewVersion: Version = Version(Random.nextLong())
  //
  //  def interpret[A, R](calc: Calc[R]): Versioned[R] = calc match {
  //    case v @ Versioned(_, _) =>
  //      v
  //    case v @ Map(inner: Calc[A], f: (A => R)) =>
  //      val x: Calc[A] = inner
  //      val result: Versioned[A] = interpret[Any, A](x)
  //      Versioned(f(result.get), requestNewVersion)
  //
  //  }
  //
  //
  //  val graph: Calc[String] =
  //    Versioned(1232, requestNewVersion)
  //      .map(_ * 2)
  //      .map(_.toString)
  //      .output(x => println(s"Persisted: $x"))
  //
  //  val result: Versioned[String] = interpret(graph)
  //
  //  println(result)

}
