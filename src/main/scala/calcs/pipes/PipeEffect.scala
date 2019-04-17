package calcs.pipes

/**
  * Starting with uninspired...
  */

//sealed abstract class PipeEffectLike[F[_]] {
//  def name: String
//}
//
//sealed abstract class PipeEffect[F[_], T] extends PipeEffectLike[F] {
//  def parent: PipeLike
//  def run: F[T]
//  override def toString: String = parent.toString + "===>" + name
//}
//
//case object PurePipe extends PipeEffectLike[_] {
//  def name = "<nothing>"
//}
//
//case class SourceEffect[F[_], T](name: String, in: () => T) extends PipeEffect[F, T] {
//  override def parent = Plug
//  override def run: F[T] = {println(name); in()}
//}
//object PureSource {
//  def apply[F[_], T](name: String, arg: => T): SourceEffect[F, T] =
//    SourceEffect(name, () => arg)
//}
//
//case class SinkEffect[F[_], T](name: String, parent: Pipe[T], writer: T => Unit) extends PipeEffect[F, T] {
//  override def run: F[T] = {
//    println(name)
//    val result = parent.run
//    writer(result)
//    result
//  }
//}
//
//case class MapEffect[F[_], I, O](name: String, parent: Pipe[I], f: I => O) extends PipeEffect[F, O]{
//  override def run = {println(name); f(parent.run)}
//}
//
//case class FlatMapEffect[F[_], I, O](name: String, parent: Pipe[I], f: I => Pipe[O]) extends PipeEffect[F, O] {
//  override def run = {println(name); f(parent.run).run}
//}
//
//object Pipe {
//  implicit class PipeSyntax[F[_], T](val pipe: PipeEffect[F, T]) extends AnyVal {
//    def pure(t: T): SourceEffect[F, T] = PureSource("sourcing", t)
//    def sink(persister: T => Unit): Sink[T] =  SinkEffect(s"persisting", pipe, persister)
//    def map[O](name: String)(f: T => O): Map[T, O] = MapEffect("mapping", pipe, f)
//    def flatMap[O](name: String)(f: T => Pipe[O]) = FlatMapEffect("binding...", pipe, f)
//  }
//}
