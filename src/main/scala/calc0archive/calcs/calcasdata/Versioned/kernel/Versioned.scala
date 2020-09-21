package archive.calcs.calcasdata.Versioned.kernel

import scala.language.higherKinds


/**
  * Unsealed because we want Calculators to be able to Extend directly.
  *
  * TODO may require some rejiggering to support Trampolining/stack safety.
  */
trait V[T] extends Ancestral with NamedMetadata {
//  final def wrappedAs(s: String): Procedure[T] = Procedure(s, this)
//  final def named(s: String): NamedVariable[T] = NamedVariable(s, this)
  def run(implicit ex: Executor): ex.Result[T]
  def map[B](f: T => B): Map[T, B] = Map(this, f)
  def fmap[B](f: T => V[B]): FMap[T, B] = FMap(this, f)
//  def combine[B](f: T => B): Map[T, B] = Map(this, f)

}

case class Pure[T](raw: T) extends V[T]{
  override final def ancestors = Set()
  def run(implicit ex: Executor): ex.Result[T] = ex.run(this)
}

case class LazyV[T](raw: () => T) extends V[T]{
  override final def ancestors = Set()
  def run(implicit ex: Executor): ex.Result[T] = ex.run(this)
}

case class Map[A, T](ancestor: V[A],
                     f: A => T) extends V[T] with OneAncestor {
  def run(implicit ex: Executor): ex.Result[T] = ex.run(this)
}

case class FMap[A, T](ancestor: V[A],
                      f: A => V[T]) extends V[T] with OneAncestor {
  def run(implicit ex: Executor): ex.Result[T] = ex.run(this)
}

case class Combine[A, B, T](a: V[A],
                            b: V[B],
                            f: (A, B) => T) extends V[T] with TwoAncestors {
  def run(implicit ex: Executor): ex.Result[T] = ex.run(this)
}

/**
  * Both Procedures and NamedVariables are just V[T] + String name,
  *   the difference is that in diagnostics you know Procedures
  *   with the same name link to the same inner function,
  *   whereas Variables with the same name don't mean anything
  *   (are presumably in different namespaces - could enforce with macros).
  *
  * I.e. the rule is that all Functions should return Procedures,
  *   but inside the functions you can assign them names to make them appear
  *   named inside diagnostics.
  */
//case class Procedure[T](name: String, in: V[T]) extends V[T] {
//  override def run = in.run
//}
//case class NamedVariable[T](name: String, in: V[T]) extends V[T] {
//  override def run = in.run
//}

trait Procedure[T] extends V[T] {
  def name: String
  def run(implicit ex: Executor): ex.Result[T] = ex.run(this)

  override final def ancestors: Set[Ancestral] =
    if (isInnerHidden) {
      inputs.toSeq.toSet // TODO for some reason need a conversion to get syntax highlighting to work
    }
    else {
      result.ancestors
    }

  def inputs: Set[V[_]] // TODO HLists? Type erasure?
  def result: V[T] // TODO a little confusing that this is a V[T] and also produces something of the same type
  def isInnerHidden: Boolean
}

trait CProcedure[T] extends Procedure[T] with Product {
  override val isInnerHidden = false
  def inputs: Set[V[_]] = this.productIterator.collect{ case v: V[_] => v}.toSet
}
