package calcs.calcasdata.Versioned.kernel

import scala.language.higherKinds


/**
  * Analogue to Escher.executor.ExecutionContext
  *
  * TODO unsure about Result; mostly a way to enforce a context-specific
  *   concrete datatype (e.g. a Spark dataframe),
  *   but probably is overkill.
  */
trait Executor {

  type Result[T]

  def run[T](pure: Pure[T]): Result[T]
  def run[T](lazyV: LazyV[T]): Result[T]
  def run[A, T](map: Map[A, T]): Result[T]
  def run[A, T](fMap: FMap[A, T]): Result[T]
  def run[A, B, T](combine: Combine[A, B, T]): Result[T]
  def run[T](procedure: Procedure[T]): Result[T]

}
