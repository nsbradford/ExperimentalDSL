package calcs.calcasfunction.smartdomain

import scala.util.{Success, Try}


/**
  * TODO transition APIs to Future, then Try, and finally abstract over F[_] (tagless final)
  */
trait Binder[T] {

  def get: T

  /**
    * Pushes a new CalcRun context onto T
    *   so that T knows in what context it's in.
    */
  def bind(calcRun: CalcRun): Binder[T]

  /**
    * Returns a new T with the context unbinded.
    */
  //  def unbind: Binder[T]

}


// TODO use macro or classpath to eliminate collisions
case class StorageTypeRepresentation(name: String)
//  final def fullName: String = s"$dataConceptName: ${this.getClass.getName}"

trait Repo[T] extends Binder[T] {
  def name: StorageTypeRepresentation
}

trait Bundle[T <: Binder[T]] extends Binder[T] {
  this: T =>
  override final def get: T = this
  def get(repo: Repo[T]): T
}
