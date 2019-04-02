package calcs

import scala.util.Try

/**
  * Created by nicholasbradford on 3/31/19.
  */
object UnversionedCalcCollision extends App {

  /**
    * If you're not persisting to an external data store with side effects, and you're instead accumulating,
    *   you need some way of telling whether or not calcs are equivalent (here, the root A).
    */

  trait A
  trait B
  trait C
  trait X
  trait Y
  trait Z

  /**
    * Let's say you assign an arbitrary "unversioned" ID incrementally, so the run of CalcB gets Unid(1).
    * If calcX and calcY keep track of this, then rebuilding the graph will correctly point to the same CalcB run.
    */
  def calcB: A => B = ???
  def calcX: B => X = ???
  def calcY: B => Y = ???
  def calcZ: (X, Y) => Z = ???

  /**
    * However, consider the case where two separate branches merge - how can you tell they're different?
    *   They both received Unid(1).
    */
  def calcB1: A => B = ???
  def calcB2: A => B = ???
  def calcC: (B, B) => C = ???

  /**
    * However, consider the case where two separate branches merge - how can you tell they're different?
    *   They both received Unid(1).
    *
    * We have a couple options:
    *   1) Give UnIDs a random hash and creation timestamp to prevent collisions
    *   2) check reference equality - more elegant, but potentially very confusing to debug.
    */

  /**
    * Ref equality
    *
    * ...you could argue that including an "ID" here is actually more confusing than not including one,
    *   seeing as it won't be used for anything.
    */
  class UnIDbyRef(calcName: String) {
    override def equals(obj: scala.Any): Boolean = obj match { // doesn't let you compare by internal id, only by ref
      case x: scala.AnyRef => this.eq(x)
      case _ => false
    }

//    override def hashCode(): Int = ??? // hashing might not really work well here
  }
  val unId1 = new UnIDbyRef("calcB")
  val unId2 = new UnIDbyRef("calcB")
  val unId3 = unId1
  println(unId1 eq unId3) // true
  println(unId1 == unId3) // true
  println(unId1 == unId2) // false


  /**
    * Random hash
    *
    * Here's an issue though: when generating the random hashes, we still need to initialize
    *   with a seed - if both threads start with the same seed, we get the same problem...
    * Either way, we're back to having some sort of global context containing
    *   either all the runs, or assigning random seeds for the hash generators.
    */
  case class UnIDbyRandom private(calcName: String, id: Long, randomId: Array[Byte])
  object UnIDbyRandom {
    type Rand[A] = cats.data.State[scala.util.Random, A]
    def apply(calcName: String, id: Long, rand: Rand[Array[Byte]]): UnIDbyRandom = ??? // use nextBytes 128
  }
}
