package calcs.arityabstracted

import shapeless.{Generic, HList, ::, HNil}
import shapeless.syntax.std.function._
import shapeless.ops.function._

/**
  * Requires:
  *   Input types are Tuples or HLists
  *   Convert functionN => f.tupled() // and then convert to HList?
  *     Aux pattern for f type => return type
  *   Implicit ev input HList is InputLoggable[T]
  */
object CalcArityAbstracted extends App {


  /**
    * Simple demo showing you can abstract over function arity.
    */

  def applyProduct[InputTuple <: Product, Function, InputRepr <: HList, Result]
    (p: InputTuple)
    (f: Function)
    (implicit gen: Generic.Aux[InputTuple, InputRepr],
    fp: FnToProduct.Aux[Function, InputRepr => Result])
  : Result = {
    f.toProduct(gen.to(p))
  }

  // Note: you get a very egregious compile error if the FnToProduct isn't found
  // when the reason is just that your input and function aren't same arity
  val f: (Int, Int, Int) => Int = (a: Int, b: Int, c: Int) => a + b + c
  val x = applyProduct((1, 2, 3))(f)
  val y = applyProduct(1, 2, 3)(f) // can unpack the tuple too!

  println(x)



  /**
    * The real challenge - prove that all elements of the product, and of the result, are serializable
    */

  trait Repository[T]{
    def persist(t: T): Unit
  }

  def createRepository[T](f: T => Unit = (_: T) => ()) = new Repository[T] {
    override def persist(t: T): Unit = f(t)
  }

  implicit val IntHasRepository = createRepository[Int](x => println(s"Persisted Int: $x"))

  implicit val hNilEncoder = createRepository[HNil]()

  implicit def hListRepository[H, T <: HList]
    (implicit
     headRepository: Repository[H],
     tailRepository: Repository[T])
  : Repository[H :: T] = {
    createRepository{
      case h :: t =>
        headRepository.persist(h)
        tailRepository.persist(t)
    }
  }

  def applyWithEvd[InputTuple <: Product, Function, InputRepr <: HList, Result]
    (p: InputTuple)
    (f: Function)
    (implicit
     gen: Generic.Aux[InputTuple, InputRepr],
     fp: FnToProduct.Aux[Function, InputRepr => Result],
     repoInput: Repository[InputRepr],
     repoResult: Repository[Result])
  : Result = {
    val answer = f.toProduct(gen.to(p))
    repoInput.persist(gen.to(p))
    repoResult.persist(answer)
    answer
  }

  val z = applyWithEvd(1, 2, 3)(f)

  println(y)
}

