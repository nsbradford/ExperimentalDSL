package shapelessfeatures


object FunctionArityAbstraction {

  import shapeless.ops.function._
  import shapeless.syntax.std.function._
  import shapeless.{Generic, HList}


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

}