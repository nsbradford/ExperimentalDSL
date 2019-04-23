package calcs.arityabstracted

import calcs.SharedModel.{CalcName, CalcVersionAssigned}
import calcs.arityabstracted.GetGenericFToWork.Versioned
import shapeless.ops.function.FnToProduct
import shapeless.{::, Generic, HList, HNil, the}
import calcs.SharedModel.{CalcName, CalcVersionAssigned, VersionedData, VersionedDataUnpersisted}
import shapeless.LUBConstraint.<<:
import shapeless.{::, Generic, HList, HNil, LUBConstraint, the}
import shapeless.syntax.std.function._
import shapeless.ops.function._
import shapeless.ops.hlist.IsHCons

/**
  * Created by nicholasbradford on 4/22/19.
  */
class CalcArityAbstracted {

  trait VProduct[T]{
    type HListRepr <: HList
    def to(t: T): HListRepr
  }

  object VProduct{
    type Aux[T, Repr] = VProduct[T]{ type HListRepr = Repr }

    //    def apply[T <: HList, Repr <: HList](implicit vProduct: VProduct.Aux[T, Repr]): VProduct[T] = vProduct
    def apply[T <: HList](implicit vProduct: VProduct[T]): VProduct.Aux[T, vProduct.HListRepr] =
      vProduct // ignore red

    def create[T <: HList, Repr <: HList](f: T => Repr): VProduct.Aux[T, Repr] = new VProduct[T]{
      override type HListRepr = Repr
      override def to(t: T): Repr = f(t)
    }
  }

  implicit val hnilIsVProduct: VProduct.Aux[HNil, HNil] = VProduct.create[HNil, HNil](_ => HNil)

  implicit def groupIsVProduct[H, T <: HList]
  (implicit tailProduct: VProduct[T])
  : VProduct.Aux[Versioned[H] :: T, H :: tailProduct.HListRepr] =
  {
    VProduct.create[Versioned[H] :: T, H :: tailProduct.HListRepr] {
      case h :: t => h.data :: tailProduct.to(t)
    }
  }

  // DEMO
  val vdata: Versioned[String] = Versioned("howdy", CalcVersionAssigned(CalcName("dummyCalc"), 34324))

  val x_ = implicitly[VProduct[HNil]]
  val x = VProduct[Versioned[String] :: HNil] // VProduct.Aux[Versioned[String] :: HNil, String :: HNil]
  val xx = VProduct[Versioned[String] :: Versioned[String] :: HNil] // VProduct.Aux[Versioned[String] :: HNil, String :: HNil]

  val asdf = the[VProduct[Versioned[String] :: HNil]]

  def foo[T, Repr](vdata: Versioned[T] :: Versioned[T] :: HNil)
                  (implicit vprod: VProduct.Aux[Versioned[T] :: Versioned[T] :: HNil, Repr])
  : String = {
    vprod.to(vdata).toString
  }
  println(foo(vdata :: vdata :: HNil))


  def bar[InputTuple <: Product, InputRepr <: HList, UnboxedData <: HList]
  (p: InputTuple)
  (implicit
   gen: Generic.Aux[InputTuple, InputRepr], // Generic: (VData[A], VData[B]) => VData[A] :: VData[B] :: HNil
   vprod: VProduct.Aux[InputRepr, UnboxedData] // VProduct: VData[A] :: VData[B] :: HNil => A :: B :: HNil
  )
  : String = {
    val inputAsHlistOfVersionedData: InputRepr = gen.to(p)
    val unwrappedVersionedData: UnboxedData = vprod.to(inputAsHlistOfVersionedData)
    unwrappedVersionedData.toString
  }

  println(bar(Tuple1(vdata))) // have to wrap in Tuple1 because ... otherwise Generic deconstructs it improperly
  println(bar(vdata, vdata))


  /**
    * Abstract over function arity.
    *   See Shapeless FnToProduct, and Astronaut's Guide 4.3: Chaining dependent functions
    *
    * @param versionedInput InputTuple
    * @param f Function
    * @param gen Generic.Aux: InputTuple => InputRepr
    * @param vprod VProduct: InputRepr => UnboxedData
    * @param fp FnToProduct: transforms FunctionN to Function1 taking in UnboxedData
    * @tparam InputTuple inputs of arbitrary arity of form (VData[A], VData[B], ...)
    * @tparam InputRepr InputTuple converted to generic form, e.g. VData[A] :: VData[B] :: HNil
    * @tparam UnboxedData an HList of the inputs to Function, without the VData[_] wrapper
    * @tparam Function function of arbitrary arity: (A, B) => C
    * @tparam Result result type of Function
    * @return VersionedData[Result]
    */
  def applyWithVProd[InputTuple <: Product, InputRepr <: HList, UnboxedData <: HList, Function, Result]
  (versionedInput: InputTuple)
  (f: Function)
  (implicit
   gen: Generic.Aux[InputTuple, InputRepr],
   vprod: VProduct.Aux[InputRepr, UnboxedData],
   fp: FnToProduct.Aux[Function, UnboxedData => Result]
   //     inputIsVersionedData: LUBConstraint[InputRepr, Versioned[_]] // technically redundant though makes things clear
  )
  : Result = {
    val inputAsHlistOfVersionedData: InputRepr = gen.to(versionedInput)
    val unwrappedVersionedData: UnboxedData = vprod.to(inputAsHlistOfVersionedData)
    val answer = f.toProduct(unwrappedVersionedData)
    answer
  }

  val f: (String, String) => String = (a: String, b: String) => a + b
  val g: (String) => String = (s: String) => s + "!!!"
  println(applyWithVProd((vdata, vdata))(f))
  println(applyWithVProd(Tuple1(vdata))(g))

}
