package calcs.arityabstracted

import calcs.{Repo}
import calcs.SharedModel.{CalcName, CalcVersionAssigned, VersionedData}
import shapeless.ops.function.FnToProduct
import shapeless.syntax.std.function._
import shapeless.{::, Generic, HList, HNil, the}



object ArityCalcRequiresTypeParamsDemo extends App {

  import calcs.CommonRepositories._
  import calcs.Repo
  import calcs.Repo._

  val x = Repo[String :: HNil]

  val vdata: VersionedData[String] = VersionedData("howdy", CalcVersionAssigned(CalcName("dummyCalc"), 34324))
  val f: (String, String) => String = (a: String, b: String) => a + b
  val g: (String) => String = (s: String) => s + "!!!"

  val calc = new ArityCalcRequiresTypeParams[
    (VersionedData[String], VersionedData[String]),
    VersionedData[String] :: VersionedData[String] :: HNil,
    String :: String :: HNil,
    (String, String) => String,
    String](
    f
  )

  val result: VersionedData[String] = calc((vdata, vdata))
  println(result)
  //  println(applyWithVProd(Tuple1(vdata))(g))
}



/**
  * Abstract over function arity.
  *   See Shapeless FnToProduct, and Astronaut's Guide 4.3: Chaining dependent functions
  *
  * @param VersionedDataInput InputTuple
  * @param f Function
  * @param gen Generic.Aux: InputTuple => InputRepr
  * @param vprod VProduct: InputRepr => UnboxedData
  * @param fp FnToProduct: transforms FunctionN to Function1 taking in UnboxedData
  * @tparam InputTuple inputs of arbitrary arity of form (VData[A], VData[B], ...)
  * @tparam InputRepr InputTuple converted to generic form, e.g. VData[A] :: VData[B] :: HNil
  * @tparam UnboxedData an HList of the inputs to Function, without the VData[_] wrapper
  * @tparam Function function of arbitrary arity: (A, B) => C
  * @tparam Result result type of Function
  * @return VersionedDataData[Result]
  */
class ArityCalcRequiresTypeParams[InputTuple <: Product, InputRepr <: HList, UnboxedData <: HList, Function, Result]
  (f: Function)
  (implicit
   gen: Generic.Aux[InputTuple, InputRepr],
   vprod: ArityCalcRequiresTypeParams.VProduct.Aux[InputRepr, UnboxedData],
   fp: FnToProduct.Aux[Function, UnboxedData => Result],
   repoInput: Repo[UnboxedData],
   repoResult: Repo[Result]
  )
  extends (InputTuple => VersionedData[Result])
{
  def apply(VersionedDataInput: InputTuple): VersionedData[Result] = {
    val inputAsHlistOfVersionedDataData: InputRepr = gen.to(VersionedDataInput)
    val unwrappedVersionedDataData: UnboxedData = vprod.to(inputAsHlistOfVersionedDataData)
    val answer = f.toProduct(unwrappedVersionedDataData)

    // TODO add actual side-effects here
//    calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
//    _ <- this.logOutput(calcVersionAssigned)(evR)
//    _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
//    pureResult = f(vT1.data)
//    persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))

    VersionedData(answer, CalcVersionAssigned(CalcName("dummyCalc"), 34324))
  }
}

object ArityCalcRequiresTypeParams {

  /**
    * Typeclass representing that T is a Product of VersionedData[_]s
    * @tparam T
    */
  trait VProduct[T]{
    type HListRepr <: HList
    def to(t: T): HListRepr
  }

  object VProduct{
    type Aux[T, Repr] = VProduct[T]{ type HListRepr = Repr }

    def apply[T <: HList](implicit vProduct: VProduct[T]): VProduct.Aux[T, vProduct.HListRepr] = vProduct // ignore red

    def create[T <: HList, Repr <: HList](f: T => Repr): VProduct.Aux[T, Repr] = new VProduct[T]{
      override type HListRepr = Repr
      override def to(t: T): Repr = f(t)
    }
  }

  implicit val hnilIsVProduct: VProduct.Aux[HNil, HNil] = VProduct.create[HNil, HNil](_ => HNil)

  implicit def groupIsVProduct[H, T <: HList]
    (implicit tailProduct: VProduct[T])
  : VProduct.Aux[VersionedData[H] :: T, H :: tailProduct.HListRepr] =
  {
    VProduct.create[VersionedData[H] :: T, H :: tailProduct.HListRepr] {
      case h :: t => h.data :: tailProduct.to(t)
    }
  }

}

