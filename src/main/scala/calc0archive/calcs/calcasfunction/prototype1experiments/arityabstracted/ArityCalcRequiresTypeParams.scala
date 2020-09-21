package archive.calcs.calcasfunction.prototype1experiments.arityabstracted

import archive.calcs.calcasfunction.prototype1experiments.Repo
import archive.calcs.calcasfunction.prototype1experiments.SharedModel.{CalcName, CalcVersionAssigned, VersionedData}
import shapeless.ops.function.FnToProduct
import shapeless.syntax.std.function._
import shapeless.{::, Generic, HList, HNil, the}



object ArityCalcRequiresTypeParamsDemo extends App {

  import Repo._
  import archive.calcs.calcasfunction.prototype1experiments.CommonRepositories._

  val vdata: VersionedData[String] = VersionedData("howdy", CalcVersionAssigned(CalcName("dummyCalc"), 34324))


  // TODO One-arg function requires wrapping in Tuple1 to find Generic Representation
  val g: (String) => String = (s: String) => s + "!!!"
  val calc1 =
    new ArityCalcRequiresTypeParams[
      Tuple1[VersionedData[String]],
      VersionedData[String] :: HNil,
      String :: HNil,
      (String) => String,
      String
      ](
      "DemoCalc2",
      g
    )

  val result1: VersionedData[String] = calc1(Tuple1(vdata))
  println(result1)

  // TODO multi-arg function still requires implicit type params to backtrack out input arg types
  val f: (String, String) => String = (a: String, b: String) => a + b
  val calc2 =
    new ArityCalcRequiresTypeParams[
      (VersionedData[String], VersionedData[String]),
      VersionedData[String] :: VersionedData[String] :: HNil,
      String :: String :: HNil,
      (String, String) => String,
      String
      ](
      "DemoCalc2",
      f
    )

  val result2: VersionedData[String] = calc2((vdata, vdata))
  println(result2)

}



/**
  * Abstract over function arity.
  *   See Shapeless FnToProduct, and Astronaut's Guide 4.3: Chaining dependent functions
  *
  * @param VersionedDataInput VInputTuple
  * @param f Function
  * @param gen Generic.Aux: VInputTuple => VInputRepr
  * @param vprod VProduct: VInputRepr => UnboxedData
  * @param fp FnToProduct: transforms FunctionN to Function1 taking in UnboxedData
  * @tparam VInputTuple inputs of arbitrary arity of form (VData[A], VData[B], ...)
  * @tparam VInputRepr VInputTuple converted to generic form, e.g. VData[A] :: VData[B] :: HNil
  * @tparam UnboxedData an HList of the inputs to Function, without the VData[_] wrapper
  * @tparam Function function of arbitrary arity: (A, B) => C
  * @tparam Result result type of Function
  * @return VersionedDataData[Result]
  */
class ArityCalcRequiresTypeParams[VInputTuple <: Product, VInputRepr <: HList, UnboxedData <: HList, Function, Result]
  (name: String,
   f: Function)
  (implicit
   gen: Generic.Aux[VInputTuple, VInputRepr],
   vprod: ArityCalcRequiresTypeParams.VProduct.Aux[VInputRepr, UnboxedData],
   fp: FnToProduct.Aux[Function, UnboxedData => Result],
   repoInput: Repo[UnboxedData],
   repoResult: Repo[Result]
  )
  extends (VInputTuple => VersionedData[Result])
{
  def apply(VersionedDataInput: VInputTuple): VersionedData[Result] = {
    val inputAsHlistOfVersionedData: VInputRepr = gen.to(VersionedDataInput)
    val unwrappedVersionedData: UnboxedData = vprod.to(inputAsHlistOfVersionedData)
    val result: Result = f.toProduct(unwrappedVersionedData)

    // TODO add actual side-effects here
//    calcVersionAssigned <- this.calcRepository.requisitionNewRunId(this.fullyQualifiedName)
//    _ <- this.logOutput(calcVersionAssigned)(evR)
//    _ <- this.logSingleInput(calcVersionAssigned)(vT1)(ev1)
//    pureResult = f(vT1.data)
//    persistedResult: VersionedData[R] <- this.evR.persistWrap(VersionedDataUnpersisted(pureResult, calcVersionAssigned))

    VersionedData(result, CalcVersionAssigned(CalcName("dummyCalc"), 34324))
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

