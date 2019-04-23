package calcs.arityabstracted

import calcs.Repo
import calcs.SharedModel.{CalcName, CalcVersionAssigned, VersionedData}
import shapeless.ops.function.FnToProduct
import shapeless.syntax.std.function._
import shapeless.{::, Generic, HList, HNil}


object ArityCalcPurifiedDemo extends App {

  import calcs.Repo._
  import calcs.CommonRepositories._

  import ArityCalcPurified._

  val vStr: VersionedData[String] = VersionedData("howdy", CalcVersionAssigned(CalcName("dummyCalc"), 34324))
  val g: (String) => String = (s: String) => s + "!!!"
  val f: (String, String) => String = (a: String, b: String) => a + b

  val sraw = ("yes", "please")
  val h = "yes" :: "please" :: HNil

  // TODO(nick.bradford) IntelliJ will make either the Calc creation or its calling red, though it will compile fine

  val calc1 = ArityCalcPurified("DemoCalc1", g) // : VersionedData[String] :: HNil => VersionedData[String]
  val result1 = calc1(vStr :: HNil)

  val calc2 = ArityCalcPurified("DemoCalc2", f)
  val result2 = calc2(vStr :: vStr :: HNil)
  println(result2)
  // : VersionedData[String] ::  VersionedData[String] :: HNil => VersionedData[String]
  // : [String :: String :: HNil, (String, String) => String, String, VersionedData[String] :: VersionedData[String] :: HNil]

}

object PurifiedExperimentsDemo extends App {

  import calcs.Repo._

  val vdata: VersionedData[String] = VersionedData("howdy", CalcVersionAssigned(CalcName("dummyCalc"), 34324))
  val g: (String) => String = (s: String) => s + "!!!"
  val f: (String, String) => String = (a: String, b: String) => a + b

  val sraw = ("yes", "please")
  val h = "yes" :: "please" :: HNil
  val gen = Generic[(String, String)]
  val xs = gen.from(h)

  // we can infer type using only the function, that makes sense

  def underconstrained[UnboxedData <: HList, Function, Result]
    (f: Function)
    (implicit
    fp: FnToProduct.Aux[Function, UnboxedData => Result]
    )
  : fp.Out = f.toProduct

  val xunderconstrained: Function1[String :: String :: HNil, String] = underconstrained(f)
  val resultxunderconstrained: String = xunderconstrained(h)

  // can we infer a reverse generic?

  def underconstrainedGeneric[UnboxedData <: HList, Function, Result, TupleInput]
  (f: Function)
  (implicit
   fp: FnToProduct.Aux[Function, UnboxedData => Result],
   gen: Generic.Aux[TupleInput, UnboxedData]
  )
  : Function1[TupleInput, Result] = {
    val fgen: fp.Out = f.toProduct
    val ftup: TupleInput => Result = (xs: TupleInput) => fgen(gen.to(xs))
    ftup
  }
//
  val xunderconstrainedGeneric: Function1[(String, String), String] = underconstrainedGeneric(f)
  val resultxunderconstrainedGeneric: String = xunderconstrainedGeneric(sraw)

  def fullyGeneric[UnboxedData <: HList, Function, Result, VInputRepr <: HList, VInputTuple]
    (f: Function)
    (implicit
     fp: FnToProduct.Aux[Function, UnboxedData => Result],
     vprod: ArityCalcPurified.VProduct.Aux[UnboxedData, VInputRepr]//,
    )
  : (VInputRepr => VersionedData[Result]) =
  {
    (versionedDataInput: VInputRepr) => {
      val unwrappedVersionedData: UnboxedData = vprod.to(versionedDataInput)
      val result: Result = f.toProduct(unwrappedVersionedData)
      VersionedData(result, CalcVersionAssigned(CalcName("dummyCalc"), 34324))
    }
  }

  val fxfullyGeneric = fullyGeneric(f) // : VersionedData[String] :: VersionedData[String] :: HNil => VersionedData[String]
  val fresultxfullyGeneric: VersionedData[String] = fxfullyGeneric(vdata :: vdata :: HNil) // TODO red in IntelliJ but compiles fine
//
//  val gxfullyGeneric = fullyGeneric(g) // : VersionedData[String] :: HNil => VersionedData[String]
//  val gresultxfullyGeneric: VersionedData[String] = gxfullyGeneric(vdata :: HNil)

//  val givesHelpfulCompileError: VersionedData[String] = gxfullyGeneric("asdf" :: HNil) // TODO nice! infers types properly

}


class ArityCalcPurified[UnboxedData <: HList, Function, Result, VInputRepr <: HList]
  (name: String,
   f: Function)
  (implicit
   fp: FnToProduct.Aux[Function, UnboxedData => Result],
   vprod: ArityCalcPurified.VProduct.Aux[UnboxedData, VInputRepr],
   repoInput: Repo[UnboxedData],
   repoResult: Repo[Result]
  )
  extends (VInputRepr => VersionedData[Result])
{
  def apply(versionedDataInput: VInputRepr): VersionedData[Result] = {
    val unwrappedVersionedData: UnboxedData = vprod.to(versionedDataInput)
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



object ArityCalcPurified {

  def apply[UnboxedData <: HList, Function, Result, VInputRepr <: HList]
    (name: String,
     f: Function)
    (implicit
     fp: FnToProduct.Aux[Function, UnboxedData => Result],
     vprod: ArityCalcPurified.VProduct.Aux[UnboxedData, VInputRepr],
     repoInput: Repo[UnboxedData],
     repoResult: Repo[Result]
    ): ArityCalcPurified[UnboxedData, Function, Result, VInputRepr] = new ArityCalcPurified(name, f)
  // (VInputRepr => VersionedData[Result])

  /**
    * Typeclass representing that UnboxedRepr can be created from a an HList of VersionedData[_]s
    * @tparam UnboxedRepr
    */
  trait VProduct[UnboxedRepr]{
    type VList <: HList
    def to(t: VList): UnboxedRepr
  }

  object VProduct{
    type Aux[Repr, VRepr] = VProduct[Repr]{ type VList = VRepr }

    def apply[UnboxedRepr <: HList](implicit vProduct: VProduct[UnboxedRepr]): VProduct.Aux[UnboxedRepr, vProduct.VList] = vProduct // ignore red

    def create[UnboxedRepr <: HList, VRepr <: HList](f: VRepr => UnboxedRepr): VProduct.Aux[UnboxedRepr, VRepr] = new VProduct[UnboxedRepr]{
      override type VList = VRepr
      override def to(t: VRepr): UnboxedRepr = f(t)
    }
  }

  implicit val hnilIsVProduct: VProduct.Aux[HNil, HNil] = VProduct.create[HNil, HNil](_ => HNil)

  implicit def groupIsVProduct[H, T <: HList]
    (implicit tailProduct: VProduct[T])
  : VProduct.Aux[H :: T, VersionedData[H] :: tailProduct.VList] =
  {
    VProduct.create[H :: T, VersionedData[H] :: tailProduct.VList] {
      case h :: t => h.data :: tailProduct.to(t)
    }
  }

}

