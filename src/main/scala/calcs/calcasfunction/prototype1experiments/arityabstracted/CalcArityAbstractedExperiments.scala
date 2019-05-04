package calcs.calcasfunction.prototype1experiments.arityabstracted

import calcs.calcasfunction.prototype1experiments.Repo
import calcs.calcasfunction.prototype1experiments.Repo
import calcs.calcasfunction.prototype1experiments.SharedModel.{CalcName, CalcVersionAssigned, VersionedData, VersionedDataUnpersisted}
import shapeless.LUBConstraint.<<:
import shapeless.{::, Generic, HList, HNil, LUBConstraint, the}
import shapeless.syntax.std.function._
import shapeless.ops.function._
import shapeless.ops.hlist.IsHCons

import scala.util.Try

//case class Repo2[T] (get: Unit => Any, set: Any => Unit)
//
//trait PriceRepository {
//
//  def price: Repo2[Map[String, Double]]
//
//}
//trait SillyRepository extends Repo[Int]
//
//class Breaks extends PriceRepository with SillyRepository


/**
  * Requires:
  *   Input types are Tuples or HLists
  *   Convert functionN => f.tupled() // and then convert to HList?
  *     Aux pattern for f type => return type
  *   Implicit ev input HList is InputLoggable[T]
  */
object CalcArityAbstractedExperiments extends App {

  // see https://www.scala-exercises.org/shapeless/arity

  import calcs.calcasfunction.prototype1experiments.CommonRepositories._

  val x = Repo[Int :: HNil]

  def applyWithEvd[InputTuple <: Product, Function, Repr <: HList, Result]
    (p: InputTuple)
    (f: Function)
    (implicit
     gen: Generic.Aux[InputTuple, Repr],
     fp: FnToProduct.Aux[Function, Repr => Result],
     repoInput: Repo[Repr],
     repoResult: Repo[Result])
  : Result = {
    val answer = f.toProduct(gen.to(p))
    repoInput.persist(gen.to(p))
    repoResult.persist(answer)
    answer
  }

  val f: (Int, Int, Int) => Int = (a: Int, b: Int, c: Int) => a + b + c
  val z = applyWithEvd((1, 2, 3))(f)
  println(z)

}


object VersionArityAbstracted extends App {

  import calcs.calcasfunction.prototype1experiments.CommonRepositories._

  trait VProduct[T]{
    type HListRepr <: HList
    def to(t: T): HListRepr
  }
  object VProduct{
    type Aux[T, Repr] = VProduct[T]{ type HListRepr = Repr }
    def create[T, Repr <: HList](f: T => Repr): VProduct.Aux[T, Repr] = new VProduct[T]{
      override type HListRepr = Repr
      override def to(t: T): Repr = f(t)
    }
  }


  implicit val hnilIsVProduct: VProduct[HNil] = VProduct.create[HNil, HNil](_ => HNil)

  // I, H <: VersionedData[I]
  implicit def groupIsVProduct[I, H <: VersionedData[I], T <: HList]
    (implicit
     tailProduct: VProduct[T]
    )
  : VProduct.Aux[H :: T, I :: tailProduct.HListRepr] = {

    VProduct.create[H :: T, I :: tailProduct.HListRepr]{
      case h :: t =>
        h.data :: tailProduct.to(t)
    }
  }


  def applyWithVProd[InputTuple <: Product, Function, InputRepr <: HList, Result, UnboxedData <: HList]
    (p: InputTuple)
    (f: Function)
    (implicit
     gen: Generic.Aux[InputTuple, InputRepr], // Generic: (VData[A], VData[B]) => VData[A] :: VData[B] :: HNil
     vprod: VProduct.Aux[InputRepr, UnboxedData], // VProduct: VData[A] :: VData[B] :: HNil => A :: B :: HNil
     fp: FnToProduct.Aux[Function, UnboxedData => Result] // Function2[A, B, C] => Function1[ A :: B :: HNil , C]
    )
//     repoInput: Repo[InputRepr],
//     repoResult: Repo[Result])
  : Result = {
    val inputAsHlistOfVersionedData: InputRepr = gen.to(p)
    val unwrappedVersionedData: UnboxedData = vprod.to(inputAsHlistOfVersionedData)
    val answer = f.toProduct(unwrappedVersionedData)
//    repoInput.persist(gen.to(p))
//    repoResult.persist(answer)
    answer
  }

  val vdata: VersionedData[Int] = VersionedData(7, CalcVersionAssigned(CalcName("dummyCalc"), 34324))

  val f: (Int, Int, Int) => Int = (a: Int, b: Int, c: Int) => a + b + c
//  val z = applyWithVProd((vdata, vdata, vdata))(f)
  //Error:(171, 48) could not find implicit value for parameter vprod: VProduct.Aux[InputRepr,UnboxedData]

  //  println(z)

}


//https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/hlistconstraints.scala
// https://stackoverflow.com/questions/3427345/what-do-and-mean-in-scala-2-8-and-where-are-they-documented
object BothHAndTAreVProd {

  trait VProduct[T]{
    type HListRepr <: HList
    def to(t: T): HListRepr
  }
  object VProduct{
    type Aux[T, Repr] = VProduct[T]{ type HListRepr = Repr }
    def create[T, Repr <: HList](f: T => Repr): VProduct.Aux[T, Repr] = new VProduct[T]{
      override type HListRepr = Repr
      override def to(t: T): Repr = f(t)
    }
  }

  implicit val hnilIsVProduct: VProduct[HNil] = VProduct.create[HNil, HNil](_ => HNil)

  implicit def versionedDataIsProduct[T]: VProduct.Aux[VersionedData[T], T :: HNil] =
  {
    VProduct.create[VersionedData[T], T :: HNil](t => t.data :: HNil)
  }

  // I, H <: VersionedData[I]
  implicit def groupIsVProduct[H, InnerH <: HList, T <: HList, InnerT <: HList, InnerHH, InnerHT <: HList]
    (implicit
     headProduct: VProduct.Aux[H, InnerH],
     tailProduct: VProduct.Aux[T, InnerT],
     headIsHCons: IsHCons.Aux[InnerH, InnerHH, InnerHT]
    )
  : VProduct.Aux[H :: T, InnerHH :: InnerT] = {

    VProduct.create[H :: T, InnerHH :: InnerT]{
      case h :: t =>
//        ::(headProduct.to(h), tailProduct.to(t))
        ::(headIsHCons.head(headProduct.to(h)), tailProduct.to(t))
    }
  }

  def foo[T](vdata: VersionedData[T] :: HNil)
            (implicit vprod: VProduct.Aux[VersionedData[T] :: HNil, T :: HNil])
  : String = {
    vprod.to(vdata).toString
  }

  val vdata: VersionedData[String] = VersionedData("howdy", CalcVersionAssigned(CalcName("dummyCalc"), 34324))

//  println(foo(vdata :: HNil))
}


object JustTIsVProd extends App {

//  import LUBConstraint._


  trait VProduct[T]{
    type HListRepr <: HList
    def to(t: T): HListRepr
  }

  object VProduct{

    type Aux[T, Repr] = VProduct[T]{ type HListRepr = Repr }

    def apply[T <: HList](implicit vProduct: VProduct[T]): VProduct[T] = vProduct

    def create[T <: HList, Repr <: HList]
      (f: T => Repr)
      (implicit inputIsVersionedData: LUBConstraint[T, VersionedData[_]])
    : VProduct.Aux[T, Repr] = new VProduct[T]{
      override type HListRepr = Repr
      override def to(t: T): Repr = f(t)
    }
  }

  implicit val hnilIsVProduct: VProduct[HNil] = VProduct.create[HNil, HNil](_ => HNil)

  // I, H <: VersionedData[I]
  implicit def groupIsVProduct[H, T <: HList]
    (implicit
     tailProduct: VProduct[T],
     hIsVersionedData: H <:< VersionedData[Any],
     tIsVersionedData: LUBConstraint[H :: T, VersionedData[Any]]
    )
  : VProduct[H :: T] = { // , I :: tailProduct.HListRepr

    VProduct.create { // [H :: T, I :: tailProduct.HListRepr]
      case h :: t =>
        h.data :: tailProduct.to(t)
    }
  }

  // DEMO

  def foo[T, Repr](vdata: VersionedData[T] :: HNil)
            (implicit vprod: VProduct.Aux[VersionedData[T] :: HNil, Repr])
  : String = {
    vprod.to(vdata).toString
  }
  val vdata: VersionedData[String] = VersionedData("howdy", CalcVersionAssigned(CalcName("dummyCalc"), 34324))

  val x = VProduct[HNil]
  val y = VProduct[VersionedData[String] :: HNil] // (groupIsVProduct[VersionedData[String], HNil])
}

object CorrectSummoner extends App {

  case class Versioned[+T](data: T, version: CalcVersionAssigned){
//    type Inner = T
  }

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

  implicit val hnilIsVProduct: VProduct[HNil] = VProduct.create[HNil, HNil](_ => HNil)

  // I, H <: VersionedData[I]
//  implicit def groupIsVProduct[H <: Versioned[Any], T <: HList]
//    (implicit tailProduct: VProduct[T])
//  : VProduct.Aux[H :: T, H#Inner :: tailProduct.HListRepr] =
//    VProduct.create[H :: T, H#Inner :: tailProduct.HListRepr] {
//      case h :: t => h.data :: tailProduct.to(t)
//    }

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

  val x = implicitly[VProduct[HNil]]
  val xx = VProduct[Versioned[String] :: HNil] // VProduct.Aux[Versioned[String] :: HNil, String :: HNil]
  val asdf = the[VProduct[Versioned[String] :: HNil]]

  // TODO(nick.bradford) it seems like this requires Shapeless-Generic-like macros in order to preserve the Aux inner type?
//  val yy: String :: HNil = asdf.to(vdata :: HNil)

//  val y: VProduct.Aux[Versioned[String] :: HNil, String :: HNil] = groupIsVProduct[String, HNil]
//  val y: VProduct.Aux[Versioned[String] :: HNil, String :: HNil] = VProduct[Versioned[String] :: HNil, String :: HNil](groupIsVProduct[String, HNil])
//  val z: VProduct.Aux[VersionedData[String] :: HNil, String :: HNil] = VProduct[VersionedData[String] :: HNil]
  //  val z = VProduct[VersionedData[String] :: VersionedData[Int] :: HNil](groupIsVProduct[VersionedData[String], VersionedData[Int] :: HNil, String])

  def foo[T, Repr](vdata: Versioned[T] :: HNil)
                  (implicit vprod: VProduct.Aux[Versioned[T] :: HNil, Repr])
  : String = {
    vprod.to(vdata).toString
  }
  println(foo(vdata :: HNil))


  def applyWithVProd[InputTuple <: Product, Function, InputRepr <: HList, Result, UnboxedData <: HList]
    (p: InputTuple)
    (f: Function)
    (implicit
     gen: Generic.Aux[InputTuple, InputRepr], // Generic: (VData[A], VData[B]) => VData[A] :: VData[B] :: HNil
     vprod: VProduct.Aux[InputRepr, UnboxedData], // VProduct: VData[A] :: VData[B] :: HNil => A :: B :: HNil
     fp: FnToProduct.Aux[Function, UnboxedData => Result] // Function2[A, B, C] => Function1[ A :: B :: HNil , C]
    )
    //     repoInput: Repo[InputRepr],
    //     repoResult: Repo[Result])
  : Result = {
    val inputAsHlistOfVersionedData: InputRepr = gen.to(p)
    val unwrappedVersionedData: UnboxedData = vprod.to(inputAsHlistOfVersionedData)
    val answer = f.toProduct(unwrappedVersionedData)
    //    repoInput.persist(gen.to(p))
    //    repoResult.persist(answer)
    answer
  }


  val f: (String, String) => String = (a: String, b: String) => a + b
//  val z = applyWithVProd
//    [
//      (VersionedData[String], VersionedData[String]),
//      (String, String) => String,
//      VersionedData[String] :: VersionedData[String] :: HNil,
//      String,
//      String :: String :: HNil
//      ]((vdata, vdata))(f)(
//      Generic.apply[(VersionedData[String], VersionedData[String])],
//      groupIsVProduct[VersionedData[String], VersionedData[String] :: HNil],
//      implicitly[FnToProduct.Aux[(String, String) => String, String :: String :: HNil => String]]
//    )

//  println(applyWithVProd(vdata, vdata)(f)) // TODO need better chaining of dependent types
}


object GetGenericFToWork extends App {

  case class Versioned[+T](data: T, version: CalcVersionAssigned)

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