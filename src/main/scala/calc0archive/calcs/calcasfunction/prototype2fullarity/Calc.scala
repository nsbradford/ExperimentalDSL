package archive.calcs.calcasfunction.prototype2fullarity

import cats.Monad
import cats.effect.IO
import shapeless.ops.function.FnToProduct
import shapeless.syntax.std.function._
import shapeless.{::, Generic, HList, HNil}

import scala.language.higherKinds


/**
  * TODO There's no technical reason WrappedCalc and ChainedCalc need to have separate types;
  *   done here as prototype because I suspect it may help in debugging
  *
  * @tparam VInputRepr
  * @tparam Result
  */
sealed trait Calc[VInputRepr, Result] extends (VInputRepr => IO[Versioned[Result]]) {

  topCalc =>

  def chain[Output](calc: Calc[Versioned[Result] :: HNil, Output])
  : Calc[VInputRepr, Output] = {
    new ChainedCalc(this, calc)
  }

  def wrapAs(name: String)
            (implicit persistable: Persistable[Result],
             metadataRepository: MetadataRepository)
  : Calc[VInputRepr, Result] = {
    new WrappedCalc(CalcName(name), topCalc)
  }

//  def chain[Output](calc: Calculator[Versioned[Result] :: HNil, Output])
//  : Calculator[VInputRepr, Output] =
//  {
//    new Calculator[VInputRepr, Output]{
//      override def apply(versionedDataInput: VInputRepr): IO[Versioned[Output]] =
//        for {
//          result1 <- topCalc.apply(versionedDataInput)
//          result2 <- calc(result1 :: HNil)
//        } yield result2
//    }
//  }

}

class WrappedCalc[VInputRepr, Result]
  (val name: CalcName,
   val innerCalc: Calc[VInputRepr, Result])
  (implicit
   persistable: Persistable[Result], // just needed for writing metadata
   metadataRepository: MetadataRepository
  )
  extends Calc[VInputRepr, Result]
{
  override def apply(versionedDataInput: VInputRepr): IO[Versioned[Result]] =
    for {
      calcRun <- this.metadataRepository.requisitionNewRunId(this.name)
      //      _ <- metadataRepository.logInput(InputRecord[T](calcRun, inputCalc = CalcRun.unknown))(persistable)
      _ <- metadataRepository.logOutput(OutputRecord[Result](calcRun))(persistable)
      persistedResult <- innerCalc(versionedDataInput)
      _ <- metadataRepository.logEquivalence(HierarchyRecord(upper = calcRun, inner = persistedResult.version))
    } yield persistedResult
}


/**
  * Intermediate representation of anonymous Calc1 andThen Calc2
  *
  * TODO have an internal HList of the chain of archive.calcs?
  */
class ChainedCalc[VInputRepr1, Result1, Result2](f1: Calc[VInputRepr1, Result1],
                                                 f2: Calc[Versioned[Result1] :: HNil, Result2])
  extends Calc[VInputRepr1, Result2] {

  def apply(versionedDataInput: VInputRepr1): IO[Versioned[Result2]] =
    for {
      result1 <- f1(versionedDataInput)
      result2 <- f2(result1 :: HNil)
    } yield result2

}


/**
  * An intermediate builder allows you to separately bind and pass in
  *   a Persister and Metadata repository.
  */
class CalcBuilder[UnboxedInputs <: HList, Function, Result, VInputRepr <: HList]
  (val name: CalcName,
   val f: Function)
  (implicit
   fp: FnToProduct.Aux[Function, UnboxedInputs => Result],
   vprod: Calc.VProduct.Aux[UnboxedInputs, VInputRepr] //,
  )
{
  def bindPersister(implicit persistable: Persistable[Result],
                    metadataRepository: MetadataRepository)
  : SimpleCalc[UnboxedInputs, Function, Result, VInputRepr] = {
    new SimpleCalc[UnboxedInputs, Function, Result, VInputRepr](name, f)
  }
}


/**
  * TODO abstract over F[_] : Monad, and Applicative, see https://typelevel.org/cats-tagless/
  * TODO log asynchonously
  */
class SimpleCalc[UnboxedInputs <: HList, Function, Result, VInputRepr <: HList]
  (val name: CalcName,
   val f: Function)
  (implicit
   fp: FnToProduct.Aux[Function, UnboxedInputs => Result],
   vprod: Calc.VProduct.Aux[UnboxedInputs, VInputRepr],
   persistable: Persistable[Result],
   metadataRepository: MetadataRepository
   //   repoInput: Repo[UnboxedInputs], // TODO figure out DB Repr model
  )
  extends Calc[VInputRepr, Result]
{

  def apply(versionedDataInput: VInputRepr): IO[Versioned[Result]] =
    for {
      calcRun <- this.metadataRepository.requisitionNewRunId(this.name)
      unboxedData = vprod.to(versionedDataInput)
//      _ <- metadataRepository.logInput(InputRecord[T](calcRun, inputCalc = CalcRun.unknown))(persistable)
      _ <- metadataRepository.logOutput(OutputRecord[Result](calcRun))(persistable) // this must be in a transaction
      result = f.toProduct(unboxedData) // this must be in a transaction
      persistedResult <- persistable.persistWrap(VersionedUnpersisted[Result](result, calcRun)) // this must be in a transaction
    } yield persistedResult
}


object Calc {

  type Calc1[UnboxedInput, Result] = Calc[Versioned[UnboxedInput] :: HNil, Result]
  type VEffect[T] = IO[Versioned[T]]


  /**
    * Extra Apply just for the String-CalcName conversion, super annoying
    */
  def apply[UnboxedInputs <: HList, Function, Result, VInputRepr <: HList]
    (name: String,
     f: Function)
    (implicit
     fp: FnToProduct.Aux[Function, UnboxedInputs => Result],
     vprod: Calc.VProduct.Aux[UnboxedInputs, VInputRepr] //,
    ): CalcBuilder[UnboxedInputs, Function, Result, VInputRepr] = new CalcBuilder(CalcName(name), f)


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
  : VProduct.Aux[H :: T, Versioned[H] :: tailProduct.VList] =
  {
    VProduct.create[H :: T, Versioned[H] :: tailProduct.VList] {
      case h :: t => h.get :: tailProduct.to(t)
    }
  }

}

