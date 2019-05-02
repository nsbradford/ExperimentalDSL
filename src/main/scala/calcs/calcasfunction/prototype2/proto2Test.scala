package calcs.calcasfunction.prototype2


import cats.Monad
import cats.effect.IO
import shapeless.ops.function.FnToProduct
import shapeless.syntax.std.function._
import shapeless.{::, Generic, HList, HNil}


object proto2Test extends App {

  import calcs.calcasfunction.prototype2.CommonRepositories._

  val vStr: Versioned[String] = Versioned("howdy", CalcRun("dummyCalc", 34324))

  // TODO(nick.bradford) IntelliJ will make either the Calc creation or its calling red, though it will compile fine

  val g: (String) => String = (s: String) => s + "!!!"
  val calc1: Versioned[String] :: HNil => IO[Versioned[String]] = Calc("DemoCalc1", g).bindPersister
  val result1: IO[Versioned[String]] = calc1(vStr :: HNil)

  val f = (a: String, b: String) => a + b
  val calc2 = Calc("DemoCalc2", f).bindPersister
  // : VersionedData[String] ::  VersionedData[String] :: HNil => VersionedData[String]
  // : [String :: String :: HNil, (String, String) => String, String, VersionedData[String] :: VersionedData[String] :: HNil]
  val result2: IO[Versioned[String]] = calc2(vStr :: vStr :: HNil)

  // Actualy try creating a pipeline
  val x: IO[Versioned[String]] = for {
    one <- result1
    two <- result2
  } yield two


  // TODO separate binding stage, play with casting

  val simpleCalc: SimpleCalc[String :: HNil, (String) => String, String, Versioned[String] :: HNil] =
    Calc("DemoCalc1", g).bindPersister // ignore red
  val recast: Calc[Versioned[String] :: HNil, String] = simpleCalc

  import Calc.{Calc1, VEffect}
  val niceAlias: Calc1[String, String] = simpleCalc


  // TODO import syntax.std.tuple._ to allow passing tuples instead of HLists
  // TODO IntelliJ doesn't infer type of Generic

  import shapeless.syntax.std.tuple._
  val calc2Typed: Calc[Versioned[String] :: Versioned[String] :: HNil, String] = Calc("DemoCalc2", f).bindPersister

  val tupleDemo =
    for {
      result <- calc2Typed((vStr, vStr).productElements) // TODO red underline fixable?
    } yield result

  //  implicit class ConvenientTupleToHListSyntax[T <: Product](tuple: T){
  //    def hl(implicit gen: Generic[T]): gen.Repr = gen.to(tuple)
  //  }


  // TODO chaining
  val smallChain: Calc[Versioned[String] :: HNil, String] = simpleCalc chain simpleCalc
  val demoChain: VEffect[String] = smallChain(vStr :: HNil)


  // TODO nesting
  val smallNested: Calc1[String, String] = smallChain wrapAs "Complicated"
  val demoNested: VEffect[String] = smallNested(vStr :: HNil)

  demoNested.unsafeRunAsync(_ => Unit)


  // TODO parallelization is really ugly...
  import cats.implicits._
  import scala.concurrent.ExecutionContext
  import cats.effect.ContextShift

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val parallelized: IO[Versioned[String]] =
    for { //
      result <- (calc1(vStr :: HNil), calc1(vStr :: HNil))
        .parMapN((a, b) => calc2Typed(a :: b :: HNil))
        .flatten
    } yield result


}
