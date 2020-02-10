package archive.calcs.calcasfunction.prototype3apis

import shapeless.HNil

import scala.util.Try
import shapeless.{::, Generic, HList, HNil}

object RawData{
  import archive.calcs.calcasfunction.prototype3apis.CommonRepositories._

  // TODO find ways to enforce consistent DB types; using helpers like this to wrap is a good start.

  val vStr: Versioned[String] = Persistable[String].attachRepr("howdy", CalcRun("dummyCalc", 34324))

}


object DemoPrototype3 extends App {

  import archive.calcs.calcasfunction.prototype3apis.CommonRepositories._
  import RawData._

  // TODO(nick.bradford) IntelliJ will make either the Calc creation or its calling red, though it will compile fine

  val g: (String) => String = (s: String) => s + "!!!"
//  val calc1: (Versioned[String] :: HNil) => Try[Versioned[String]] = Calc("DemoCalc1", g).bindPersister
//  val result1: Try[Versioned[String]] = calc1(vStr :: HNil)
//
//  val f = (a: String, b: String) => a + b
//  val calc2 = Calc("DemoCalc2", f).bindPersister
//  // : VersionedData[String] ::  VersionedData[String] :: HNil => VersionedData[String]
//  // : [String :: String :: HNil, (String, String) => String, String, VersionedData[String] :: VersionedData[String] :: HNil]
//  val result2: Try[Versioned[String]] = calc2(vStr :: vStr :: HNil)
//
//  // Actualy try creating a pipeline
//  val x: Try[Versioned[String]] = for {
//    one <- result1
//    two <- result2
//  } yield two


  // TODO separate binding stage, play with casting

  val simpleCalc: SimpleCalc[String :: HNil, (String) => String, String, Versioned[String] :: HNil] =
    Calc("DemoCalc1", g).bindPersister // ignore red
  val recast: Calc[Versioned[String] :: HNil, String] = simpleCalc

  import Calc.{Calc1, VEffect}
  val niceAlias: Calc1[String, String] = recast

  println(niceAlias(vStr :: HNil).get)
}
