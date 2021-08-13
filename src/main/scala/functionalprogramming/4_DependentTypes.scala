package functionalprogramming


/**
  * DEPENDENT TYPES
  * One of many "advanced" techniques
  *
  * DISCUSS: What could go wrong with this function?
  */
object DependentTypes_Motivation {
  def travelTime(distance: Double, speed: Double): Double = {
    distance * speed // ouch! Somebody skipped physics:   time = distance / speed
  }
}

/**
  * This is a step further than before, where we had custom types;
  *   we need to "lift the algebra of our operations into the type system",
  *   and basically create our own DSL!
  *
  * DISCUSS: any ideas on how we could do this?
  *   Any ideas on how we could do this for ANY unit system?
  *
  * We're going to go in reverse order: first we'll see the solution, then explore the (messy) details...
  */

object DependentTypes_FoundationalDSL {

  // The basis of our DSL is a wrapper class around Double.
  //   Notice the return type uses `evidence.Out` - this is a "dependant type" we don't know yet!
  case class Numeric[A](in: Double){
    def *[B](other: Numeric[B])(implicit canMultiply: A CanMultiplyBy B): Numeric[canMultiply.Out] =
      canMultiply.multiply(this, other)
    def /[B](other: Numeric[B])(implicit canDivide: A CanDivideBy B): Numeric[canDivide.Out] =
      canDivide.divide(this, other)
  }

  // Now we have some types of "evidence" that will form the basis of "proofs" in our DSL.
  //   Notice again the unimplemented `type Out` - this is a "dependant type" we don't know yet!
  trait CanMultiplyBy[A, B]{
    type Out
    def multiply(a: Numeric[A], b: Numeric[B]): Numeric[Out] = Numeric[Out](a.in * b.in)
  }
  trait CanDivideBy[Num, Denom] {
    type Out
    def divide(num: Numeric[Num], denom: Numeric[Denom]): Numeric[Out] = Numeric[Out](num.in / denom.in)
  }
  // this is just some boilerplate, see "Aux pattern"
  type CanMultiplyByAux[A, B, Result] = CanMultiplyBy[A, B]{ type Out = Result }
  type CanDivideByAux[Num, Denom, Result] = CanDivideBy[Num, Denom]{ type Out = Result }

  // now, let's give ourselves a way to easily declare that types can CanMultiply/CanDivide
  //  this is basically just more boilerplate...
  def dividing[A]: DividingBuilder1[A] = new DividingBuilder1
  class DividingBuilder1[A]{ def by[B]: DividingBuilder2[A, B] = new DividingBuilder2() }
  class DividingBuilder2[A, B]{ def yields[C]: CanDivideByAux[A, B, C] = new CanDivideBy[A, B]{ type Out = C } }
  def multiply[A]: MultiplyBuilder1[A] = new MultiplyBuilder1
  class MultiplyBuilder1[A]{ def by[B]: MultiplyBuilder2[A, B] = new MultiplyBuilder2() }
  class MultiplyBuilder2[A, B]{ def toGet[C]: CanMultiplyByAux[A, B, C] = new CanMultiplyBy[A, B]{ type Out = C } }

  // Define a set of programmatic rules to get the compiler to build more of the operations FOR YOU BEHIND THE SCENES!
  //  i.e. if you know you can multiply safely, you can also divide!
  implicit def ifYouCanMultiplyYouCanDivide_1[A, B, Out](implicit ev: CanMultiplyByAux[A, B, Out]): CanDivideByAux[Out, A, B] = dividing [Out] .by [A] .yields [B]
  implicit def ifYouCanMultiplyYouCanDivide_2[A, B, Out](implicit ev: CanMultiplyByAux[A, B, Out]): CanDivideByAux[Out, B, A] = dividing [Out] .by [B] .yields [A]
  implicit def ifYouCanMultiplyYouCanDivide_3[A, B, Out](implicit ev: CanMultiplyByAux[A, B, Out]): CanDivideByAux[A, Out, B] = dividing [A] .by [Out] .yields [B]
  implicit def ifYouCanMultiplyYouCanDivide_4[A, B, Out](implicit ev: CanMultiplyByAux[A, B, Out]): CanDivideByAux[B, Out, A] = dividing [B] .by [Out] .yields [A]
}

object DependentTypes_MetersAndSecondsUnitSystem {
  import DependentTypes_FoundationalDSL._

  // Let's start by defining some sample units... notice these are NOT new classes, JUST TYPES
  type MetersUnit
  type SecondsUnit
  type MetersPerSecondUnit
  type Meters = Numeric[MetersUnit]
  type Seconds = Numeric[SecondsUnit]
  type MetersPerSecond = Numeric[MetersPerSecondUnit]

  // a bit more syntax...
  implicit class NewRichDouble(in: Double){
    def meters: Meters = Numeric[MetersUnit](in)
    def seconds: Seconds = Numeric[SecondsUnit](in)
    def metersPerSecond: MetersPerSecond = Numeric[MetersPerSecondUnit](in)
  }

  // TODO how do we express that you can:  multiply [MetersUnit] .by [SecondsUnit] .toGet [MetersPerSecondUnit] ?
  implicit val rule1: CanMultiplyByAux[MetersUnit, SecondsUnit, MetersPerSecondUnit] =
    multiply [MetersUnit] .by [SecondsUnit] .toGet [MetersPerSecondUnit]
  // that's it! no need to implement * and / operations for our custom units, they're handled by our DSL!
}

object DependentTypes_ApplicationCodeRaw extends App {
  import DependentTypes_FoundationalDSL._
  import DependentTypes_MetersAndSecondsUnitSystem._

  // Now, the application code!
  def travelTime(distance: Meters, speed: MetersPerSecond): Seconds = {
    val time: Numeric[SecondsUnit] = distance / speed  // even though the `/` function has no idea the type, the compiler figures it out!
//    val time = distance * speed // doesn't compile anymore! :D

    val x: Meters = 1.meters
    val y: MetersPerSecond = x * time
    time
  }

  // ah, nice syntax!
  println(travelTime(distance = 20.meters, speed = 5.metersPerSecond))

  // Given all our type inference rules in our DSL, defining a new rule for our algebra is super easy!
  //  Can we try it now with: Meters * Meters = MetersSquared?
}

/**
  * TEASER: one layer of rule-derivation is nothing;
  *   you can get the compiler to recursively apply rules,
  *   e.g. "automatic type class derivation",
  *
  *   Stay tuned for next time...
  */