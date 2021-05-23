package introtofp

// ================================================================================
// Intro
/**
  * INTRO TO TYPE-DRIVEN DEVELOPMENT AND FUNCTIONAL PROGRAMMING
  *
  * Goal for this workshop: an easy, fun, practical intro to a set of very high-level tools,
  *   which may broaden how you think about programming.
  */
/**
  * Details:
  *
  * Types and modelling:
  *   - OO vs FP design: Data vs Functional extensibility
  *   - Modeling with Algebraic Data Types (Sums and Products)
  *   - (?) Ad-hoc polymorphism with Type Classes (as opposed to Subtyping or Parametric)
  *   - Dependent types
  *
  * Useful type classes:
  *   - Functors for static, linear operations
  *   - Applicatives for static, parallel operations
  *   - Monads for dynamic, dependent computations (yes, we're going to explain Monads and it'll be super easy!)
  *   - Interesting Monads: Future, Try, IO, ...
  *
  * Teasers for advanced topics
  *   - Tagless Final DSL
  *   - Free Monads for embedded DSLs
  *   - Automatic type class derivation
  *   - Traverse for inverting contexts (using Applicative + Foldable)
  *
  * Objectives to keep in mind:
  *   - I want the compiler to do AS MUCH WORK AS POSSIBLE
  *   - I want to elegantly express VERY HIGH LEVEL common code patterns
  * Turns out, these are very complementary goals.
  */

// ================================================================================
// OO vs FP design: Data vs Functional extensibility
/**
  * DISCUSS:
  *   -What is Functional Programming?
  *   -How is it different from Object-Oriented Design? or is it complementary?
  *   -What tradeoffs are involved?
  */
/**
  * At the highest level: what does functional programming offer?
  *
  * REFERENTIAL TRANSPARENCY: expressions can always be substituted for values
  *   (no side effects; "pure"), you can more easily reason about
  *   and "compose" large programs from small building blocks.
  *
  * Second-order effect:
  *   Object-Oriented architectures => easy to add DATA TYPES
  *   Functional architectures      => easy to add FUNCTIONALITY
  */
object Animals_ObjectOriented {
  abstract class Animal{
    def noise: String
  }
  class Dog extends Animal {
    override def noise: String = "Woof"
  }
  class Cat extends Animal {
    override def noise: String = "Meow"
  }
}

object Animals_FunctionalProgramming extends App {
  sealed abstract class Animal
  case class Dog() extends Animal
  case class Cat() extends Animal

  // pattern matching! now that's a nice feature (coming to Python soon...)
  def noise(animal: Animal): String = animal match {
    case Dog() => "Woof"
//    case Cat() => "Meow" // what if I remove this line? will i get an error at runtime or compile time?
  }

  println(noise(Cat()))
}

/**
  * What if we want to declare the following interface,
  *   and we want it to work for not only Dogs, but Strings too?
  */
object TypeclassMotivation{
  type CustomBinaryFormat = String // imagine this is a fancy class
  trait CustomDbWritable {
    def customBinary(): CustomBinaryFormat
  }
  def writeToDB(bin: CustomBinaryFormat): Unit = () // imagine this is also fancy
  def serializeAndWriteToDb(writable: CustomDbWritable): Unit = {
    writeToDB(writable.customBinary())
  }
}

/**
  * Turns out this has a name: Ad-hoc Polymorphism
  *   The types of polymorphism we're previously familiar with are Subtyping (inheritance) and Parametric (using Generics).
  *   See: https://en.wikipedia.org/wiki/Polymorphism_(computer_science)
  */
object Animals_Typeclass extends App {
  type CustomBinaryFormat = String // imagine this is a fancy class

  // first a new trait - notice the type parameter
  trait CustomDbWritable[T] {
    def customBinary(t: T): CustomBinaryFormat
  }

  // Now we need the type class instances to provide "evidence" for particular types.
  // `implicit` is a fancy keyword that puts things into an additional "implicit" scope,
  //    which allows the compiler to try to automatically resolve.
  //    This can create some pretty wild (though completely safe!) functionality, as we'll see soon...
  import Animals_FunctionalProgramming._
  implicit object DogIsDbWritable extends CustomDbWritable[Dog]{
    override def customBinary(t: Dog): CustomBinaryFormat = "CustomBinaryDoggo!"
  }
  implicit object StringIsDbWritable extends CustomDbWritable[String]{
    override def customBinary(t: String): CustomBinaryFormat = s"CustomBinaryString $t"
  }

  // finally, some boilerplate for syntactic sugar to look like it's a native method (called an "extension method")
  implicit class RichWritable[T](in: T)(implicit writable: CustomDbWritable[T]){
    def customBinary: CustomBinaryFormat = writable.customBinary(in)
  }

  println("hello world!".customBinary)
  println(Dog().customBinary)
//  println(1.customBinary) // doesn't work!
}

object Animals_Typeclass_App extends App {
  import Animals_FunctionalProgramming._
  import introtofp.Animals_Typeclass._

  def writeToDB(bin: CustomBinaryFormat): Unit = () // not important
  def serializeAndWriteToDb[T : CustomDbWritable](writable: T): CustomBinaryFormat = {
    val bin = writable.customBinary
    writeToDB(bin)
    bin // in scala, there's no explicit 'return'; it's always just the last statement in a block.
  }
  // A note on implicits: the following two signatures are identical.
  def serializeAndWriteToDb2[T : CustomDbWritable](writable: T): CustomBinaryFormat = ???
  def serializeAndWriteToDb3[T](writable: T)(implicit ev: CustomDbWritable[T]): CustomBinaryFormat = ???

  println(serializeAndWriteToDb("Hello world!"))
  println(serializeAndWriteToDb(Dog()))
//  println(serializeAndWriteToDb(1)) // doesn't work, there's no type class evidence!
}

// ================================================================================
// Dependent Types

object DependentTypes_Motivation {
  // DISCUSS: What could go wrong with this function?
  def travelTime(distance: Double, speed: Double): Double = {
    distance - speed // ouch! Somebody skipped physics...
  }

  // How about this?
  // `travelTime(distanceInMeters, speedInMilesPerHour)`
}

/**
  * Let's talk about "lifting the algebra of our operations into the type system",
  *   and basically create our own DSL!
  *
  * DISCUSS: any ideas on how we could do this?
  */
object DependentTypes_FoundationalDSL {

  // The basis of our DSL is a wrapper class around Double.
  //   Notice the return type uses `evidence.Out` - this is a "dependant type" we don't know yet!
  case class Numeric[A](in: Double){
    def -(other: Numeric[A]): Numeric[A] = Numeric[A](in - other.in)
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

  // now, let's give ourselves a way to declare that we're able to
  def dividing[A]: DividingBuilder1[A] = new DividingBuilder1
  class DividingBuilder1[A]{ def by[B]: DividingBuilder2[A, B] = new DividingBuilder2() }
  class DividingBuilder2[A, B]{ def yields[C]: CanDivideByAux[A, B, C] = new CanDivideBy[A, B]{ type Out = C } }
  def multiply[A]: MultiplyBuilder1[A] = new MultiplyBuilder1
  class MultiplyBuilder1[A]{ def by[B]: MultiplyBuilder2[A, B] = new MultiplyBuilder2() }
  class MultiplyBuilder2[A, B]{ def toGet[C]: CanMultiplyByAux[A, B, C] = new CanMultiplyBy[A, B]{ type Out = C } }

  // Define a set of programmatic rules to get the compiler to programmatically build more of the operations...
  //  i.e. if you know you can multiply safely, you can also divide!
  implicit def ifYouCanMultiplyYouCanDivide_1[A, B, Out](implicit ev: CanMultiplyByAux[A, B, Out]): CanDivideByAux[Out, A, B] = dividing [Out] .by [A] .yields [B]
  implicit def ifYouCanMultiplyYouCanDivide_2[A, B, Out](implicit ev: CanMultiplyByAux[A, B, Out]): CanDivideByAux[Out, B, A] = dividing [Out] .by [B] .yields [A]
  implicit def ifYouCanMultiplyYouCanDivide_3[A, B, Out](implicit ev: CanMultiplyByAux[A, B, Out]): CanDivideByAux[A, Out, B] = dividing [A] .by [Out] .yields [B]
  implicit def ifYouCanMultiplyYouCanDivide_4[A, B, Out](implicit ev: CanMultiplyByAux[A, B, Out]): CanDivideByAux[B, Out, A] = dividing [B] .by [Out] .yields [A]
}

object DependentTypes_MetersAndSecondsUnitSystem {
  import DependentTypes_FoundationalDSL._

  // Let's start by defining some sample units...
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

  // Given all our type inference rules, defining a new rule for our algebra is super easy!
  //  The key is, adding new units and rules is only a couple lines - the rest is generated by the compiler :D
  implicit val rule1: CanMultiplyByAux[MetersUnit, SecondsUnit, MetersPerSecondUnit] =
    multiply [MetersUnit] .by [SecondsUnit] .toGet [MetersPerSecondUnit]   // look how intuitive defining a new rule is!
}

object DependentTypes_ApplicationCode extends App {
  import DependentTypes_FoundationalDSL._
  import DependentTypes_MetersAndSecondsUnitSystem._

  // Now, the application code!
  def travelTime(distance: Meters, speed: MetersPerSecond): Seconds = {
    val time: Seconds = distance / speed  // even though the `/` function has no idea the type, the compiler figures it out!
    time
  }

  println(travelTime(distance = 20.meters, speed = 5.metersPerSecond))
}