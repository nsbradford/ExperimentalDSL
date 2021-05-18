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
    case Cat() => "Meow" // what if I remove this line? will i get an error at runtime or compile time?
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

object Animals_Typeclass extends App {
  type CustomBinaryFormat = String // imagine this is a fancy class

  // first a new trait - notice the type parameter
  trait CustomDbWritable[T] {
    def customBinary(t: T): CustomBinaryFormat
  }

  // Now we need the type class instances to provide "evidence" for particular types
  import Animals_FunctionalProgramming._
  implicit object DogIsDbWritable extends CustomDbWritable[Dog]{
    override def customBinary(t: Dog): CustomBinaryFormat = "CustomBinaryDoggo!"
  }
  implicit object StringIsDbWritable extends CustomDbWritable[String]{
    override def customBinary(t: String): CustomBinaryFormat = s"CustomBinaryString $t"
  }

  // finally, some syntactic sugar ("extension method") to look like it's a native method
  //  (this is annoying boilerplate that's going away in Scala 3, just ignore...)
  implicit class RichWritable[T : CustomDbWritable](in: T){
    def customBinary: CustomBinaryFormat = implicitly[CustomDbWritable[T]].customBinary(in)
  }
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

  println(serializeAndWriteToDb("Hello world!"))
  println(serializeAndWriteToDb(Dog()))
//  println(serializeAndWriteToDb(1)) // doesn't work, there's no type class evidence!
}

// ================================================================================
// Dependent Types

object DependentTypesMotivation {
  // DISCUSS: What could go wrong with this function?
  def travelTime(startPoint: Double, endPoint: Double, speed: Double): Double = {
    endPoint - speed // ouch! Somebody skipped physics...
  }

  // How about this?
  // `travelTime(startPointInMeters, endPointInFeet, speedInMetersPerSecond)`
}


object DependentTypesDSL extends App {

  case class Numeric[A](in: Double){
    def -(other: Numeric[A]): Numeric[A] = (in - other.in).withUnits[A]
    def /[B](other: Numeric[B])(implicit ev: A CanDivideBy B): Numeric[ev.Out] = {
      ev.divide(this, other)
    }
  }

  type MetersUnit
  type SecondsUnit
  type MetersPerSecondUnit
  type Meters = Numeric[MetersUnit]
  type Seconds = Numeric[SecondsUnit]
  type MetersPerSecond = Numeric[MetersPerSecondUnit]
  def attach[T](in: Double): Numeric[T] = Numeric[T](in)
  implicit class RichDouble(in: Double){
    def withUnits[T]: Numeric[T] = Numeric[T](in)
    def meters: Meters = Numeric[MetersUnit](in)
    def seconds: Seconds = Numeric[SecondsUnit](in)
    def metersPerSecond: MetersPerSecond = Numeric[MetersPerSecondUnit](in)
  }

  trait CanDivideBy[Num, Denom] {
    type Out
    def divide(num: Numeric[Num], denom: Numeric[Denom]): Numeric[Out] = (num.in / denom.in).withUnits[Out]
  }
  type CanDivideByAux[Num, Denom, Result] = CanDivideBy[Num, Denom]{ type Out = Result }

  // just some boilerplate so we can have nice syntax...
  def dividing[A]: Builder1[A] = new Builder1
  class Builder1[A]{ def by[B]: Builder2[A, B] = new Builder2() }
  class Builder2[A, B]{
    def yields[C]: CanDivideByAux[A, B, C] = new CanDivideBy[A, B]{ type Out = C }
  }

  // now defining a new rule for our algebra is super easy!
  implicit val rule1 = dividing [MetersUnit] .by [MetersPerSecondUnit] .yields [SecondsUnit]

  // Now, the application code!
  def travelTime(startPoint: Meters, endPoint: Meters, speed: MetersPerSecond): Seconds = {
    val distance: Meters = endPoint - startPoint
    val time: Seconds = distance / speed // wow! neat!
    time
  }

  println(travelTime(20.meters, endPoint = 120.meters, speed = 5.metersPerSecond))

}
