package functionalprogramming

/**
  * TYPE CLASSES
  *
  * Let's extend the idea of separating objects from functionality...
  */

/**
  * What if we want to declare the following interface,
  *   and we want it to work for not only Dogs, but Strings too?
  */
object TypeclassMotivation{
  type CustomBinaryFormat = String // imagine this is a fancy class
  trait CustomDbWritable { // trait == interface
    def customBinary(): CustomBinaryFormat
  }
  def writeToDB(bin: CustomBinaryFormat): Unit = () // imagine this is also fancy
  def serializeAndWriteToDb(writable: CustomDbWritable): Unit = {
    writeToDB(writable.customBinary())
  }
}

/**
  * Turns out this has a name: Ad-hoc Polymorphism, which you implement with a "Type class"!
  *   A type class basically just separates the interface from the class definition!
  *   Don't worry if this seems weird at first, the syntax to implement is annoying, the idea/end result is the important bit
  *
  * The types of polymorphism we're previously familiar with are Subtyping (inheritance) and Parametric (using Generics).
  *   See: https://en.wikipedia.org/wiki/Polymorphism_(computer_science)
  */
object Animals_Typeclass extends App {
  type CustomBinaryFormat = String // imagine this is a fancy class

  // first a new trait - notice the type parameter; this says to us and the compiler "T Is CustomDbWritable"
  trait IsCustomDbWritable[T] {
    def customBinary(t: T): CustomBinaryFormat
  }

  // Now we need the "type class instances" to provide "evidence" for particular types.
  // `implicit` is a fancy keyword that puts things into an additional "implicit" scope,
  //    which allows the compiler to try to automatically resolve.
  //    This can create some pretty wild (though completely safe!) functionality, as we'll see soon...
  import Animals_FunctionalProgramming._
  implicit object Dog_IsCustomDbWritable extends IsCustomDbWritable[Dog]{
    override def customBinary(t: Dog): CustomBinaryFormat = "CustomBinaryDoggo!"
  }
  implicit object String_IsCustomDbWritable extends IsCustomDbWritable[String]{
    override def customBinary(t: String): CustomBinaryFormat = s"CustomBinaryString $t"
  }

  // finally, some boilerplate for syntactic sugar to look like it's a native method (called an "extension method")
  implicit class RichWritable[T](in: T)(implicit writable: IsCustomDbWritable[T]){
    def customBinary: CustomBinaryFormat = writable.customBinary(in)
  }

  println("hello world!".customBinary)
  println(Dog().customBinary)
//  println(1.customBinary) // doesn't work!
}

object Animals_Typeclass_App extends App {
  import Animals_FunctionalProgramming._
  import functionalprogramming.Animals_Typeclass._

  def writeToDB(bin: CustomBinaryFormat): Unit = () // not important
  def serializeAndWriteToDb[T : IsCustomDbWritable](writable: T): CustomBinaryFormat = {
    val bin = writable.customBinary
    writeToDB(bin)
    bin // in scala, there's no explicit 'return'; it's always just the last statement in a block.
  }

  // A note on implicits: the following two signatures are identical.
//  def serializeAndWriteToDb2[T : IsCustomDbWritable](writable: T): CustomBinaryFormat = ???
//  def serializeAndWriteToDb3[T](writable: T)(implicit ev: IsCustomDbWritable[T]): CustomBinaryFormat = ???

  println(serializeAndWriteToDb("Hello world!"))
  println(serializeAndWriteToDb(Dog()))
  //  println(serializeAndWriteToDb(1)) // doesn't work, there's no type class evidence!
}
