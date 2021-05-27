package functionalprogramming


/**
  * INTRO TO TYPE-DRIVEN DEVELOPMENT AND FUNCTIONAL PROGRAMMING
  *
  * Goal for this workshop: an easy, fun, practical intro to a set of very high-level tools,
  *   which may broaden how you think about programming.
  *
  * THESIS:
  *   Monads and other FP concepts are useful (and ubiquitous) abstractions to recognize;
  *     They're not "necessary" to understand, but they can give greater depth/appreciation for code you're writing.
  *     I think of it as learning Chord Progressions for music, Archetypes/Hero's Journey for storytelling, etc.
  */

/**
  * Details:
  *
  * Intro
  *   - OO vs FP design: Data vs Functional extensibility
  *   - Type-driven development walkthrough
  *
  * Fancy type stuff:
  *   - Ad-hoc polymorphism with Type Classes
  *   - Dependent types
  *
  * Useful type classes (high-level concepts):
  *   - Functors for static, linear operations
  *   - Applicatives for static, parallel operations
  *   - Monads for dynamic, dependent computations (yes, we're going to explain Monads and it'll be super easy!)
  *
  * Objectives to keep in mind:
  *   - I want the compiler to do AS MUCH WORK AS POSSIBLE
  *   - I want to elegantly express VERY HIGH LEVEL common code patterns
  * Turns out, these are very complementary goals.
  */


// ================================================================================
// OO vs FP design

/**
  * DISCUSS:
  *   -What is Functional Programming?
  *   -How is it different from Object-Oriented Design? or is it complementary?
  *   -What tradeoffs are involved?
  */
/**
  * At the highest level: what does Functional Programming offer?
  *
  * REFERENTIAL TRANSPARENCY: expressions can always be substituted for values (no side effects; "pure"),
  *   any necessary side effects are "pushed to the edge of the program"
  *
  * BENEFIT: you can more easily reason about and "compose" large programs from small building blocks.
  *
  * Second-order effect of architecture choice:
  *   Object-Oriented architectures => combine Data with Functions; easy to add DATA TYPES
  *   Functional architectures      => separate Data and Functions; easy to add FUNCTIONALITY
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
