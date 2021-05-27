package functionalprogramming

import scala.util.{Failure, Success, Try}


// ================================================================================
// Type-Driven Development: philosophy

/**
  * DISCUSS: What is type-driven development? (The other TDD, heheh)
  */

/**
  * Let's work through an example scenario!
  */
object TDD_Motivation {
  // TODO: first day on your new job! You get ticket: "implement this function. return error on failure"
  def updateHeight(session: Int, height: Int): Unit = {
    import LibraryFunctions._
    val user = getUser(session)
    updateUser(user, height)
  }
  object LibraryFunctions {
    // these look useful...
    def getUser(session: Int): Int = ???
    def updateUser(user: Int, height: Int): Unit = ??? // Scala "Unit" is equivalent to Java "void" / Python "None"

    // wait a sec... do I need these?
    def validateHeight(height: Int): Int = ???
    def convertToCm(inches: Int): Int = ???
  }
}

object TDD_CorrectSolution {
  import TDD_Motivation.LibraryFunctions._
  def validateAndUpdate(session: Int, height: Int): Unit = {
    val heightInCm: Int = convertToCm(height)
    val validatedHeight: Int = validateHeight(heightInCm) // does this have side-effects?
    val user: Int = getUser(session)
    updateUser(user, validatedHeight) // also hold on... how are errors handled?
  }
}

/**
  * DISCUSS: how can we improve this?
  *   Also: What Computational Context might be useful here?
  */
object TDD_TypeDrivenSolution {
  case class SessionId(id: Int)
  case class UserId(id: Int)
  case class Inches(value: Int)
  case class Centimeters(value: Int)
  case class ValidatedHeight(cm: Centimeters)

  // Try can return either Success(value) or Failure(exception)
  def getUser(session: SessionId): Try[UserId] = ???                              // not sure if DB read will work
  def updateUser(user: UserId, validatedHeight: ValidatedHeight): Try[Unit] = ??? // not sure if DB write will work
  def validateHeight(height: Centimeters): Try[ValidatedHeight] = ???             // returns Failure for bad values
  def convertToCm(inches: Inches): Centimeters = ???

  // TODO let's try to implement together!
  def validateAndUpdate(session: SessionId, height: Inches): Try[Unit] = ???
//  def validateAndUpdate(session: SessionId, height: Inches): Try[Unit] = {
//    val heightInCm: Centimeters = convertToCm(height)
//
//    // If the DB read succeeds, try to write. If it fails, pass along the error to the caller.
//    for { // IT'S THE RETURN OF THE MONAD!!!
//      userId: UserId <- getUser(session)
//      validatedHeight: ValidatedHeight <- validateHeight(heightInCm)
//      update: Unit <- updateUser(userId, validatedHeight)
//    } yield update
//  }
}

/**
  * DISCUSS: do we think the type-driven solution is more readable/approachable + less error-prone?
  *   Did the function feel like it "writes itself"? That's what we mean by Type-Driven Development :D
  */