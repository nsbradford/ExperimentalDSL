package functionalprogramming

import scala.util.{Failure, Success, Try}


/**
  * TYPE-DRIVEN DEVELOPMENT
  *
  * DISCUSS: What is type-driven development? (The other TDD, heheh)
  */

/**
  * Let's work through an example scenario!
  */
object TDD_Motivation {
  // TODO: first day on your new job! You get ticket: "implement this function. return error on failure"
  def updateHeight(session: Int, height: Int): Unit = ???

  // other functions you find around the codebase...
  object LibraryFunctions {
    // these look useful...
    def getUser(session: Int): Int = ???
    def updateUser(user: Int, height: Int): Unit = ???

    // wait a sec... do I need these?
    def validateHeight(height: Int): Int = ???
    def convertToCm(inches: Int): Int = ???
  }
}

object TDD_CorrectSolution extends App {
  import TDD_Motivation.LibraryFunctions._
  def updateHeight(session: Int, height: Int): Unit = {
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
//  def updateHeight(session: SessionId, height: Inches): Try[Unit] = ???
  def validateAndUpdate(session: SessionId, height: Inches): Try[Unit] = {
    val heightInCm: Centimeters = convertToCm(height)
    val userId: Try[UserId] = getUser(session)
    val validatedHeight: Try[ValidatedHeight] = validateHeight(heightInCm)
//    val update: Try[Unit] = updateUser(userId, validatedHeight)

//      val update: Try[Unit] = userId.flatMap{ id =>
//        validatedHeight.flatMap(height =>
//          updateUser(id, height)
//        )
//      }

//
//    // If the DB read succeeds, try to write. If it fails, pass along the error to the caller.
    for { // IT'S THE RETURN OF THE MONAD!!!
      userId: UserId <- getUser(session)
      validatedHeight: ValidatedHeight <- validateHeight(heightInCm)
      update: Unit <- updateUser(userId, validatedHeight)
    } yield update
  }

  val x: Option[Int] = Some(1)
  val y: Option[Int] = Some(1)
  val z: Option[Int] = Some(1)

  val q: Option[Int] = for { // = Some(3), not None
    xx: Int <- x
    yy: Int <- y
    zz: Int <- z
  } yield xx + yy + zz

}

/**
  * DISCUSS: do we think the type-driven solution is more readable/approachable + less error-prone?
  *   Did the function feel like it "writes itself"? That's what we mean by Type-Driven Development :D
  */

/**
  * DISCUSS: could we go even farther with our type safety and FP design? How about side effects?
  */