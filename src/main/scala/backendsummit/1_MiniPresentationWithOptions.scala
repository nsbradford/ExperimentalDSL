package backendsummit


/**
  * MINI-INTRO TO TYPE-DRIVEN DEVELOPMENT AND FUNCTIONAL PROGRAMMING
  *
  * Goal for this workshop: an easy, fun, practical intro
  *   to a set of very high-level abstractions
  *   through a fairly simple and narrow example,
  *   which may broaden how you think about programming
  *   with types :)
  *
  * We'll use Scala instead of Python, because of some nice syntax (and advanced features).
  */
object MiniPresentationWithOptions {

  /** Let's take a common example from Python - dealing with Optional!
    * In Scala, it's a class called `Option`, which can either be `Some(value)` or `None`.
    *
    * First, a super-abbreviated tour of how to use Option (Python-style):
    */
  val someOption: Option[Int] = Some(1)
  val emptyOption: Option[Int] = None

  val isEmptyOptionDefined: Boolean = someOption.isDefined   // True
  val isSomeOptionDefined: Boolean = emptyOption.isDefined   // False
  val isSomeOptionEqualToNone: Boolean = emptyOption == None // True

  val getSomeOption: Int = someOption.get                    // 1
  val getEmptyOption: Int = emptyOption.get                  // throws Error!
  val getEmptyOptionOrElse: Int = emptyOption.getOrElse(0)   // 0

  /** What are some typical patterns for dealing with Optional in Python?
    * We often check to see if it's `None`, and then do something with the value if it exists:
    */
  def convertOptionIntToStr(myOption: Option[Int]): Option[(Int, Int)] = {
    var answer: Option[(Int, Int)] = None
    if (myOption != None)
      answer = Some((myOption.get, myOption.get)) // safe to use `get`
    return answer
  }

  /** Hm, this is annoying to write over and over again.
    *   And what if we accidentally call `get()` somewhere unsafe?
    *
    * Can we "lift" this pattern into types and express with a function,
    *   such that it's **impossible to write something that fails at runtime?**   <-- the holy grail of FP/TDD
    *
    * DISCUSS: any ideas?
    */
}

/**
  * HINT 1: What if we think of an Option[A] as just a List[A], but with a max of one element?
  */

/**
  * HINT 2: What function in Python exists for going from List[A] => List[B]?
  */