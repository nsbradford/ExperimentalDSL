package calcdsl

import org.joda.time.DateTime

/**
  * Created by nicholasbradford on 2/9/20.
  */
class Syntax {
  implicit class RichAny[A](x: A){
    def calc: Calc[A] = Calc(x)
  }

  implicit class RichSeq[A](xs: Seq[A]){
    def maxOpt(implicit ev: Ordering[A]): Option[A] = xs.maxByOpt(identity)
    def maxByOpt[B : Ordering](f: A => B): Option[A] = xs match {
      case Seq() => None
      case xss => Some(xss.maxBy(f))
    }
  }

  implicit val dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)
}

object Syntax extends Syntax
  with cats.syntax.ApplySyntax
