package calc2InterMonad

import org.joda.time.DateTime

import scala.language.higherKinds
import scala.util.Try



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


case class CalcRun(repositoryName: RepositoryName, version: Version, asOf: DateTime)
case class InputRecord(version: Version, inputs: Set[Version])
case class RepositoryName(id: String)

trait VersionedRepository[A] {

  def name: RepositoryName

  // syntactic sugar for new API

  //  def apply: ReadCalc[A] = latest
  //  def latest: ReadCalc[A]

  // old API, force execution
  //  def readLatest: Versioned[A]
  def read(version: Version): Try[Versioned[A]]
  def write(fa: Versioned[A]): Try[Unit]
}


trait CalcInterpreter{
  type Result[A]
//  implicit val resultIsMonad: cats.Monad[Result] // TODO

  def execute[A](fa: PureCalc[A]): Result[A]
  //  def execute[A, B](fa: Calc.MergeCalc[A, B]): Result[(A, B)]
  def execute[A, B](fa: FlatMapCalc[A, B]): Result[B]
  //  def execute[A](fa: ReadCalc[A]): Result[A]
  def execute[A](fa: OutputCalc[A]): Result[A]
}

object CalcInterpreter {
  type Aux[R[_]] = CalcInterpreter{ type Result[A] = R[A] }
}



/**
  * Created by nicholasbradford on 1/12/20.
  */
trait VersionManager {

  def asOfDate: DateTime

  // all of these should use Future or other

  // write
  def requisitionNewVersion(repositoryName: RepositoryName): Version
  def logInputs(version: Version, inputs: Set[Version])


  // read
  def getLatest(repositoryName: RepositoryName): Option[CalcRun]
  def getInputs(version: Version): Set[Version]
  def getAllRunsOf(repositoryName: RepositoryName): Set[CalcRun]

}
