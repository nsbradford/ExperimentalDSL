package calcdsl

import scala.util.Try


case class RepositoryName(id: String)

trait VersionedRepository[A] {

  def name: RepositoryName

  // TODO syntactic sugar for new API
//  def apply: ReadCalc[A] = latest
//  def latest: ReadCalc[A]

  // old API, force execution
//  def readLatest: Versioned[A]
  def read(version: Version): Try[Versioned[A]]
  def write(fa: Versioned[A]): Try[Unit]
}
