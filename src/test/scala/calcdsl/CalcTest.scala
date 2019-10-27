package calcdsl

import java.util.concurrent.ConcurrentHashMap

import org.scalatest.{FlatSpec, Matchers, WordSpec}

import scala.util.Try

import CalcSyntax._


/**
  * Created by nicholasbradford on 10/25/19.
  */
class CalcTest extends WordSpec with Matchers {

  private val stdTimeoutMs: Long = 1000

  private def delayed(x: Int, timeout: Long = stdTimeoutMs): Int = {
    Thread.sleep(timeout)
    x
  }

  private def stopwatch[A](block: => A): (A, Long) = {
    val startTime: Long = System.currentTimeMillis()
    val result = block
    val elapsed = System.currentTimeMillis() - startTime
    (result, elapsed)
  }

  // TODO testing with IO is annoying, might need to convert to Future and use ScalaTest .await matcher
  // https://stackoverflow.com/questions/27750244/how-to-test-methods-that-return-future

  "" when {
    val db = new MockDatabase[Int]
    "run in parallel" should {
      "execute parallel branches concurrently" in {
        (Calc(delayed(1)), Calc(delayed(1))).mapN(_ + _)
      }

    }
  }

}

class MockDatabase[A](timeoutMs: Long = 0) extends VersionedRepository[A]{

  var dbTable: ConcurrentHashMap[Version, A] = new ConcurrentHashMap()

  override def read(version: Version): Try[Versioned[A]] = Try{
    Thread.sleep(timeoutMs)
    Versioned(dbTable.get(version), version)
  }

  override def write(fa: Versioned[A]): Try[Unit] = Try{
    Thread.sleep(timeoutMs)
    dbTable.put(fa.version, fa.get)
  }

}