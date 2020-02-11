package calcdsl

import java.util.concurrent.ConcurrentHashMap

import org.scalatest.{FlatSpec, Matchers, WordSpec}

import scala.util.Try
import Syntax._
import cats.Traverse
import org.joda.time.DateTime
import cats.syntax.applicative._

import scala.annotation.tailrec


/**
  * Created by nicholasbradford on 10/25/19.
  */
class CalcTest extends WordSpec with Matchers {

  val asOfDate = DateTime.now()
  val address = RepositoryName("address")

  "Calc" when {
    "diamond-shaped dependency graph" should {
      "only evaluate root once, cache it, and not evaluate again" in {
        val manager = new TestVersionManager(asOfDate)
        val db = new MockDatabase[Int](address)
        val a = Calc(1).output(db)
        val b = a.map(_ + 1)
        val c = a.map(_ + 1)
        val d = (b, c).mapN(_ + _)
        d.runSyncWithDiags(manager)
        db.dbTable should have size 1
        manager.getAllRunsOf(address) should have size 1
      }
    }

    "expressing" should {
      "work in for-comprehensions" in {
        val z =
          for {
            x <- Calc(1)
            y <- Calc(2)
          } yield x + y
        z.runSync() shouldBe 3
      }
      "work with applicative mapN" in {
        val x = Calc(1)
        val y = Calc(2)
        val z = (x, y).mapN(_ + _)
        z.runSync() shouldBe 3
      }
      "work with traverse" in {
        import cats.instances.list._
        val xs: List[Calc[Int]] = List(Calc(1), Calc(2), Calc(3))
        val result: Calc[List[Int]] = Traverse[List].sequence(xs)
        result.runSync() shouldBe List(1, 2, 3)
      }
    }

    "running standard sync" should {
      implicit val db = new MockDatabase[Int](address)

      "execute lazily" in {
        val _ = Calc(1).map(_ + 1).output
        db.dbTable shouldBe empty
      }

      "not be stack safe due to lack of trampoline" in {
        @tailrec
        def buildNested(acc: Calc[Int], i: Int): Calc[Int] = i match {
          case 0 => acc
          case _ => buildNested(acc.map(_ + 1), i - 1)
        }

        val calc: Calc[Int] = buildNested(1.calc, 1000)
        a[StackOverflowError] shouldBe thrownBy { calc.runSync() }
      }
    }
  }



//  private val stdTimeoutMs: Long = 1000
//
//  private def delayed(x: Int, timeout: Long = stdTimeoutMs): Int = {
//    Thread.sleep(timeout)
//    x
//  }
//
//  private def stopwatch[A](block: => A): (A, Long) = {
//    val startTime: Long = System.currentTimeMillis()
//    val result = block
//    val elapsed = System.currentTimeMillis() - startTime
//    (result, elapsed)
//  }

  // TODO testing with IO is annoying, might need to convert to Future and use ScalaTest .await matcher
  // https://stackoverflow.com/questions/27750244/how-to-test-methods-that-return-future
//
//  "" when {
//    val db = new MockDatabase[Int]
//    "run in parallel" should {
//      "execute parallel branches concurrently" in {
//        (Calc(delayed(1)), Calc(delayed(1))).mapN(_ + _)
//      }
//
//    }
//  }


}
