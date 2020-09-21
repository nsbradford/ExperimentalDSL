package axoni

import org.scalatest.{Matchers, WordSpec}
import org.scalatest.wordspec.AnyWordSpec
import Trie._

import scala.collection.mutable


/**
  * Created by nicholasbradford on 9/21/20.
  */
class TrieTest extends WordSpec with Matchers {


  "Trie" when {
    "toBinaryLeastSigBitFirst" should {
      "convert properly" in {
        4.toBinaryString shouldBe "100"
        2.toBinaryString shouldBe "10"
        toBinaryLeastSigBitFirst(4) shouldBe Array(0, 0, 1)
        toBinaryLeastSigBitFirst(2) shouldBe Array(0, 1)
      }
    }

    "get" should {
      "return the empty string when prefix not in tree" in {
        val empty = new TrieNode()
        empty.get(4) shouldBe ""
      }
      "return the empty string when prefix is not a terminated node" in {
        // assemble manually so doesn't have to test insert function
        val expected = "answer"
        val has4 = new TrieNode(children = mutable.Map(
          0 -> new TrieNode(children = mutable.Map(
            0 -> new TrieNode(children = mutable.Map(
              1 -> new TrieNode(stored = Some(expected))
            ))
          ))
        ))

        val result = has4.get(4)
        result shouldBe expected
      }
    }
  }
}
