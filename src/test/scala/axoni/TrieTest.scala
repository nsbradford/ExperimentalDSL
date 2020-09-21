package axoni

import org.scalatest.{Matchers, WordSpec}
import org.scalatest.wordspec.AnyWordSpec
import Trie._

import scala.collection.mutable


/**
  * Created by nicholasbradford on 9/21/20.
  */
class TrieTest extends WordSpec with Matchers {

  def buildTreeStoring4(expected: String): TrieNode =
    new TrieNode(children = mutable.Map(
      0 -> new TrieNode(children = mutable.Map(
        0 -> new TrieNode(children = mutable.Map(
          1 -> new TrieNode(stored = Some(expected))
        ))
      ))
    ))


  "Trie" when {
    "custom equality" should {
      "work for equal trees" in {
        buildTreeStoring4("answer") shouldBe buildTreeStoring4("answer")
      }
      "work for unequal trees" in {
        buildTreeStoring4("asdfasdf") should not equal buildTreeStoring4("xxxxxxx")
      }
    }

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
        val trie = buildTreeStoring4(expected)
        val result = trie.get(4)
        result shouldBe expected
      }
    }

    "insert" should {
      "work with key of length 1" in {

      }

      "work on an empty tree" in {
        val trie = new TrieNode()
        val expectedStr = "answer"
        val expected = buildTreeStoring4(expectedStr)
        trie.insert(4, expectedStr)
        trie shouldBe expected
      }

      "work on a partially similar tree" in {
        val expectedStr4 = "four"
        val expectedStr2 = "two"
        val result = buildTreeStoring4(expectedStr4)
        result.insert(2, expectedStr2)

        val expected = new TrieNode(children = mutable.Map(
          0 -> new TrieNode(children = mutable.Map(
            1 -> new TrieNode(stored = Some(expectedStr2)),
            0 -> new TrieNode(children = mutable.Map(
              1 -> new TrieNode(stored = Some(expectedStr4))
            ))
          ))
        ))

        result shouldBe expected
      }

      "replace an existing value" in {

      }
    }
  }
}
