package axoni


/**
  * Created by nicholasbradford on 9/21/20.
  */
object Trie {

  type uint = Int // Java/Scala don't have unsigned

  def toBinaryLeastSigBitFirst(x: Int): Seq[Int] = {
    x.toBinaryString
      .reverse
      .split("")
      .map(_.toInt)
  }

}

import Trie._

import scala.annotation.tailrec
import scala.collection.mutable


/**
  * Making this class mutable to meet problem specs easily.
  *
  * Purposefully making non-generic for simplicity;
  *   Could alternatively have default behavior or typeclasses for converting various types to array-like types.
  *
  */
class TrieNode(val children: mutable.Map[Int, TrieNode] = mutable.Map(),
               var stored: Option[String] = None)
{
  // more generic than hardcoding {0,1} as children options (for future expansion of this class), at price of compile-time safety
  require(children.keySet.forall(key => Set(0,1) contains key))

  override def toString: String = s"TrieNode(stored=$stored, children={${children.mkString(",")}})"

  override def equals(obj: scala.Any): Boolean = obj match {
    case t : TrieNode => t.stored == stored && t.children == children
    case _ => false
  }

  /** Decodes integer into binary representation (least sig bit first),
    * and then stores into the Trie. */
  def insert(key: uint, value: String): Unit = insertHelper(toBinaryLeastSigBitFirst(key), value)

  @tailrec
  private def insertHelper(key: Seq[Int], value: String): Unit = key match {
    case Seq() =>
      stored = Some(value)
      ()
    case head +: tail =>
      val nextNode: TrieNode = children.getOrElse(head, {
        val next = new TrieNode()
        this.children += ((head, next))
        next
      })
      nextNode.insertHelper(tail, value)
  }


  /** Returns the value for given key, or the empty string if the key is not found. */
  def get(key: uint): String = getHelper(toBinaryLeastSigBitFirst(key))

  @tailrec
  private def getHelper(key: Seq[Int]): String = key match {
    case Seq() =>
      println(s"Reached end of search, returning stored value $stored")
      stored.getOrElse("")
    case head +: tail =>
      println(s"Found $head +: $tail, searching children $children")
      children.get(head) match {
        case Some(trieNode) =>
          println(s"\tFound child with $head, continuing search with $tail...")
          trieNode.getHelper(tail)
        case None =>
          println(s"\tFound no children with $head, abandoning search.")
          ""
      }
  }
}