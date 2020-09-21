package axoni


import scala.annotation.tailrec
import scala.collection.mutable

import java.security.MessageDigest


/**
  * Created by nicholasbradford on 9/21/20.
  */
object Trie {

  type uint = Int // Java/Scala don't have unsigned

  // placing here just for easier reference in tests
  private lazy val digester: MessageDigest = MessageDigest.getInstance("SHA-256")
  lazy val nullMerkle: Array[Byte] = hashStr("")

  def hashStr(s: String): Array[Byte] = digester.digest(s.getBytes("UTF-8"))
  def hashBytes(xs: Array[Byte]): Array[Byte] = digester.digest(xs)

  def toBinaryLeastSigBitFirst(x: Int): Seq[Int] = {
    x.toBinaryString
      .reverse
      .split("")
      .map(_.toInt)
  }

}


import Trie._



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
  val storedStr: String = stored.getOrElse("")

  // more generic than hardcoding {0,1} as children options (for future expansion of this class), at price of compile-time safety
  require(children.keySet.forall(key => Set(0,1) contains key))

  override def toString: String = s"TrieNode(stored=$stored, children={${children.mkString(",")}})"

  override def equals(obj: scala.Any): Boolean = obj match {
    case t : TrieNode => t.stored == stored && t.children == children
    case _ => false
  }

//  /** Decodes integer into binary representation (least sig bit first),
//    * and then stores into the Trie. */
//  def insert(key: uint, value: String): Unit = insertHelper(toBinaryLeastSigBitFirst(key), value)
//
//  @tailrec
//  private def insertHelper(key: Seq[Int], value: String): Unit = key match {
//    case Seq() =>
//      stored = Some(value)
//      ()
//    case head +: tail =>
//      val nextNode: TrieNode = children.getOrElse(head, {
//        val next = new TrieNode()
//        this.children += ((head, next))
//        next
//      })
//      nextNode.insertHelper(tail, value)
//  }


  /** Returns the value for given key, or the empty string if the key is not found. */
  def get(key: uint): String = getHelper(toBinaryLeastSigBitFirst(key))

  @tailrec
  private def getHelper(key: Seq[Int]): String = key match {
    case Seq() =>
      println(s"Reached end of search, returning stored value $stored")
      storedStr
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



  //====================================================================================================================
  // Task 2

//  def MerkleRoot: Array[Byte] = {
//    // handles the case of a null root
//    if (stored.isEmpty && children.isEmpty) {
//      Trie.nullMerkle
//    } else {
//      val selfMerkle: Array[Byte] = Trie.hashStr(storedStr)
//      val zeroMerkle: Array[Byte] = children.get(0).map(_.MerkleRoot).getOrElse(Trie.nullMerkle)
//      val oneMerkle: Array[Byte] = children.get(1).map(_.MerkleRoot).getOrElse(Trie.nullMerkle)
//      val concatenated = selfMerkle ++ zeroMerkle ++ oneMerkle
//      Trie.hashBytes(concatenated)
//    }
//  }



  //====================================================================================================================
  // Task 3

  // Method: when calling MerkleRoot(), nodes cache their MerkleRoot.
  // This is reset by insert() at all nodes along the path of insertion.
  private var cachedMerkle: Option[Array[Byte]] = None

  def MerkleRoot: Array[Byte] = cachedMerkle.getOrElse{
    val answer: Array[Byte] =
      if (stored.isEmpty && children.isEmpty) {
        Trie.nullMerkle // handles the case of a null root
      } else {
        val selfMerkle: Array[Byte] = Trie.hashStr(storedStr)
        val zeroMerkle: Array[Byte] = children.get(0).map(_.MerkleRoot).getOrElse(Trie.nullMerkle)
        val oneMerkle: Array[Byte] = children.get(1).map(_.MerkleRoot).getOrElse(Trie.nullMerkle)
        val concatenated = selfMerkle ++ zeroMerkle ++ oneMerkle
        Trie.hashBytes(concatenated)
      }
    cachedMerkle = Some(answer)
    answer
  }

  /** Decodes integer into binary representation (least sig bit first),
    * and then stores into the Trie. */
  def insert(key: uint, value: String): Unit = insertHelper(toBinaryLeastSigBitFirst(key), value)

  @tailrec
  private def insertHelper(key: Seq[Int], value: String): Unit = {

    cachedMerkle = None // covers all the cases

    key match {
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
  }

}