package archive.programmingexercises




object Quicksort {
  def quicksort[A <: Ordered[A]](xs: Seq[A]): Seq[A] = xs match {
    case Seq() => Seq()
    case head +: tail =>
      val (smallerThan, greaterThan) = tail.partition(_ <= head)
      quicksort(smallerThan) ++ Seq(head) ++ quicksort(greaterThan)
  }
}


object Palindrome {
  // Start writing your ScalaFiddle code here

  def canBePal(s: String): Boolean = {
    val nOdds = s.toSeq
      .groupBy(identity)
      .values
      .map(_.length)
      .count(_ % 2 != 0)
    nOdds <= 1
  }

  canBePal("asdfasd") // true
  canBePal("aaasssdd") // false

}