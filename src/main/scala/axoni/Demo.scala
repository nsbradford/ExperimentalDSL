package axoni


sealed trait JSON
final case class JsonString(in: String) extends JSON
final case class JsonInteger(in: Int) extends JSON
final case class JsonObject(in: Map[JsonString, JSON]) extends JSON

sealed trait BinaryTree[+T]
object Empty extends BinaryTree[Nothing]
final case class Node[+T](data: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T]

trait Jsonable[T]{
  def toJson(t: T): JSON
  def fromJson(json: JSON): T
}

object JsonableOps {

  implicit class JsonableSyntax[T: Jsonable](t: T){
    def toJson: JSON = implicitly[Jsonable[T]].toJson(t)
  }
  implicit class JsonableReverseSyntax[T: Jsonable](json: JSON){
    def fromJson: T = implicitly[Jsonable[T]].fromJson(json)
  }


  implicit val IntIsJsonable: Jsonable[Int] = new Jsonable[Int] {
    override def toJson(t: Int): JSON = JsonInteger(t)
    override def fromJson(json: JSON): Int = json match {
      case JsonInteger(in) => in
      case x => throw new IllegalArgumentException(s"$x is not valid")
    }
  }

  implicit def BinaryTreeIsJsonable[T : Jsonable]: Jsonable[BinaryTree[T]] = new Jsonable[BinaryTree[T]] {
    override def toJson(t: BinaryTree[T]) = t match {
      case Empty => JsonObject(Map())
      case Node(data, left, right) => JsonObject(Map(
        JsonString("data") -> implicitly[Jsonable[T]].toJson(data),
        JsonString("left") -> toJson(left),
        JsonString("right") -> toJson(right)
      ))
    }

    override def fromJson(json: JSON): BinaryTree[T] = json match {
      case JsonObject(x) if x.isEmpty => Empty
      case JsonObject(in) =>
        val data: JSON = in(JsonString("data"))
        val left: JSON = in(JsonString("left"))
        val right: JSON = in(JsonString("right"))
        Node(implicitly[Jsonable[T]].fromJson(data), fromJson(left), fromJson(right))
      case x => throw new IllegalArgumentException(s"$x is not valid")
    }
  }

}