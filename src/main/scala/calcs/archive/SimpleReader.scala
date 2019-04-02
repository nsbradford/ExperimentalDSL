package calcs

import cats._, cats.data._, cats.implicits._
import cats._
import cats.data._
import cats.implicits._
import cats.data.Reader

//case class User(id: Long, parentId: Long, name: String, email: String)
//trait UserRepo {
//  def get(id: Long): User
//  def find(name: String): User
//}
//
//trait Users {
//  def getUser(id: Long): UserRepo => User = {
//    case repo => repo.get(id)
//  }
//  def findUser(name: String): UserRepo => User = {
//    case repo => repo.find(name)
//  }
//}
//
//object UserInfo extends Users {
//  def userInfo(name: String): UserRepo => Map[String, String] =
//    for {
//      user <- findUser(name)
//      boss <- getUser(user.parentId)
//    } yield Map(
//      "name" -> s"${user.name}",
//      "email" -> s"${user.email}",
//      "boss_name" -> s"${boss.name}"
//    )
//}
//
//trait Program {
//  def app: UserRepo => String =
//    for {
//      fredo <- UserInfo.userInfo("Fredo")
//    } yield fredo.toString
//}
//
//
//object Main extends Program {
//
//  val testUsers = List(User(0, 0, "Vito", "vito@example.com"),
//    User(1, 0, "Michael", "michael@example.com"),
//    User(2, 0, "Fredo", "fredo@example.com"))
//
//  def run: String = app(mkUserRepo)
//  def mkUserRepo: UserRepo = new UserRepo {
//    def get(id: Long): User = (testUsers find { _.id === id }).get
//    def find(name: String): User = (testUsers find { _.name === name }).get
//  }
//}