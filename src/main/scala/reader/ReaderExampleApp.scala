package com.breezzo
package reader

import cats.data.Reader
import cats.implicits.catsSyntaxApplicativeId


object ReaderExampleApp extends App {

  final case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = {
    Reader[Db, Option[String]] { db =>
      db.usernames.get(userId)
    }
  }

  def checkPassword(username: String, password: String): DbReader[Boolean] = {
    Reader[Db, Boolean] { db =>
      db.passwords.get(username).contains(password)
    }
  }

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      usernameOpt       <- findUsername(userId)
      isPasswordCorrect <- usernameOpt
                             .map(username => checkPassword(username, password))
                             .getOrElse(false.pure[DbReader])
    } yield isPasswordCorrect
  }

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(
    checkLogin(1, "zerocool").run(db)
  )
  println(
    checkLogin(4, "davinci").run(db)
  )
}
