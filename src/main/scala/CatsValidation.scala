package com.breezzo

import cats.data.{ NonEmptyList, Validated, ValidatedNel }
import cats.{ Applicative, Semigroup, SemigroupK }

import scala.util.Try

object CatsValidation extends App {

  sealed trait ConfigError
  final case class ParseError(field: String)  extends ConfigError
  final case class MissingField(name: String) extends ConfigError

  implicit val nelSemigroup: Semigroup[NonEmptyList[ConfigError]] = SemigroupK[NonEmptyList].algebra[ConfigError]

  trait Read[A] {
    def read(value: String): Option[A]
  }
  implicit val stringRead: Read[String] = (value: String) => Some(value)
  implicit val intRead: Read[Int]       = (value: String) => Try(value.toInt).toOption

  type ValType[A] = ValidatedNel[ConfigError, A]

  class Config(values: Map[String, String]) {
    def read[A: Read](field: String): ValType[A] = {
      values.get(field) match {
        case Some(value) =>
          implicitly[Read[A]].read(value) match {
            case Some(x) => Validated.validNel[ConfigError, A](x)
            case None    => Validated.invalidNel(ParseError(field))
          }
        case None        => Validated.invalidNel(MissingField(field))
      }
    }
  }

  final case class ConnectionParams(url: String, port: Int, username: String)

  val config = new Config(
    Map(
      "url"      -> "127.0.0.1",
      "port"     -> "3434",
      "username" -> "user"
    )
  )

  val connectionParams = Applicative[ValType]
    .map3(
      config.read[String]("url"),
      config.read[Int]("port"),
      config.read[String]("username")
    )(ConnectionParams.apply)

  println(connectionParams)
}
