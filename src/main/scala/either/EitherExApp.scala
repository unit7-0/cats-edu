package com.breezzo
package either

import cats.syntax.either._

object EitherExApp extends App {
  val errorOrInt = 0.asRight[String]
  println(errorOrInt.map(_ + 1))

  val errorOrParsedInt = Either.catchNonFatal("10!".toInt).leftMap(_.getMessage)
  println(errorOrParsedInt)
}
