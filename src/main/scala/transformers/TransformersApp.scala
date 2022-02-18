package com.breezzo
package transformers

import cats.data.{ EitherT, OptionT }
import cats.implicits._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object TransformersApp extends App {

  type ErrorOr[A] = Either[String, A]

  final case class User(userId: Int, name: String)

  def lookupUser(userId: Int): OptionT[ErrorOr, User] = ???

  def lookupUserName(userId: Int): OptionT[ErrorOr, String] = {
    for {
      user <- lookupUser(userId)
    } yield user.name
  }

  lookupUserName(1).value
}

object TripleStack extends App {

  def monadTransformers(): Unit = {
    type FutureEither[A]       = EitherT[Future, String, A]
    type FutureEitherOption[A] = OptionT[FutureEither, A]

    val futureEitherOr: FutureEitherOption[Int] =
      for {
        a <- 10.pure[FutureEitherOption]
        b <- 32.pure[FutureEitherOption]
      } yield a + b

    println(Await.result(futureEitherOr.value.value, 1.second))
  }

  def functors(): Unit = {
    import cats.Applicative

    type ErrorOr[A]            = Either[String, A]
    type FutureEitherOption[A] = Future[ErrorOr[Option[A]]]

    implicit val f: Applicative[FutureEitherOption] =
      Applicative[Future].compose(Applicative[ErrorOr].compose(Applicative[Option]))

    val a = 10.pure[FutureEitherOption]
    val b = 32.pure[FutureEitherOption]

    val r = f.map2(a, b) { case (x, y) => x + y }

    println(Await.result(r, 1.second))
  }

  monadTransformers()
  functors()
}

object AutobotsMessaging extends App {

  type FutureEither[A] = EitherT[Future, String, A]
  type Response[A]     = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = {
    EitherT(powerLevels.get(autobot).toRight(s"$autobot is unavailable now").pure[Future])
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      pl1 <- getPowerLevel(ally1)
      pl2 <- getPowerLevel(ally2)
    } yield pl1 + pl2 > 15
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    val specialMoveReport = for {
      csm <- canSpecialMove(ally1, ally2)
    } yield if (csm) s"$ally1 and $ally2 are ready to roll out!" else s"$ally1 and $ally2 need a recharge."

    Await.result(specialMoveReport.value, 1.second).fold(x => s"Comms error: $x", y => y)
  }

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))
}
