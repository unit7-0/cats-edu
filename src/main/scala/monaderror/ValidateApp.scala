package com.breezzo
package monaderror

import cats.MonadError
import cats.implicits.catsSyntaxApplicativeId

import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContextExecutor, ExecutionContextExecutorService, Future}
import scala.util.Try

object ValidateApp extends App {

  def validateAdult[F[_]](age: Int)(implicit monadError: MonadError[F, Throwable]): F[Int] =
    validateAdult(age.pure[F])

  def validateAdult[F[_]](age: F[Int])(implicit monadError: MonadError[F, Throwable]): F[Int] = {
    monadError.ensure(age)(
      new IllegalArgumentException(s"Age must be greater than or equal to 18. Value specified: $age")
    )(_ >= 18)
  }

  println(validateAdult[Try](10))
  println(validateAdult[Try](20))

  type ExceptionOr[A] = Either[Throwable, A]

  println(validateAdult[ExceptionOr](50))
  println(validateAdult[ExceptionOr](5))

  implicit val ec: ExecutionContextExecutorService = scala.concurrent.ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

  println(
    Await.result(
      validateAdult[Future](Future {
        Thread.sleep(5000)
        5
      }),
      6.seconds
    )
  )

  ec.shutdownNow()
}
