package com.breezzo
package writer

import cats.data.Writer
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxWriterId}

import scala.concurrent.Future
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._

object WriterFactorialApp extends App {
  type Logged[A] = Writer[Vector[String], A]

  def factorial1(n: Int): Logged[Int] =
    (if (n == 0) {
      1.pure[Logged]
    } else {
      slowly(factorial1(n - 1).map(_ * n))
    })
      .flatMap(ans =>
        Vector(s"fact $n $ans").tell
          .map(_ => ans)
      )

  type Fact = Writer[Vector[String], Int]

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Fact = {
    def logMsg(v: Int): String = s"fact $n = $v"
    def calc(): Fact = {
      if (n == 0) {
        1.writer(Vector(logMsg(1)))
      } else {
        factorial(n - 1).mapBoth { case (log, f) =>
          val ans = n * f
          (log :+ logMsg(ans), ans)
        }
      }
    }
    slowly(calc())
  }

  val f15 = Future(factorial(15).written.mkString("\n"))
  val f5 = Future(factorial(5).written.mkString("\n"))

  println(Await.result(f15, 3.seconds))
  println(Await.result(f5, 3.seconds))
}
