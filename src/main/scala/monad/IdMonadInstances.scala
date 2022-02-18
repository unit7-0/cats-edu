package com.breezzo
package monad

import cats.{ Id, Monad }

object IdMonadInstances {
  implicit val idMonad: Monad[Id] = new Monad[Id] {
    override def pure[A](x: A): Id[A] = x

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = f(a) match {
      case Left(a1) => tailRecM(a1)(f)
      case Right(b) => b
    }
  }

  implicit val loggingIdMonad: Monad[Id] = new Monad[Id] {
    override def pure[A](x: A): Id[A] = {
      println(s"logging: pure called with ${x}")
      x
    }

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = {
      println(s"logging: flatMap called with $fa")
      f(fa)
    }

    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = {
      println(s"logging: tailRecM called with $a")
      f(a) match {
        case Left(a1) => tailRecM(a1)(f)
        case Right(b) => b
      }
    }
  }
}

object IdMonadApp extends App {
  import cats.syntax.functor._
  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import IdMonadInstances.loggingIdMonad

  val a = IdMonadInstances.idMonad.pure(10)
  val b = IdMonadInstances.idMonad.pure(20)

  val r = for {
    a1 <- a
    b1 <- b
  } yield a1 + b1

  println(r)

  print(1010.pure)
  print(Option(777))
  print(List("12", "123"))

  def print[A, F[_]: Monad](a: F[A]): Unit = {
    a.map { v =>
      println(s"valueOf: $v")
    }
  }
}
